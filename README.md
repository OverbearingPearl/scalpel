# Scalpel

**A deterministic, Emacs-native coding agent for surgical code modification.**

Scalpel is designed as a modern Emacs replacement for Aider-style terminal AI
assistants. It keeps the conversational workflow people expect from coding
agents, but replaces fuzzy text matching with deterministic, tool-driven
location. The LLM decides **what** to change; Scalpel decides **where** to
change.

> **Core principle**  
> The LLM never locates code. It plans, explains, and generates new text.
> Location is always resolved by deterministic tooling – LSP, tree-sitter, or
> structural scanning – down to exact byte ranges.

---

## Why Scalpel?

Aider introduced the idea of an LLM that works directly with your repo, but its
maintenance has stalled, and its `SEARCH/REPLACE` approach remains fragile.
Newer agents (Claude Code, OpenCode) feel powerful but are hard to constrain –
they often edit code you did not ask to touch.

Scalpel exists at the intersection: a full coding agent, **inside Emacs**, with
a strict boundary lock that makes unwanted edits almost impossible.

### The design trade

Scalpel is built around one chain of reasoning:

1. **An edit must be exact.** A change that lands on the wrong code, or that
   spills outside the intended block, is a defect -- not something a reviewer is
   expected to catch afterwards.  Exactness is the product, not a bonus.
2. **So application does not ask permission.** The locator resolves each target
   to a verified byte range, which makes applying a change a mechanical act.
   Stopping to confirm every hunk -- the diff-by-diff approval loop -- would tax
   every correct edit to guard against a failure the boundary lock already
   prevents.  Scalpel applies edits as it plans them.
3. **So the result must be visible.** A change made without asking must not
   vanish quietly into the tree; it has to be readable afterwards.  Each
   modification is reported as it lands, and the session can be diffed as a
   whole, so the agent's work is legible after the fact instead of gated before
   it.
4. **So rollback must exist.** Visibility tells you what went wrong; it does not
   take it back.  The same record that shows the change is what undoes it:
   session recording and one-shot rollback are load-bearing, not polish, and
   they are what buy the speed in (2).

Read in reverse, the chain is the whole safety argument.  Because a session can
be taken back, its changes can be reported rather than approved; because the
report is enough to decide, application can run at full speed; and because every
edit lands on a resolved byte range, all of it can rest on exactness.

The single exception is a shell command the planner flagged `long-running`.  It
asks first because it freezes Emacs until it returns: a cost the user has to
agree to before it is paid, not after.

**Key differences from Aider and newer coding agents (Claude Code, OpenCode, etc.)**

- **Interaction and ecosystem**: Scalpel is not a standalone CLI; it lives inside
  Emacs as a buffer. It reuses gptel, LSP, and magit, so there is no need to switch
  to another window outside Emacs.
- **Location without text guessing**: Aider asks the LLM to produce SEARCH/REPLACE
  blocks for matching; Scalpel instead resolves symbols directly to exact byte
  ranges using LSP/tree-sitter/structural scanning, and asks the LLM only for
  *what* to change.
- **No scope creep**: Newer agents often change files you did not ask to touch.
  Scalpel's boundary lock makes that impossible -- replacements land only on the
  byte range the locator resolved.
- **Visible, then reversible**: Every modification is reported as it lands, and
  the whole session can be diffed afterwards and reverted in one shot -- no
  hunting through scattered diffs.  These are the visibility and recovery halves
  of the design trade above, and they land with `scalpel-lineage`; see Status &
  Roadmap.
- **Surgical precision, not bulk replace**: Multi-file refactoring is supported,
  but every replacement lands on its own resolved range, so a wide change is
  still a sum of exact ones.

The user story is simple: **you talk to a coding assistant from inside Emacs,
every actual edit lands on an exact, verified range, and whatever a session
touched can be read back and unwound.**

---

## Design Principles

1. **Planning and positioning are decoupled**  
   The LLM may suggest steps, propose symbols, and describe changes. It never
   emits byte offsets, line numbers, or diff anchors for matching.

2. **The locator is deterministic**  
   Scalpel resolves every target through LSP / Tree-sitter / structural scans.
   Results are bytes in a file, not guesses.

3. **The boundary lock is absolute**  
   An edit is applied only to the resolved region. Nothing adjacent is changed,
   no matter how confident the LLM seems.

4. **The console is the agent**  
   User interactions happen in a dedicated `*scalpel*` buffer. The LLM returns
   structured actions, not raw diffs, so the buffer is a readable log of what was
   done and why.

5. **Every change is visible, then reversible**  
   Each accepted modification is reported as it lands, and the session can be
   diffed as a whole, so what the agent did is legible before it is undoable.
   Visibility comes first because recovery depends on it: you cannot decide to
   roll back a change you cannot see.  The same record is what rolls back --
   modifications are committed to an orphan Git branch (`scalpel/autosave`), and
   a session reverts as a unit.

6. **Application is unconfirmed, recovery is not**  
   Because the boundary lock makes a replacement exact, applying it does not
   stop for approval: the diff-by-diff confirmation loop would tax every correct
   edit to guard against a failure that resolution already prevents.  The safety
   net sits on the other side instead -- what principle 5 makes visible is what
   makes this acceptable.  A shell command flagged `long-running` is the
   exception: it asks first, because it freezes Emacs until it returns.

---

## System Architecture

<pre>
                         ╭──────────────────────────────────────╮
                         │               User                   │
                         │   cursor on target; speaks intent    │
                         ╰──────────────┬───────────────────────╯
                                        │ 1. instruction
                                        ▼
                         ╭──────────────────────────────────────╮
                         │           *scalpel* console          │
                         │       persistent agent session       │
                         ╰──────────────┬───────────────────────╯
                                        │ 2. request
                                        ▼
   ╭───────────────╮    ╭──────────────────────────────────────╮
   │     gptel     │◀───│           scalpel-agent              │
   │  (LLM calls)  │    │   LLM: plan → structured actions     │
   ╰───────────────╯    ╰───────────────┬──────────────────────╯
                                        │ 3. action list, e.g.
                                        │  {edit, file, symbol}
                                        ▼
   ╭───────────────╮    ╭──────────────────────────────────────╮
   │     LSP /     │◀───│          scalpel-locate              │
   │ tree-sitter / │    │  resolve action to exact byte range  │
   │ syntax-ppss   │    ╰───────────────┬──────────────────────╯
   ╰───────────────╯                    │ 4. (beg . end)
                                        ▼
                         ╭──────────────────────────────────────╮
                         │          scalpel-execute             │
                         │     boundary-locked replacement      │
                         ╰───────┬──────────────────┬───────────╯
                                 │ 5. replace       │ 6. record
                                 ▼                  ▼
          ╭──────────────────────────────╮  ╭──────────────────────────────╮
          │    edited Emacs buffer       │  │       scalpel-lineage        │
          │    (nothing else changes)    │  │  session diff and rollback   │
          ╰──────────────────────────────╯  ╰──────────────┬───────────────╯
                                                           │ git commit
                                                           ▼
                                            ╭──────────────────────────────╮
                                            │      scalpel/autosave        │
                                            ╰──────────────────────────────╯
</pre>

Components live in focused modules:

- `scalpel-console` – persistent agent UI loop
- `scalpel-agent` – LLM planning integration and structured action parser
- `scalpel-locate` – locator dispatch; resolves actions to byte ranges via per-language providers
- `scalpel-locate-elisp` – built-in structural locator provider for Emacs Lisp
- `scalpel-execute` – boundary-locked edit application and deletion,
  written to disk as applied
- `scalpel-sandbox` – command sandbox for shell actions (`bubblewrap` on Linux, `sandbox-exec` on macOS)
- `scalpel-lineage` – session diff, tracking, and Git-backed rollback

Every additional language arrives as a `scalpel-locate-<lang>.el` provider
module, registered through `scalpel-locate-register-provider`. Planned
providers cover the languages in the table below: Rust, TypeScript /
JavaScript, Go, Python, C / C++, and data formats (JSON / YAML / TOML).

---

## Interaction Model

Scalpel is a **conversational editor**, not a one-shot command.

1. Open any code files you need as context.
2. Run `M-x scalpel-console` to open the agent console.
3. Type an instruction in natural language, e.g.:

   ```
   change parse-config to return nil if the config file is missing
   ```

4. Scalpel asks the LLM for a structured plan, resolves each target through the
   locator, and applies the change **only to the resolved range**, reporting each
   edit in the console:

   ```
   Edited parse-config in parser.el
   Edited init-server in start.el
   ```

5. Nothing stops for approval along the way, because the stream of edit reports
   is the trace that replaces it: you read what happened rather than authorize it
   beforehand. Exactness comes from step 4 being byte-range pinned, not from a
   review loop, and a shell command flagged `long-running` is the only action
   that asks first, because it freezes Emacs until it returns.

6. If a change is wrong, the session is there to inspect and undo: review the
   recorded edits as a diff, then `M-x scalpel-revert-session` takes the session
   back as one unit. `C-g` stops a request in flight. This is the reason step 5
   can move without asking.

Anonymous targets such as "the second if branch" are handled by first moving
the Emacs point to that block; point is the strongest coordinate Scalpel knows.

---

## Deterministic Location By Language

| Language | Primary Locator | Fallback | Supported Granularity |
| --- | --- | --- | --- |
| Emacs Lisp | built-in `syntax-ppss` + `beginning-of-defun` | none | function/macro/defvar blocks |
| Rust | rust-analyzer (LSP) | tree-sitter | functions, methods, impl items |
| TypeScript / JS | tsserver (LSP) | tree-sitter | function/class/module blocks |
| Go | gopls (LSP) | tree-sitter | funcs, types |
| Python | pyright / pylsp (LSP) | tree-sitter | def/class blocks |
| C / C++ | clangd (LSP) | tree-sitter + ripgrep | functions, methods |
| JSON / YAML / TOML | tree-sitter | structural scan | top-level keys |

When no LSP and no tree-sitter grammar exist, Scalpel falls back to bracket
depth scanning (`syntax-ppss`) and supports only **cursor-driven block edits** –
never speculative whole-file changes.

---

## Installation

### Prerequisites

- Emacs 29.1+
- a working `gptel` setup (any backend: OpenAI, Anthropic, Ollama, ...)
- Git (used for local undo/rollback and to expand context directories through gitignore rules)
- optionally `lsp-mode`/`eglot` and language servers for richer semantics

Shell actions need a command sandbox on top of the above. Without a working
sandbox, planning and edits keep working and only shell actions are refused:

- Linux: `bubblewrap` (`bwrap`)
- macOS: `sandbox-exec`, deprecated by Apple; the macOS backend is experimental
- other systems: no sandbox backend, so shell actions are refused

### Install with use-package

Since Scalpel is not yet on MELPA, point use-package at its source directory.

First, install and configure `gptel`. A minimal setup using `use-package` is:

```elisp
(use-package gptel
  :ensure t
  :config
  ;; Example: DeepSeek via an OpenAI-compatible backend.
  ;;
  ;; Store the API key in `~/.authinfo`:
  ;;   machine api.deepseek.com login api-key password YOUR_DEEPSEEK_API_KEY
  (setq gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pick-first-password :host "api.deepseek.com")
          :models '("deepseek-v4-flash")))
  )
```

Then install Scalpel itself:

```elisp
(use-package scalpel
  :load-path "~/path/to/scalpel"   ; replace with the actual path
  :after (gptel)
  :commands (scalpel-open)
```

After evaluating the above, run `M-x scalpel-open`.

If `gptel` is missing, Scalpel still loads, so the test suite can run without
it; the first agent request then fails with a clear message explaining how to
install it, instead of an opaque "Cannot open load file" error.

If you prefer not to use `use-package`, you can manually add both packages to
`load-path` and require them in order:

```elisp
(add-to-list 'load-path "/path/to/gptel")
(add-to-list 'load-path "/path/to/scalpel")
(require 'gptel)
;; configure gptel here
(require 'scalpel)
```

---

## Quickstart

```elisp
;; Open a project and an Emacs Lisp / Rust / TS file.
M-x scalpel-open
;; In the console:
;;   "in parser.el, make parse-config return nil on missing file"
;;   "add a with-timeout helper next to the current function"
;;   "rename load-config to load-settings and update its callers"
```

Inside the console:

- `RET` – send everything typed since the last reply
- `S-RET` – insert a newline, so an instruction may span several lines
- `C-c C-c` – also send the pending instruction
- `C-g` – cancel a running request
- `C-c C-b` – switch the active gptel backend (model) for future requests
- `C-c C-a` – add a file or directory as writable context
- `C-c C-d` – remove a file or directory from the context
- `C-c C-r` – reset the context to currently open located files
- `C-c C-f` – forget the conversation; keeps the context files
- `M-x scalpel-history` – show previous Scalpel sessions
- `M-x scalpel-revert-session` – revert one session completely

---

## Privacy & Safety

- All LLM traffic goes through `gptel`; Scalpel never connects directly to an
  LLM or sends data outside your configured backend.
- Shell actions run inside an OS sandbox whose file scope is exactly the
  current context files, rebuilt for every action.  On Linux that is
  `bubblewrap`; on macOS it is the deprecated `sandbox-exec`, treated as
  experimental.  A context file is exposed at its own absolute path, so
  commands read the same names the user and the planner already use,
  and a file outside the context cannot be opened even when it sits beside one
  in the same directory.  Context files are bound read-only, so no shell
  command can change one: every file change goes through the edit
  primitives.  The planner is never told that a sandbox exists: it
  sees an ordinary shell whose filesystem holds only the context files.
  Commands also get the temporary directory for scratch files.  When the
  sandbox is unavailable, or its probe fails, shell actions fail closed
  instead of falling back to an unsandboxed shell.
- For local models (Ollama, llama.cpp), no source leaves your machine.
- Every change is reviewable before it is recoverable: each modification is
  reported in the console as it lands and stays in the buffer, and a session can
  be diffed as a whole.  Recovery then runs through the orphan branch
  `scalpel/autosave`: the current Git working tree stays clean until you decide
  to commit.  This ships with `scalpel-lineage`; see Status & Roadmap.

---

## Status & Roadmap

Scalpel is in active, deliberately small MVP stages.

**Currently implemented**
- Emacs Lisp structural location, plus the provider dispatch API
  (`scalpel-locate` and `scalpel-locate-elisp`)
- The console and the multi-round agent loop: context management, shell-output
  continuation, and conversation replay
- Structured action planning and parsing (`edit`, `create`, `delete`, `shell`,
  `reply`, `confirm`), each action returning a human-readable report that stays
  in the console buffer, so a session's changes are readable after the fact
- Execution boundary lock: a replacement lands only on the range the locator
  resolved, and an unbalanced replacement is refused; an applied change reaches
  its file as it lands, so the shell commands the next round runs already see
  it, and a deletion collapses the blank lines it brings together
- Shell execution through `bubblewrap` on Linux, plus an experimental macOS
  backend through `sandbox-exec`; shell actions are refused when the sandbox is
  unavailable or fails its probe

**Near-term**
- Session diff and one-shot rollback: review everything a session changed as a
  single diff, then revert it as one unit -- `scalpel-lineage`, the
  `scalpel/autosave` orphan branch, `M-x scalpel-history`, and
  `M-x scalpel-revert-session`
- LSP-backed locator providers, registered through the existing dispatch API

**Later**
- Rust / TS / Go LSP-first workflows
- Multi-file refactor plans with per-session rollback
- `tree-sitter` fallbacks for dynamic languages

Contributions are welcome, especially around locator providers and the agent
loop.

---

## Contributing

We keep the same hygiene rules across every commit:

- Location stays deterministic; the LLM must never output positions.
- New languages arrive as official locator plugins, not user-side hacks.
- Every edit obeys the boundary lock and is visible in the console.
- Tests mock every external interaction, including LLM calls.

---

## License

Apache License 2.0. See [LICENSE](LICENSE).

---

## Etymology

A scalpel is a small, sharp blade used for precise cutting. Unlike broad
strokes, it touches only what the surgeon intends. This is exactly what the
tool aims to be for code changes – especially when an AI is doing the cutting.
