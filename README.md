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
   expected to catch afterwards. Exactness is the product, not a bonus.
2. **So application does not ask permission.** The locator resolves each target
   to a verified byte range, which makes applying a change a mechanical act.
   Stopping to confirm every hunk -- the diff-by-diff approval loop -- would tax
   every correct edit to guard against a failure the boundary lock already
   prevents. Scalpel applies edits as it plans them.
3. **So the result must be visible.** A change made without asking must not
   vanish quietly into the tree; it has to be readable afterwards. Each
   modification is reported as it lands, and the session can be diffed as a
   whole, so the agent's work is legible after the fact instead of gated before
   it.
4. **So rollback must exist.** Visibility tells you what went wrong; it does not
   take it back. The same record that shows the change is what undoes it:
   session recording and one-shot rollback are load-bearing, not polish, and
   they are what buy the speed in (2).

Read in reverse, the chain is the whole safety argument. Because a session can
be taken back, its changes can be reported rather than approved; because the
report is enough to decide, application can run at full speed; and because every
edit lands on a resolved byte range, all of it can rest on exactness.

Shell commands always ask for confirmation before running, since their effects
are not constrained by the boundary lock. The user agrees to those effects
before they occur.

For how this differs from Aider and newer coding agents, see Core Advantages.

## Core Advantages

Scalpel gives users an AI editing workflow that is precise, safe, and native to Emacs.

- Deterministic targeting: edits lock to explicit locations and boundaries, reducing unintended changes.
- Visible, recoverable sessions: every modification is visible as it happens and can be undone or restored.
- Emacs-native workflow: console interaction, context selection, and history remain inside the editor.
- Zero-config request optimization: Scalpel automatically detects the active backend and model, suppresses redundant reasoning where possible to reduce latency and token cost, and provides one toggle to restore default request behavior.
- Privacy and safety: redaction, bounded context, and sandboxed shell execution limit data exposure and side effects.
- Extensible locator provider: locator and backend support can be extended without changing the core editing model.

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
   roll back a change you cannot see. The same record is what rolls back --
   modifications are committed to an orphan Git branch (`scalpel/autosave`), and
   a session reverts as a unit.

6. **Application is unconfirmed, recovery is not**  
   Because the boundary lock makes a replacement exact, applying it does not
   stop for approval: the diff-by-diff confirmation loop would tax every correct
   edit to guard against a failure that resolution already prevents. The safety
   net sits on the other side instead -- what principle 5 makes visible is what
   makes this acceptable. Shell-command confirmation is governed by its own
   policy, not by execution duration.

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
                         ╭──────────────────────────────────────╮
                         │           scalpel-agent              │
                         │  plans → structured action list      │
                         ╰──────────────┬───────────────────────╯
                                        │ 3. LLM requests
                                        ▼
   ╭───────────────╮     ╭──────────────────────────────────────╮
   │     gptel     │◀────│           scalpel-llm                │
   │  (LLM calls)  │     │  minimal-request probe, reasoning    │
   ╰───────────────╯     │  suppressed; one toggle to restore   │
                         ╰──────────────┬───────────────────────╯
                                        │ 4. actions, e.g.
                                        │  [[action]] TOML tables
                                        ▼
   ╭───────────────╮     ╭──────────────────────────────────────╮
   │  structural   │◀────│          scalpel-locate              │
   │  providers    │     │  resolve action to exact byte range  │
   │  (elisp, md,  │     ╰──────────────┬───────────────────────╯
   │  yaml, git-   │                    │ 5. (beg . end)
   │  ignore, org, │                    ▼
   │  and more)    │     ╭──────────────────────────────────────╮
   ╰───────────────╯     │          scalpel-execute             │
                         │     boundary-locked replacement      │
                         ╰───────┬──────────────────┬───────────╯
                                 │ 6. replace       │ 7. record
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

## Interaction Model

Scalpel is a **conversational editor**, not a one-shot command.

1. Open any code files you need as context.
2. Run `M-x scalpel-open` to open the agent console.
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
   is the trace that replaces it. Shell actions do not ask for confirmation based
   on execution duration. Exactness comes from step 4 being byte-range pinned,
   not from a review loop.

6. If a change is wrong, the session is there to inspect and undo: review the
   recorded edits as a diff — rejecting individual hunks from the review buffer
   if only some of them are wrong — then `M-x scalpel-revert-session` takes the
   session back as one unit. When shell output is truncated, it can be continued
   from where it stopped (continue-after-shell). `C-c C-k` stops a request in
   flight. This is the reason step 5 can move without asking.

Anonymous targets such as "the second if branch" are handled by first moving the
Emacs point to that block; point is the strongest coordinate Scalpel knows.

Each console session can run its own gptel backend and model. Switch with
`C-c C-b` in the console; the choice is snapshotted into the session, so module
reloads keep it.

Multiple consoles on the same project root work concurrently without git
worktrees. Per-request staleness baselines refuse writes to files another
console just touched, and every change stays reviewable and revertible per
session.

## Deterministic Location By Language

| Language | Primary Locator | Fallback | Supported Granularity |
| --- | --- | --- | --- |
| Emacs Lisp | built-in `syntax-ppss` + `beginning-of-defun` | none | function/macro/defvar blocks |
| Markdown | built-in structural scan (ATX headings) | none | heading sections |
| YAML | built-in structural scan (top-level keys) | none | top-level key blocks |
| .gitignore | built-in structural scan (pattern lines) | none | pattern lines |
| Org | built-in structural scan (Org headlines) | none | headline subtrees |
| HTML | built-in structural scan (HTML tag id) | none | element blocks keyed by id attribute |
| Rust | rust-analyzer (LSP) | tree-sitter | functions, methods, impl items |
| TypeScript / JS | tsserver (LSP) | tree-sitter | function/class/module blocks |
| Go | gopls (LSP) | tree-sitter | funcs, types |
| Python | pyright / pylsp (LSP) | tree-sitter | def/class blocks |
| C / C++ | clangd (LSP) | tree-sitter + ripgrep | functions, methods |
| JSON / YAML / TOML | tree-sitter | structural scan | top-level keys |

When no LSP and no tree-sitter grammar exist, Scalpel falls back to bracket
depth scanning (`syntax-ppss`) and supports only **cursor-driven block edits** –
never speculative whole-file changes.

## Installation

### Prerequisites

- Emacs 29.1+
- a working `gptel` setup (any backend: OpenAI, Anthropic, Ollama, ...)
- `toml` package: the plan output contract is TOML (single-quoted literal strings); parsing requires the `toml` package and is checked at load time
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
  (setq my-glm-low-backend
        (gptel-make-openai "OpenRouter-GLM-Low"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (auth-source-pick-first-password :host "openrouter.ai")
          :models '("z-ai/glm-5.3-flash")
          :request-params '(:reasoning (:effort "low"))))
  (setq gptel-backend my-glm-low-backend))
```

Then install Scalpel itself:

```elisp
(use-package scalpel
  :load-path "~/path/to/scalpel"   ; replace with the actual path
  :after (gptel)
  :commands (scalpel-open))
```

After evaluating the above, run `M-x scalpel-open`.

`gptel` is a hard dependency, not an optional integration: loading Scalpel
without it fails with a `file-missing' error, so install and configure `gptel`
first, as above.  A missing API key is a separate, non-fatal case: the first
agent request then fails with a clear message pointing at
`M-x scalpel-set-backend`.

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

## Web search configuration

The web-search tool is controlled by three defcustom options: the
default engine, a per-engine proxy table, and the engine registry.

`scalpel-agent-web-search-default-engine` names the engine used when an
action omits the engine field. The only remaining built-in engine is
`zhipu`, which talks to a structured API and is therefore unaffected by
anti-scraping on the engine side; it is also the default.

`scalpel-agent-web-search-proxy-table` is an alist mapping an engine
name to a proxy address of the form `host:port`. Engines without an
entry connect directly. Whether a proxy is needed is a property of the
user's network environment rather than of the engine, which is why
proxies live in a separate table instead of the engine definitions.

`scalpel-agent-web-search-engine-registry` is the declarative registry
where each engine is a single entry describing its endpoint, HTTP
method, request construction and response parsing. Adding or replacing
an engine is a pure configuration change that appends one entry, so
extend the table after load rather than shadowing it wholesale. Engines
whose registry entry carries an `auth-host` field read a Bearer token
from `auth-source` and fail loudly when the key is missing.

```elisp
(use-package scalpel-agent
  :custom
  (scalpel-agent-web-search-default-engine "zhipu")
  (scalpel-agent-web-search-proxy-table
   '(("zhipu" . "127.0.0.1:7897"))))
```

## Quickstart

```elisp
;; Open a project and an Emacs Lisp / Rust / TS file.
M-x scalpel-open
;; In the console:
;;   "in parser.el, make parse-config return nil when the file is missing"
;;   "add a with-timeout helper next to the current function"
;;   "rename load-config to load-settings and update its callers"
```

Inside the console:

- `RET` – sends everything typed since the last reply
- `S-RET` – inserts a newline, so an instruction may span several lines
- `C-c C-c` – also sends the pending instruction
- `C-c C-k` – cancels the in-flight request
- `C-c C-b` – switches the active gptel backend (model) for future requests
- `C-c C-a` – adds a file or directory to the context (writable)
- `C-c C-A` – adds a file or directory to the context as read-only: the agent can read it, but no mutation tool may write to it; re-adding the same path with `C-c C-a` lifts the restriction
- `C-c C-d` – removes a file or directory from the context
- `C-c C-r` – clears the context
- `C-c C-f` – forgets the conversation; keeps the context files
- `C-c C-e` – repeats the previous instruction verbatim (after a planner-output failure)
- `C-c C-o` – toggles verbatim output of consumed bodies
- `M-x scalpel-token-open` – shows per-round token accounting (round, per-console and grand totals)
- `M-x scalpel-token-reset` – clears the token accounting
- `M-x scalpel-llm-probe-minimize-reasoning-toggle` – minimizes reasoning in the LLM probe with one toggle
- `M-x scalpel-history` – shows previous Scalpel sessions
- `M-x scalpel-revert-session` – reverts one session completely

## Privacy & Safety

- All LLM traffic goes through `gptel`; Scalpel never connects directly to an
  LLM or sends data outside your configured backend.
- Outgoing prompts pass through `scalpel-redact` first: configured rules --
  home-directory paths, and any pattern you add -- are replaced before the text
  reaches the LLM, so identifiers that would point back to your machine never
  leave it.
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
- Reading code back does not go through the sandbox: the `file-peek` action serves
  one definition, or a whole file, straight from the context list, and refuses
  any path outside it.  A report's body reaches the planner once -- on the
  round that follows the command or read that produced it -- and later prompts
  carry only the report's header, so a long session does not re-send output
  the planner has already read.  The console buffer keeps every byte;
  `scalpel-console-trim-consumed-output` turns the trimming off.
- The context is bounded by `scalpel-agent-context-max-files`, and its
  collection honors `.scalpelignore` and the project's Git ignore rules, so
  ignored files never enter the prompt or the sandbox's reach.  An add that
  would cross the cap is refused whole, leaving the context exactly as it
  was, so neither the sandbox's reach nor the planner prompt grows past it
  by accident.
- For local models (Ollama, llama.cpp), no source leaves your machine.
- Every change is reviewable before it is recoverable: each modification is
  reported in the console as it lands and stays in the buffer, and a session can
  be diffed as a whole.  Recovery then runs through the orphan branch
  `scalpel/autosave`: the current Git working tree stays clean until you decide
  to commit.  This ships with `scalpel-lineage`; see Status & Roadmap.

## Status & Roadmap

Current block:

## Contributing

We keep the same hygiene rules across every commit:

- Location stays deterministic; the LLM must never output positions.
- New languages arrive as official locator plugins, not user-side hacks.
- Every edit obeys the boundary lock and is visible in the console.
- Tests mock every external interaction, including LLM calls.

We gratefully acknowledge the two third-party packages Scalpel depends on: **gptel** and **toml**. Both are hard dependencies declared in `Package-Requires`, and this project would not function without them. Thanks to their maintainers and contributors.

---

## License

Apache License 2.0. See [LICENSE](LICENSE).

---

## Etymology

A scalpel is a small, sharp blade used for precise cutting. Unlike broad
strokes, it touches only what the surgeon intends. This is exactly what the
tool aims to be for code changes – especially when an AI is doing the cutting.
