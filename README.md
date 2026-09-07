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
  confirmed byte range.
- **Auditable sessions**: Every modification is recorded as a trackable session on
  `scalpel/autosave`, with one-shot rollback -- no hunting through scattered diffs.
- **Surgical precision, not bulk replace**: Multi-file refactoring is supported,
  but each step can be confirmed before it is applied; Scalpel never silently edits
  the whole repo.

The user story is simple: **you talk to a coding assistant from inside Emacs,
but every actual edit is a controlled act.**

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
   structured actions, not raw diffs, so users can review and abort.

5. **Every change is an auditable transaction**  
   Each accepted modification is recorded on an orphan Git branch
   (`scalpel/autosave`). A session can be reverted as a unit.

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
          │    (nothing else changes)    │  │    session audit trail       │
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
- `scalpel-locate` – deterministic locator implementation
- `scalpel-execute` – boundary-locked edit application
- `scalpel-lineage` – session tracking and Git-backed rollback

---

## Interaction Model

Scalpel is a **conversational editor**, not a one-shot command.

1. Open any code files you need as context.
2. Run `M-x scalpel-console` to open the agent console.
3. Type an instruction in natural language, e.g.:

   ```
   change parse-config to return nil if the config file is missing
   ```

4. Scalpel inspects the open buffers, asks the LLM for a structured plan, and
   presents something like:

   ```
   [1/2] edit  config/parser.el  symbol=parse-config
   [2/2] edit  server/start.el    symbol=init-server
   ```

5. Confirm (or edit) the plan. Scalpel then resolves each symbol with the
   locator, generates replacement text, and applies it **only to the resolved
   range**. Each edit is reported in the console.

6. If a change looks wrong, abort it with `C-g` or revert the whole session
   with `M-x scalpel-revert-session`.

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
- Git (used for local undo/rollback)
- optionally `lsp-mode`/`eglot` and language servers for richer semantics

### Install

```elisp
;; until this is on MELPA:
(add-to-list 'load-path "~/path/to/scalpel")
(require 'scalpel)
```

---

## Quickstart

```elisp
;; Open a project and an Emacs Lisp / Rust / TS file.
M-x scalpel-console
;; In the console:
;;   "in parser.el, make parse-config return nil on missing file"
;;   "add a with-timeout helper next to the current function"
;;   "rename load-config to load-settings and update its callers"
```

Inside the console:

- `C-c C-c` – send the current input line to Scalpel
- `C-c C-x` – interrupt a running agent operation
- `M-x scalpel-history` – show previous Scalpel sessions
- `M-x scalpel-revert-session` – revert one session completely

---

## Privacy & Safety

- All LLM traffic goes through `gptel`; Scalpel never connects directly to an
  LLM or sends data outside your configured backend.
- For local models (Ollama, llama.cpp), no source leaves your machine.
- Every accepted edit is committed to the orphan branch `scalpel/autosave`; the
  current Git working tree stays clean until you decide to commit.

---

## Status & Roadmap

Scalpel is in active, deliberately small MVP stages.

**Currently implemented**
- Emacs Lisp structural location
- Console UI skeleton

**Near-term**
- Deterministic locator API with LSP integration
- Structured action planner
- Execution boundary lock and `scalpel/autosave`

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
