# Scalpel

**Surgical-precision code editing for the AI era – built for Emacs.**

Scalpel is an Emacs-native code-modification tool built around a single principle:  
**"The LLM shall never locate code – it only generates new content."**

Unlike other AI assistants that rely on fuzzy `SEARCH/REPLACE` blocks or over-eager agents, Scalpel uses **deterministic tooling** (LSP, Tree-sitter, Ripgrep) to resolve every symbol to an exact byte offset. The executor then replaces *only* that range and enforces a strict boundary lock – no adjacent code is ever changed.

- Pin-point accuracy – no more failures due to extra spaces or line breaks.
- No over-editing – the agent cannot "improve" unrelated functions.
- Zero plugin hassle – language support is built in and version-locked.
- Full audit trail – every change is committed to a Git orphan branch (`scalpel/autosave`) and can be reverted per session.
- Emacs-native – deeply integrated with `lsp-mode` / `eglot`, `magit`, and `ediff`.

---

## Design Philosophy

| Traditional Tools (Aider, OpenCode) | Scalpel |
| :--- | :--- |
| LLM writes `SEARCH` blocks – fails on formatting differences | **Location is resolved by LSP / Tree-sitter – 100% deterministic** |
| Agents often modify code you didn't ask for | **Boundary lock** guarantees *only* the target block is replaced |
| Users must configure plugins or write adapters | **Zero-config** – Scalpel knows your language out of the box |
| Coupled "plan + act" – high error rate | **Decoupled generation and positioning** – each part does what it does best |
| Built as a CLI – works everywhere, but feels like an external tool | **Built for Emacs** – feels like a native part of your editor |

---

## System Architecture

<pre>
╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
║                          SCALPEL — SYSTEM ARCHITECTURE (Emacs-Native)                                   ║
╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝

┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  LAYER 1: USER INTERACTION (SELF)                                                                       │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐   │
│  │  Emacs Minibuffer / Keybinding (`M-x scalpel-edit`)                                            │   │
│  │  User types natural language instruction → Scalpel captures it                                │   │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘
                                          │
                                          ▼
┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  LAYER 2: CORE ORCHESTRATION ENGINE (SELF) — MAIN PIPELINE                                             │
│                                                                                                         │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐   │
│  │  1. SCHEDULER                                                                                  │   │
│  │  Intent parser (LLM function calling)  |  Language router (based on buffer's major mode)       │   │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────┘   │
│                                          │                                                               │
│                                          ▼                                                               │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐   │
│  │  2. LOCATOR (LSP Client)                                                                       │   │
│  │  Calls `lsp-mode` / `eglot` to get symbol range (byte offset)  |  Cache layer                   │   │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────┘   │
│                                          │                                                               │
│                                          ▼                                                               │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐   │
│  │  3. ATOMIC EXECUTOR                                                                             │   │
│  │  Byte-range precise replacement  |  Boundary lock (NO changes outside target block)            │   │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────┘   │
│                                          │                                                               │
│                                          ▼                                                               │
│  ┌─────────────────────────────────────────────────────────────────────────────────────────────────┐   │
│  │  4. LINEAGE TRACKER                                                                             │   │
│  │  Session ID per request  |  Git orphan branch (scalpel/autosave)  |  Revert by session         │   │
│  └─────────────────────────────────────────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘
                                          │
                                          ▼  (invoke external dependencies)
┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  LAYER 3: EXTERNAL DEPENDENCIES (EXT) — PROVIDED BY EMACS ECOSYSTEM                                    │
│                                                                                                         │
│  ┌─────────────────────────┐  ┌─────────────────────────┐  ┌─────────────────────────┐  ┌─────────────┐ │
│  │  LSP CLIENTS            │  │  VERSION CONTROL        │  │  LLM GATEWAY            │  │  RENDERING  │ │
│  │  (provided by Emacs)    │  │  (provided by Emacs)    │  │  (pluggable)            │  │  (provided  │ │
│  ├─────────────────────────┤  ├─────────────────────────┤  ├─────────────────────────┤  │  by Emacs)  │ │
│  │ * lsp-mode              │  │ * magit (Git porcelain) │  │ * gptel (OpenAI API)    │  ├─────────────┤ │
│  │ * eglot                 │  │ * Git executable        │  │ * gptel (Anthropic API) │  │ * ediff     │ │
│  │ * LSP Servers:          │  │   (system)              │  │ * gptel (Ollama)        │  │ * diff-mode │ │
│  │   rust-analyzer         │  │                         │  │                         │  │             │ │
│  │   gopls                 │  │                         │  │                         │  │             │ │
│  │   tsserver              │  │                         │  │                         │  │             │ │
│  │   clangd                │  │                         │  │                         │  │             │ │
│  └─────────────────────────┘  └─────────────────────────┘  └─────────────────────────┘  └─────────────┘ │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘
                                          ▲
                                          │  (provides context, overrides policies)
                                          │
┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  LAYER 4: PROJECT PROBE & CONFIG (SELF)                                                                 │
│  ┌──────────────────────────────────────────┐    ┌────────────────────────────────────────────────────┐ │
│  │  PROJECT DETECTOR                        │    │  CONFIG MANAGER                                    │ │
│  │  Scan Cargo.toml / go.mod / package.json │    │  Read .scalpel/config.el                          │ │
│  │  Detect language ecosystem               │    │  Override: LSP priority, fallback order,          │ │
│  │                                           │    │  LLM provider, etc.                              │ │
│  └──────────────────────────────────────────┘    └────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘
</pre>

---

## Language-Specific Positioning Strategies

Scalpel uses the most reliable deterministic tool available for each language:

| Language | Primary Locator | Fallback | Precision Level |
| :--- | :--- | :--- | :--- |
| Rust | rust-analyzer (LSP) | Tree-sitter | Semantic (full) |
| TypeScript / JavaScript | tsserver (LSP) | Tree-sitter | Semantic (full) |
| Go | gopls (LSP) | Tree-sitter | Semantic (full) |
| Python | pylsp / pyright (LSP) | Tree-sitter | Semantic (limited) |
| C / C++ | clangd (LSP) | Tree-sitter + Ripgrep | Semantic (requires compile_commands.json) |
| JSON / YAML / TOML | Tree-sitter | Ripgrep | Structural |
| Other languages | Tree-sitter (if available) | Ripgrep | Structural |

---

## Installation

### Prerequisites

- Emacs 29.1 or later
- lsp-mode or eglot (for LSP support)
- magit (for Git integration, optional but recommended)
- gptel (for LLM gateway)

### Install from MELPA (once published)

```elisp
(use-package scalpel
  :ensure t
  :config
  (scalpel-setup))
```

### Manual install

```bash
git clone https://github.com/your-username/scalpel.git ~/.emacs.d/site-lisp/scalpel
```

Then add to your init.el:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/scalpel")
(require 'scalpel)
(scalpel-setup)
```

---

## Usage

### Basic workflow

1. Place your cursor on or inside the symbol/function you want to modify.
2. Run `M-x scalpel-edit`.
3. Type your instruction in the minibuffer, for example:
   ```
   change the return type of this function from i32 to f64
   ```
4. Scalpel highlights the target region.
5. Confirm with `y` – Scalpel calls the LLM, generates new code, and replaces *only* the target block.
6. An `ediff` buffer shows you the before/after diff.
7. Accept the change (`y`) or reject (`n`).

### Advanced features

- `M-x scalpel-refactor` – for cross-file refactoring (uses LSP `references`).
- `M-x scalpel-history` – shows a Magit-style history of all Scalpel sessions.
- `M-x scalpel-revert-session` – reverts all changes from a specific session.

---

## Privacy and Safety

- All LLM requests go through `gptel` – you choose the backend (OpenAI, Anthropic, Ollama, etc.).
- Your source code is **never uploaded** to any third-party server (except when using cloud model APIs).
- Every modification is atomically committed to the Git orphan branch `scalpel/autosave` – you can roll back any session with a single command.

---

## Technology Stack

| Component | Implementation |
| :--- | :--- |
| Core Language | Elisp (Emacs Lisp) |
| LSP Client | lsp-mode or eglot (user's choice) |
| LLM Gateway | gptel (supports OpenAI, Anthropic, Ollama, etc.) |
| Version Control | magit + system Git |
| Diff Rendering | ediff / diff-mode |
| Build / Package | package.el / straight.el / quelpa |

---

## Contributing

Issues and pull requests are welcome.

Core principles:

- Maintain the **decoupling** of location and generation.
- **No user-written plugins** – all language support is officially bundled.
- Every new language must be added by the core team.
- Keep it **Emacs-native** – embrace Emacs' built-in tools (`lsp-mode`, `magit`, `ediff`) rather than reinventing them.

---

## License

This project is licensed under the Apache License 2.0 – see the [LICENSE](LICENSE) file for details.

---

## Etymology

The name **Scalpel** evokes surgical precision – sharp, focused, and minimally invasive. That is exactly what this tool does: it operates on your code with the same care a surgeon applies to a patient, never touching healthy tissue.
