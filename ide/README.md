# Graphix IDE Support

This directory contains IDE support tools for the Graphix programming language:

- **tree-sitter-graphix/** - Tree-sitter grammar for syntax highlighting
- **editors/** - Editor-specific configurations
- **skills/** - The language reference for coding agents (Claude Code)

The LSP server is built into the main `graphix` binary and launched via `graphix lsp`.

## Quick Start

### 1. Build Graphix

```bash
cargo build --release -p graphix-shell

# Or install it
cargo install --path graphix-shell
```

### 2. Set Up Your Editor

#### VS Code

1. Copy `editors/vscode/` to your extensions directory
2. Run `npm install` and `npm run compile` in the extension directory
3. The extension will use `graphix lsp` from your PATH

#### Neovim

`editors/nvim/` is a runtime directory (`lua/graphix/`, `ftdetect/`,
`queries/graphix/`). Put it on the runtimepath, through your plugin
manager (lazy.nvim: `{ dir = "/path/to/graphix/ide/editors/nvim" }`) or
`vim.opt.runtimepath:append(..)`, then:

```lua
require('graphix').setup()
```

and `:TSInstall graphix` for the grammar. See the header of
`editors/nvim/lua/graphix/init.lua` for the options.

#### Emacs

1. Copy `editors/emacs/graphix-mode.el` to your load-path
2. Add to init.el:
   ```elisp
   (require 'graphix-mode)

   ;; For Eglot:
   (add-to-list 'eglot-server-programs '(graphix-mode . ("graphix" "lsp")))
   ```

#### Helix

```bash
cd editors/helix && ./install.sh
```

The install script links the tree-sitter queries into
`~/.config/helix/runtime/queries/graphix/` (copies them with `--copy`),
appends the language/server/grammar blocks to
`~/.config/helix/languages.toml` with the grammar built from this
checkout, and runs `hx --grammar build`. See `editors/helix/README.md` for details.

#### Zed

See `editors/zed/README.md` for Zed-specific instructions.

#### Claude Code

Graphix is not in any model's training set, so an agent writing it
needs the language reference in front of it. `skills/graphix-lang` is
that reference as a Claude Code skill; symlink it in and load it with
`/graphix-lang` before touching a `.gx` file:

```bash
ln -s "$(pwd)/skills/graphix-lang" ~/.claude/skills/graphix-lang
```

## Features

### Tree-sitter Grammar

Provides syntax highlighting for:
- Comments (line and documentation)
- Attributes (`#[sync]`, `#[native]`, ...)
- Keywords (`let`, `mod`, `use`, `type`, `fn`, `select`, etc.)
- Operators
- Strings (including interpolation and raw strings)
- Numbers (integers, floats, hex, binary, octal, durations)
- Types (primitive and user-defined)
- Variants and labeled parameters

### LSP Server

The LSP server runs as a subcommand of the `graphix` binary (`graphix lsp`).

Currently supports:
- **Diagnostics**: the check's errors and warnings
- **Completions**: Symbol completion from the environment
- **Hover**: Type and documentation display
- **Go to Definition** and **References**
- **Document and Workspace Symbols**
- **Formatting**: `graphix fmt` as one whole-document edit

## Building Tree-sitter Grammar

The tree-sitter grammar requires Node.js and tree-sitter-cli:

```bash
cd tree-sitter-graphix
npm install
npm run generate
npm test        # ./check.sh: parse every .gx/.gxi here (and in ../netidx), compile the queries
```

## Changing the language syntax

Six integrations render Graphix, and they fail differently: the
tree-sitter ones die LOUD (one stale node name and the editor shows no
colors at all — the whole query is refused), the regex ones die quiet
(the new form is simply uncolored). Both were true after the
2026-08-18 string/use changes, so the checklist is:

1. `tree-sitter-graphix/grammar.js` (+ `src/scanner.c` for anything the
   internal lexer can't express), then `npm run generate`.
2. `tree-sitter-graphix/queries/*.scm` — the CANONICAL queries. Helix,
   Zed and Neovim all read these files (the first two by symlink), so
   there is one copy to edit.
3. `editors/emacs/graphix-mode.el` — its queries capture font-lock
   faces, so they're written separately in elisp.
4. `editors/vim/syntax/graphix.vim` and
   `editors/vscode/syntaxes/graphix.tmLanguage.json` — regex
   highlighters, no grammar to check them against.
5. `skills/graphix-lang/SKILL.md` — the quietest of all: an agent
   reading a stale rule writes the old form, and `--check` is the only
   thing that tells it. Fix the rule, and when awkward code came from a
   rule that was missing rather than wrong, add the rule.

The gate for 1–3 is `cargo test -p graphix-types queries_compile`
(every query compiles against the built grammar), the ts-compat
proptests in the same module (the grammar parses the printer's
canonical output) and `tree-sitter-graphix/check.sh` (every program in
the repo; its count of files with errors should only go down). None of
these sees a wrong tree shape or syntax the compiler refuses, so check
new forms by eye with `tree-sitter parse`. 4 and 5 have no gate.

## Architecture

```
ide/
├── tree-sitter-graphix/    # Tree-sitter grammar
│   ├── grammar.js          # Grammar definition
│   ├── package.json
│   └── queries/
│       ├── highlights.scm  # Syntax highlighting
│       ├── locals.scm      # Scope tracking
│       └── indents.scm     # Auto-indentation
│
├── editors/
│   ├── vscode/             # VS Code extension
│   ├── nvim/               # Neovim configuration
│   ├── emacs/              # Emacs major mode
│   ├── helix/              # Helix install script + queries
│   └── zed/                # Zed configuration
│
└── skills/
    └── graphix-lang/       # Claude Code skill: the language reference
```

The LSP server source lives in `graphix-lsp/`; its backend is
`graphix-shell/src/lsp_backend.rs`.
