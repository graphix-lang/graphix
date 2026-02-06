# Graphix IDE Support

This directory contains IDE support tools for the Graphix programming language:

- **tree-sitter-graphix/** - Tree-sitter grammar for syntax highlighting
- **graphix-lsp/** - Language Server Protocol server
- **editors/** - Editor-specific configurations

## Quick Start

### 1. Build the LSP Server

```bash
cargo build --release -p graphix-lsp

# Or install it
cargo install --path ide/graphix-lsp
```

### 2. Set Up Your Editor

#### VS Code

1. Copy `editors/vscode/` to your extensions directory
2. Run `npm install` and `npm run compile` in the extension directory
3. The extension will use `graphix-lsp` from your PATH

#### Neovim

1. Copy `editors/nvim/ftdetect/graphix.lua` to `~/.config/nvim/ftdetect/`
2. Add to your config:
   ```lua
   -- Requires nvim-lspconfig
   local lspconfig = require('lspconfig')
   lspconfig.util.default_config = vim.tbl_extend(
     'force',
     lspconfig.util.default_config,
     {
       cmd = { 'graphix-lsp' },
       filetypes = { 'graphix' },
     }
   )
   ```

#### Emacs

1. Copy `editors/emacs/graphix-mode.el` to your load-path
2. Add to init.el:
   ```elisp
   (require 'graphix-mode)

   ;; For Eglot:
   (add-to-list 'eglot-server-programs '(graphix-mode . ("graphix-lsp")))
   ```

#### Zed

See `editors/zed/README.md` for Zed-specific instructions.

## Features

### Tree-sitter Grammar

Provides syntax highlighting for:
- Comments (line and documentation)
- Keywords (`let`, `mod`, `use`, `type`, `fn`, `select`, etc.)
- Operators
- Strings (including interpolation and raw strings)
- Numbers (integers, floats, hex, binary, octal)
- Types (primitive and user-defined)
- Variants and labeled parameters

### LSP Server

Currently supports:
- **Diagnostics**: Parse error reporting
- **Completions**: Symbol completion from the environment
- **Hover**: Type and documentation display
- **Go to Definition**: Navigate to symbol definitions

## Building Tree-sitter Grammar

The tree-sitter grammar requires Node.js and tree-sitter-cli:

```bash
cd tree-sitter-graphix
npm install
npm run generate
npm test
```

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
├── graphix-lsp/            # Language server
│   ├── Cargo.toml
│   └── src/
│       ├── main.rs
│       ├── server.rs       # LSP protocol handling
│       ├── state.rs        # Document state management
│       ├── convert.rs      # Type conversions
│       └── handlers/       # LSP request handlers
│           ├── diagnostics.rs
│           ├── completion.rs
│           ├── hover.rs
│           └── definition.rs
│
└── editors/
    ├── vscode/             # VS Code extension
    ├── nvim/               # Neovim configuration
    ├── emacs/              # Emacs major mode
    └── zed/                # Zed configuration
```
