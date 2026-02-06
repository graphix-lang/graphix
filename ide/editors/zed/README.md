# Graphix Support for Zed

This directory contains configuration for Graphix language support in Zed editor.

## Installation

1. Copy the `languages/graphix` directory to your Zed extensions directory

2. Install the Graphix binary:
   ```bash
   cargo install --path path/to/graphix-claude/graphix-shell
   ```

3. Add to your Zed settings.json:
   ```json
   {
     "lsp": {
       "graphix-lsp": {
         "binary": {
           "path": "graphix",
           "arguments": ["lsp"]
         }
       }
     },
     "languages": {
       "Graphix": {
         "language_servers": ["graphix-lsp"]
       }
     }
   }
   ```

## Tree-sitter Grammar

Zed uses tree-sitter for syntax highlighting. To build the tree-sitter grammar:

```bash
cd path/to/graphix-claude/ide/tree-sitter-graphix
npm install
npm run build
```

Then copy the built grammar to your Zed configuration.

## Features

- Syntax highlighting (via tree-sitter or TextMate grammar)
- LSP support for:
  - Diagnostics (parse errors)
  - Completions
  - Hover information
  - Go to definition
