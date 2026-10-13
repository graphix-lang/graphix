# Graphix support for Zed

Tree-sitter syntax highlighting and LSP integration for the Graphix
programming language in Zed editor.

## Installation

### From source (dev install)

1. In Zed, run `zed: install dev extension` and pick this directory;
   it builds the extension's Rust part too. (On Linux, dev extensions
   live under `~/.local/share/zed/extensions`.)

2. Ensure `graphix` is on your PATH (the LSP server is launched via
   `graphix lsp`).

3. Restart Zed. The extension will detect `.gx` and `.gxi` files
   automatically.

## Features

- Syntax highlighting via tree-sitter (highlights.scm)
- Indentation rules (indents.scm)
- LSP support: diagnostics, completions, hover, go-to-definition
- Auto-closing brackets (there is no brackets.scm, so no bracket matching)

## Maintenance

The query files under `languages/graphix/` (`highlights.scm`,
`indents.scm`, and `locals.scm`, which Zed does not read) are symlinks
to the canonical queries in
`ide/tree-sitter-graphix/queries/`, so a dev extension always reads the
current queries. Zed packages a published extension as a self-contained
directory, where a symlink pointing outside it does not survive:
dereference them when packaging (`cp -rL`).
