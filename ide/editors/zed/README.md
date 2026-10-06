# Graphix support for Zed

Tree-sitter syntax highlighting and LSP integration for the Graphix
programming language in Zed editor.

## Installation

### From source (dev install)

1. Clone or symlink this directory into your Zed extensions:

   ```bash
   ln -s /path/to/graphix/ide/editors/zed ~/.config/zed/extensions/installed/graphix
   ```

2. Ensure `graphix` is on your PATH (the LSP server is launched via
   `graphix lsp`).

3. Restart Zed. The extension will detect `.gx` and `.gxi` files
   automatically.

## Features

- Syntax highlighting via tree-sitter (highlights.scm)
- Indentation rules (indents.scm)
- Local variable tracking (locals.scm)
- LSP support: diagnostics, completions, hover, go-to-definition
- Bracket matching and auto-closing

## Maintenance

The query files under `languages/graphix/` (`highlights.scm`,
`indents.scm`, `locals.scm`) are symlinks to the canonical queries in
`ide/tree-sitter-graphix/queries/`, so a dev extension always reads the
current queries. Zed packages a published extension as a self-contained
directory, where a symlink pointing outside it does not survive:
dereference them when packaging (`cp -rL`).
