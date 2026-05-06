# Editor Setup

Graphix ships with a Language Server Protocol (LSP) implementation and
tree-sitter grammar, so any editor with LSP support can give you
diagnostics, completion, hover, and go-to-definition for `.gx` and `.gxi`
files.

The LSP server is built into the `graphix` binary itself — there is no
separate `graphix-lsp` executable to install. Editors launch it by
running:

```bash
graphix lsp
```

It speaks LSP over stdin/stdout, so make sure `graphix` is on the `PATH`
that your editor sees.

Editor-specific configurations live in
[`ide/editors/`](https://github.com/graphix-lang/graphix/tree/main/ide/editors)
in the source tree. The sections below summarize how to install each
one. None of them are published to upstream package registries yet —
the modes for Emacs, Helix, and Zed are intended for upstream submission
in a future release, but until then they install from the repo.

## Prerequisites

Make sure the `graphix` binary is on your `PATH`:

```bash
cargo install graphix-shell
# or, from a checkout:
cargo install --path graphix-shell
```

Then verify:

```bash
graphix lsp --help
```

If your editor cannot find `graphix`, point its config at the absolute
path of the binary (`~/.cargo/bin/graphix` on a typical Cargo install).

## VS Code

The extension lives in `ide/editors/vscode/`. It bundles a TextMate
grammar for syntax highlighting and a thin client that launches
`graphix lsp`.

```bash
cd ide/editors/vscode
npm install
npm run compile
```

Then either symlink the directory into `~/.vscode/extensions/graphix`
or open it in VS Code and press <kbd>F5</kbd> to launch a development
host.

The extension exposes a single setting, `graphix.server.path`, which
defaults to `graphix`. Override it if the binary is not on your
editor's `PATH`.

## Neovim

The Neovim plugin in `ide/editors/nvim/` registers the filetype, wires
up the LSP via `nvim-lspconfig` (with a fallback to the built-in
`vim.lsp.start` when lspconfig is missing), and registers the
tree-sitter grammar.

Drop the `nvim/` directory into your config or your plugin manager,
then call:

```lua
require('graphix').setup()
```

To install the tree-sitter grammar, run `:TSInstall graphix` after
`nvim-treesitter` has picked up the registered parser config.

You can disable either piece independently:

```lua
require('graphix').setup({ lsp = true, treesitter = false })
```

## Vim

For traditional Vim (8+), `ide/editors/vim/` provides regex-based
syntax highlighting and filetype detection. Tree-sitter is Neovim-only,
so this is the right config for plain Vim.

```bash
mkdir -p ~/.vim/pack/graphix/start
ln -s "$PWD/ide/editors/vim" ~/.vim/pack/graphix/start/graphix
```

LSP setup depends on your client. With
[vim-lsp](https://github.com/prabirshrestha/vim-lsp):

```vim
if executable('graphix')
  au User lsp_setup call lsp#register_server(#{
    \ name: 'graphix',
    \ cmd: ['graphix', 'lsp'],
    \ allowlist: ['graphix'],
    \ })
endif
```

With [coc.nvim](https://github.com/neoclide/coc.nvim), add to
`coc-settings.json`:

```json
{
  "languageserver": {
    "graphix": {
      "command": "graphix",
      "args": ["lsp"],
      "filetypes": ["graphix"]
    }
  }
}
```

## Emacs

`ide/editors/emacs/graphix-mode.el` provides two modes:

- `graphix-mode` — regex-based fallback for Emacs < 29 or when the
  tree-sitter grammar is not installed.
- `graphix-ts-mode` — tree-sitter mode for Emacs 29+. Selected
  automatically when the grammar is available.

Drop the file on your `load-path` and require it:

```elisp
(require 'graphix-mode)

;; Eglot (built-in, Emacs 29+):
(add-to-list 'eglot-server-programs
             '(graphix-mode . ("graphix" "lsp")))
(add-hook 'graphix-mode-hook #'eglot-ensure)
(add-hook 'graphix-ts-mode-hook #'eglot-ensure)
```

To install the tree-sitter grammar:

```
M-x graphix-ts-mode-install-grammar
```

This compiles the grammar from the GitHub repo via
`treesit-install-language-grammar`. Once it succeeds, opening a `.gx`
file will route to `graphix-ts-mode` automatically.

`lsp-mode` users can replace the `eglot-*` lines with the equivalent
`lsp-mode` registration; the LSP command is the same.

## Helix

Helix has no plugin system, so language support has to be added by
copying queries into your runtime path and appending blocks to
`languages.toml`. The repo ships a script that does both:

```bash
cd ide/editors/helix
./install.sh
```

The script:

1. Copies tree-sitter queries to
   `~/.config/helix/runtime/queries/graphix/`.
2. Appends `[[language]]`, `[language-server.graphix-lsp]`, and
   `[[grammar]]` blocks to `~/.config/helix/languages.toml` (idempotent
   — it skips if a graphix entry already exists).
3. Runs `helix --grammar fetch && helix --grammar build` to compile
   the parser.

Re-running the script is safe; it overwrites the queries (so updates
land) but leaves your `languages.toml` alone after the first install.

To verify, open a `.gx` file and run `:tree-sitter-scopes`,
`:lsp-restart`, or `helix --health graphix`.

If you're hacking on the grammar locally, edit the `[[grammar]]` block
in your `languages.toml` to point at your checkout instead of the git
URL. See `ide/editors/helix/README.md` for the exact form.

## Zed

The Zed extension lives in `ide/editors/zed/`. For development:

```bash
ln -s "$PWD/ide/editors/zed" ~/.config/zed/extensions/installed/graphix
```

Restart Zed. The extension auto-detects `.gx` and `.gxi` files,
launches `graphix lsp`, and ships its own copy of the tree-sitter
queries.

The query files under `languages/graphix/` are duplicates of the
canonical ones in `ide/tree-sitter-graphix/queries/` because Zed
packages each extension as a self-contained directory. If you change
the upstream queries, copy them across:

```bash
cp ide/tree-sitter-graphix/queries/{highlights,indents,locals}.scm \
   ide/editors/zed/languages/graphix/
```

## What the LSP currently supports

| Feature | Status |
| --- | --- |
| Diagnostics (parse + type errors) | ✓ |
| Completions from the active environment | ✓ |
| Hover with type and doc information | ✓ |
| Go-to-definition | ✓ |

Find references, rename, code actions, and formatting are not yet
implemented. File issues at
[graphix-lang/graphix](https://github.com/graphix-lang/graphix/issues)
if something specific would unblock you.

## Reporting editor issues

Editor configs live in the same repo as the compiler, so bugs in
syntax highlighting, indentation, or LSP behavior all belong in the
same issue tracker. When filing one, mention which editor and which
mode (`graphix-mode` vs `graphix-ts-mode`, vim vs nvim, etc.) you're
using, plus a minimal `.gx` snippet that triggers the problem.
