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
putting queries into your runtime path and appending blocks to
`languages.toml`. The repo ships a script that does both:

```bash
cd ide/editors/helix
./install.sh
```

The script:

1. Links the tree-sitter queries into
   `~/.config/helix/runtime/queries/graphix/` (`--copy` copies them
   instead, for a checkout you won't keep).
2. Appends `[[language]]`, `[language-server.graphix-lsp]`, and
   `[[grammar]]` blocks to `~/.config/helix/languages.toml` (idempotent
   — it skips if a graphix entry already exists).
3. Runs `helix --grammar fetch && helix --grammar build` to compile
   the parser.

Re-running the script is safe; it replaces the query links (so updates
land) but leaves your `languages.toml` alone after the first install.
Re-run it after pulling a grammar change — the queries name grammar
nodes, and the compiled grammar is the half that doesn't track your
checkout.

To verify, open a `.gx` file and run `:tree-sitter-scopes` or
`:lsp-restart`. Note that `helix --health graphix` only checks that the
query files exist; if coloring is missing entirely, the reason is in
`~/.cache/helix/helix.log`.

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

## Formatting

`graphix fmt` is the source formatter. It rewrites each file you name in
place, or formats stdin to stdout when you name none, which is the form
an editor's "format on save" hook wants:

```bash
graphix fmt src/main.gx src/lib.gxi   # rewrite in place
graphix fmt --check src/*.gx          # list files that would change, fail if any
graphix fmt --stdout src/main.gx      # print instead of rewriting
graphix fmt < main.gx                 # stdin to stdout (--interface for a .gxi)
graphix fmt --width 100 --indent 2 main.gx   # override graphixfmt.json for this run
```

### Configuration

The line width and the indent come from a `graphixfmt.json`; anything it
leaves out keeps its default:

```json
{ "width": 90, "indent": 4 }
```

The formatter uses the nearest `graphixfmt.json` in the source file's
directory or any directory above it, so a project keeps one at the base
of its repository. Failing that it uses yours, in `graphix/` under your
platform's configuration directory:

| Platform | Path |
| --- | --- |
| Linux | `$XDG_CONFIG_HOME/graphix/graphixfmt.json`, else `~/.config/graphix/graphixfmt.json` |
| macOS | `~/Library/Application Support/graphix/graphixfmt.json` |
| Windows | `%APPDATA%\graphix\graphixfmt.json` |

With neither it fits 90 columns and indents by 4. `--width` and
`--indent` override the file for one run, and the language server reads
the same files. A `graphixfmt.json` that does not parse, or that names a
setting the formatter does not have, is an error rather than a silent
fallback.

### What it changes

The formatter keeps what you chose where the choice is yours: the order
of struct fields and of the variants in a union, the delimiters of a
string (`"…"`, `r"…"`, `"""…"""`), your comments and attributes. It
normalizes the rest: adjacent `use` statements are merged into one sorted
tree per root, `i64`/`f64` literals lose a redundant type prefix,
primitive types come first in a union, a blank line separates every
top-level item that spans more than one line, and lines are fitted to the
width. Before anything is written the formatted text is parsed again and
compared with your program; if the two differ in any way the file is left
untouched and the difference is reported, so a formatter bug can never
change what your code means.

The language server formats through the same code, and the editor
configurations in `ide/editors/` turn format-on-save ON by default, with
a four-space indent to match the formatter. To turn it off:

| Editor | Setting |
| --- | --- |
| VS Code | `"[graphix]": { "editor.formatOnSave": false }` in `settings.json` |
| Neovim | `require('graphix').setup({ format_on_save = false })` |
| Helix | `auto-format = false` in the graphix entry of `languages.toml` |
| Emacs | `(setq graphix-format-on-save nil)` |
| Zed | `"languages": { "Graphix": { "format_on_save": "off" } }` in `settings.json` |

A file that does not parse is left alone. Plain Vim has no LSP client of
its own; use `:%!graphix fmt` there.

## What the LSP currently supports

| Feature | Status |
| --- | --- |
| Diagnostics (parse and type errors, warnings) | ✓ |
| Completions: names in scope, struct fields, call labels, `use` trees | ✓ |
| Hover with type and doc information | ✓ |
| Go-to-definition | ✓ |
| Find references | ✓ |
| Document and workspace symbols | ✓ |
| Formatting (whole document) | ✓ |

Diagnostics arrive as you type, for the files you have open and the
projects they belong to. A project is a `.gx` file that no other file
loads with `mod`, together with everything it loads; a file outside
every project is checked by itself. Files you have not opened are not
checked. The compiler stops at the first error, so a project shows one
diagnostic at a time.

While a buffer does not compile, completion, hover and navigation answer
from the last version that did. Completion offers the names in scope
(locals included), a struct's fields after `.`, a call's `#labels` inside
its parentheses, and inside a `use` what the module under the cursor
exports, however deep the tree.

Warnings (an error no `catch` will see, for instance) are shown beside
errors, and stay while a buffer does not compile.

A package that defines its own builtins (an external `graphix-package-*`
crate) can be edited with the stock `graphix`: each builtin it declares
that this binary was not built with is a warning on its `'name`, calls to
it are checked against its declared signature, and everything else is
checked as usual. Run the `graphix` built with the package to clear them.

Rename and code actions are not yet implemented. File issues at
[graphix-lang/graphix](https://github.com/graphix-lang/graphix/issues)
if something specific would unblock you.

## Reporting editor issues

Editor configs live in the same repo as the compiler, so bugs in
syntax highlighting, indentation, or LSP behavior all belong in the
same issue tracker. When filing one, mention which editor and which
mode (`graphix-mode` vs `graphix-ts-mode`, vim vs nvim, etc.) you're
using, plus a minimal `.gx` snippet that triggers the problem.
