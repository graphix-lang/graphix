# Graphix support for Helix

Tree-sitter syntax highlighting and LSP integration for the Graphix
programming language in the [Helix](https://helix-editor.com) editor.

## Why this needs an installer

Helix has no plugin system. Language support lives in two places:

- **`languages.toml`** in your config dir, which declares the language,
  the LSP command, and the tree-sitter grammar source.
- **`runtime/queries/<lang>/`**, where Helix looks for `highlights.scm`,
  `indents.scm`, etc.

Both must be set up by hand (or by a script) inside `~/.config/helix/`.
Once Helix's plugin system (Steel) ships, this can move into a real
extension. Until then, run `install.sh`.

## Install

Prerequisites:

- `helix` on PATH
- `graphix` on PATH (`cargo install --path graphix-shell` from the repo
  root, or grab it from your package manager once published).

Then:

```bash
./install.sh
```

The script:

1. Copies the queries in `queries/` to
   `~/.config/helix/runtime/queries/graphix/`.
2. Appends the language and grammar blocks from `languages.toml` to your
   `~/.config/helix/languages.toml`. If a graphix entry is already
   present, it leaves the file alone.
3. Runs `helix --grammar fetch && helix --grammar build` to compile the
   tree-sitter parser.

Re-running is safe — step 1 overwrites queries (so updates land), step 2
is idempotent.

## Grammar source: local checkout vs git

`languages.toml` ships with the git source enabled by default. If you're
hacking on the grammar in a local checkout, comment that out and use the
local-path block instead:

```toml
[[grammar]]
name = "graphix"
source = { path = "/absolute/path/to/graphix/ide/tree-sitter-graphix" }
```

After grammar changes, rerun `helix --grammar build`.

## Verifying

Open a `.gx` file and:

- `:tree-sitter-scopes` — shows every capture tagged at the cursor.
- `:lsp-restart` — useful after rebuilding the `graphix` binary.
- `gd` — go to definition (LSP).
- `K` (or `space + K`) — hover.
- `<C-x>` — completions menu while typing.

`helix --health graphix` reports the status of the parser, queries, and
LSP server.

## What's in the queries

| File | Purpose |
| --- | --- |
| `highlights.scm` | Syntax coloring. Translates the tree-sitter parse into Helix theme scopes (`@function`, `@keyword.control`, `@constant.numeric.float`, etc.). |
| `indents.scm` | Auto-indent rules — which nodes open/close a level. |
| `locals.scm` | Scope and definition tracking — drives local symbol awareness. |

`textobjects.scm` is not yet provided. Until it lands, motions like
`]f` (next function) won't work for `.gx` files.

## Uninstall

```bash
rm -rf ~/.config/helix/runtime/queries/graphix
# Then remove the [[language]] graphix, [language-server.graphix-lsp],
# and [[grammar]] graphix blocks from ~/.config/helix/languages.toml.
```
