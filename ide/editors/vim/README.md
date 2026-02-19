# Graphix support for Vim

Regex-based syntax highlighting, filetype detection, and LSP setup
instructions for traditional Vim (8+).

For Neovim, use the `nvim/` config instead — it has tree-sitter support.

## Installation

Copy (or symlink) the contents of this directory into your vim runtime:

```bash
# Option 1: symlink into ~/.vim/pack
mkdir -p ~/.vim/pack/graphix/start
ln -s /path/to/graphix/ide/editors/vim ~/.vim/pack/graphix/start/graphix

# Option 2: copy files
cp -r ftdetect ftplugin syntax ~/.vim/
```

## LSP support

### vim-lsp

Install [vim-lsp](https://github.com/prabirshrestha/vim-lsp) and add to
your `.vimrc`:

```vim
if executable('graphix')
  au User lsp_setup call lsp#register_server(#{
    \ name: 'graphix',
    \ cmd: ['graphix', 'lsp'],
    \ allowlist: ['graphix'],
    \ })
endif
```

### coc.nvim

Add to `:CocConfig` (`coc-settings.json`):

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
