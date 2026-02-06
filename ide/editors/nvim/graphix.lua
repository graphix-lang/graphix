-- Neovim configuration for Graphix language support
--
-- Installation:
-- 1. Copy this file to your Neovim config (e.g., ~/.config/nvim/lua/graphix.lua)
-- 2. Add `require('graphix').setup()` to your init.lua
-- 3. Install graphix-lsp: `cargo install --path path/to/graphix-claude/ide/graphix-lsp`
-- 4. Optionally install tree-sitter grammar for syntax highlighting

local M = {}

-- Setup file type detection
function M.setup_filetype()
    vim.filetype.add({
        extension = {
            gx = "graphix",
        },
    })
end

-- Setup LSP
function M.setup_lsp()
    local lspconfig = require('lspconfig')
    local configs = require('lspconfig.configs')

    -- Register the graphix language server
    if not configs.graphix then
        configs.graphix = {
            default_config = {
                cmd = { 'graphix-lsp' },
                filetypes = { 'graphix' },
                root_dir = function(fname)
                    return lspconfig.util.find_git_ancestor(fname)
                        or lspconfig.util.path.dirname(fname)
                end,
                single_file_support = true,
                settings = {},
            },
        }
    end

    lspconfig.graphix.setup({})
end

-- Setup tree-sitter (requires nvim-treesitter)
function M.setup_treesitter()
    local ok, parsers = pcall(require, 'nvim-treesitter.parsers')
    if not ok then
        return
    end

    local parser_config = parsers.get_parser_configs()
    parser_config.graphix = {
        install_info = {
            url = "path/to/graphix-claude/ide/tree-sitter-graphix",
            files = { "src/parser.c" },
            branch = "main",
        },
        filetype = "graphix",
    }
end

-- Main setup function
function M.setup(opts)
    opts = opts or {}

    M.setup_filetype()

    if opts.lsp ~= false then
        M.setup_lsp()
    end

    if opts.treesitter ~= false then
        M.setup_treesitter()
    end
end

return M
