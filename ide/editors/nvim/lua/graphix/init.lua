-- Neovim configuration for Graphix language support
--
-- Installation:
--
-- 1. Put this directory (ide/editors/nvim) on the runtimepath, through a
--    plugin manager (e.g. lazy.nvim `{ dir = ".../ide/editors/nvim" }`) or
--    `vim.opt.runtimepath:append(".../ide/editors/nvim")`. Its layout is a
--    runtime directory:
--
--      nvim/
--        lua/graphix/init.lua -- this file: require('graphix')
--        ftdetect/graphix.lua -- filetype detection
--        queries/graphix/     -- the grammar's queries (symlinks into
--                                ide/tree-sitter-graphix/queries)
--
--    Copying it elsewhere copies the query symlinks' targets only if you
--    copy with dereferencing (`cp -rL`).
--
-- 2. In your init.lua:
--
--      require('graphix').setup()
--
-- 3. Install the tree-sitter grammar (inside Neovim):
--
--      :TSInstall graphix
--
-- 4. Ensure `graphix` is on your PATH (the binary provides the LSP server
--    via `graphix lsp`).
--
-- Options (all default to true):
--
--   require('graphix').setup({
--     lsp = true,             -- configure LSP via nvim-lspconfig
--     treesitter = true,      -- register tree-sitter grammar
--     format_on_save = true,  -- format through the LSP before each write
--   })

local M = {}

--- Register the graphix filetype for .gx and .gxi files.
function M.setup_filetype()
  vim.filetype.add({
    extension = {
      gx = "graphix",
      gxi = "graphix",
    },
  })
end

--- Configure the graphix LSP server via nvim-lspconfig.
--- Falls back to vim.lsp.start() if lspconfig is not installed.
function M.setup_lsp()
  local ok, lspconfig = pcall(require, 'lspconfig')
  if ok then
    local configs = require('lspconfig.configs')
    if not configs.graphix then
      configs.graphix = {
        default_config = {
          cmd = { 'graphix', 'lsp' },
          filetypes = { 'graphix' },
          root_dir = function(fname)
            return lspconfig.util.find_git_ancestor(fname)
              or lspconfig.util.path.dirname(fname)
          end,
          single_file_support = true,
        },
      }
    end
    lspconfig.graphix.setup({})
  else
    -- No lspconfig — use built-in vim.lsp (Neovim 0.11+)
    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'graphix',
      callback = function(args)
        vim.lsp.start({
          name = 'graphix',
          cmd = { 'graphix', 'lsp' },
          root_dir = vim.fs.root(args.buf, '.git')
            or vim.fs.dirname(vim.api.nvim_buf_get_name(args.buf)),
        })
      end,
    })
  end
end

local install_info = {
  url = "https://github.com/graphix-lang/graphix",
  files = { "src/parser.c", "src/scanner.c" },
  location = "ide/tree-sitter-graphix",
  branch = "main",
}

--- Register the graphix tree-sitter parser with nvim-treesitter. The
--- queries are on the runtimepath with this plugin (queries/graphix/).
function M.setup_treesitter()
  local ok, parsers = pcall(require, 'nvim-treesitter.parsers')
  if not ok then
    return
  end
  if type(parsers.get_parser_configs) == 'function' then
    -- nvim-treesitter's master branch
    parsers.get_parser_configs().graphix = {
      install_info = install_info,
      filetype = "graphix",
    }
  else
    -- its main branch: parsers is a plain table, refilled on TSUpdate
    vim.api.nvim_create_autocmd('User', {
      pattern = 'TSUpdate',
      callback = function()
        require('nvim-treesitter.parsers').graphix = {
          install_info = {
            url = install_info.url,
            location = install_info.location,
            branch = install_info.branch,
          },
        }
      end,
    })
  end
end

--- Format graphix buffers through the LSP before each write, and indent
--- them the way the formatter does.
function M.setup_format_on_save()
  local group = vim.api.nvim_create_augroup('graphix_format', { clear = true })
  vim.api.nvim_create_autocmd('FileType', {
    group = group,
    pattern = 'graphix',
    callback = function(args)
      vim.bo[args.buf].shiftwidth = 4
      vim.bo[args.buf].softtabstop = 4
      vim.bo[args.buf].expandtab = true
      -- one hook per buffer, however often its FileType fires
      vim.api.nvim_clear_autocmds({ group = group, event = 'BufWritePre', buffer = args.buf })
      vim.api.nvim_create_autocmd('BufWritePre', {
        group = group,
        buffer = args.buf,
        callback = function()
          vim.lsp.buf.format({ bufnr = args.buf, name = 'graphix', async = false })
        end,
      })
    end,
  })
end

--- Main setup function.
---@param opts? { lsp?: boolean, treesitter?: boolean, format_on_save?: boolean }
function M.setup(opts)
  opts = opts or {}
  M.setup_filetype()
  if opts.lsp ~= false then
    M.setup_lsp()
    if opts.format_on_save ~= false then
      M.setup_format_on_save()
    end
  end
  if opts.treesitter ~= false then
    M.setup_treesitter()
  end
end

return M
