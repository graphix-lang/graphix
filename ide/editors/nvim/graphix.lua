-- Neovim configuration for Graphix language support
--
-- Installation:
--
-- 1. Copy this directory into your Neovim config, or add it to your plugin
--    manager.  The directory layout is:
--
--      nvim/
--        graphix.lua          -- this file (plugin entry point)
--        ftdetect/graphix.lua -- filetype detection
--
-- 2. In your init.lua:
--
--      require('graphix').setup()
--
-- 3. Install the tree-sitter grammar (inside Neovim):
--
--      :TSInstall graphix
--
--    Or, if not using nvim-treesitter's :TSInstall, the grammar is registered
--    so you can build it manually from the source repo.
--
-- 4. Ensure `graphix` is on your PATH (the binary provides the LSP server
--    via `graphix lsp`).
--
-- Options (all default to true):
--
--   require('graphix').setup({
--     lsp = true,         -- configure LSP via nvim-lspconfig
--     treesitter = true,  -- register tree-sitter grammar
--   })

local M = {}

--- Register the graphix filetype for .gx files.
function M.setup_filetype()
  vim.filetype.add({
    extension = {
      gx = "graphix",
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
      callback = function()
        vim.lsp.start({
          name = 'graphix',
          cmd = { 'graphix', 'lsp' },
          root_dir = vim.fs.dirname(vim.fs.find('.git', { upward = true })[1]),
        })
      end,
    })
  end
end

--- Register the graphix tree-sitter parser with nvim-treesitter.
--- Also installs query files (highlights, indents, locals) from the grammar
--- source into Neovim's runtime path so they're picked up automatically.
function M.setup_treesitter()
  local ok, parsers = pcall(require, 'nvim-treesitter.parsers')
  if not ok then
    return
  end

  local parser_config = parsers.get_parser_configs()
  parser_config.graphix = {
    install_info = {
      url = "https://github.com/graphix-lang/graphix",
      files = { "src/parser.c", "src/scanner.c" },
      location = "ide/tree-sitter-graphix",
      branch = "main",
    },
    filetype = "graphix",
  }

  -- Install query files if they haven't been already.
  -- nvim-treesitter looks for queries/ under its runtime dirs, so we
  -- symlink from the grammar source when available.
  local source = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h")
  local queries_src = source .. "/../../tree-sitter-graphix/queries"
  if vim.fn.isdirectory(queries_src) == 1 then
    -- Add the parent of queries/ to runtimepath so nvim finds
    -- queries/graphix/*.scm automatically
    local queries_runtime = source .. "/../../tree-sitter-graphix"
    -- Rename the queries dir to match the language name nvim expects
    local target = queries_runtime .. "/queries/graphix"
    if vim.fn.isdirectory(target) == 0 and vim.fn.isdirectory(queries_src) == 1 then
      -- Create a graphix/ subdir with symlinks to the .scm files
      vim.fn.mkdir(target, "p")
      for _, f in ipairs({ "highlights.scm", "indents.scm", "locals.scm" }) do
        local src_file = queries_src .. "/" .. f
        local dst_file = target .. "/" .. f
        if vim.fn.filereadable(src_file) == 1 and vim.fn.filereadable(dst_file) == 0 then
          vim.uv.fs_symlink(vim.fn.resolve(src_file), dst_file)
        end
      end
    end
    vim.opt.runtimepath:prepend(queries_runtime)
  end
end

--- Main setup function.
---@param opts? { lsp?: boolean, treesitter?: boolean }
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
