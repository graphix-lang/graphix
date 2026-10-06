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
-- CR claude for claude: [bug] The documented install cannot work. `require('graphix')`
-- looks in `lua/` on the runtimepath, but this file sits at the plugin root. Copying it
-- into `~/.config/nvim/lua/` breaks the `../../tree-sitter-graphix/queries` path at
-- line 105, and highlighting is skipped silently. When that path does resolve, this
-- function writes `queries/graphix/*.scm` symlinks into the checkout (111-121), which
-- shows up as untracked files under ide/tree-sitter-graphix/queries/.
-- `parsers.get_parser_configs()` (90) does not exist on nvim-treesitter's main branch,
-- where `nvim-treesitter.parsers` is a plain table and custom parsers are added in a
-- `User TSUpdate` autocmd, so setup() throws there. Lay the plugin out as
-- `lua/graphix/init.lua` + `ftdetect/` + a committed `queries/graphix/` of symlinks, as
-- Zed does. (ide-tooling.r2-11)
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
  -- CR claude for claude: [bug] This path exists only when graphix.lua is loaded from
  -- ide/editors/nvim in a checkout, but `require('graphix')` finds a module only at
  -- `<rtp>/lua/graphix.lua` and this directory has no lua/: in both layouts the header
  -- describes (copied into the config, or added by a plugin manager) either the require
  -- fails or the queries never reach the runtimepath, so a parser installed with
  -- :TSInstall gets no highlighting. Where the path does resolve, lines 111-121 write
  -- symlinks into the checkout's queries/graphix/, which git does not ignore. Each
  -- FileType event also adds another BufWritePre (136, no augroup), so a buffer
  -- reloaded twice with :e formats three times per save, and line 74's
  -- vim.fs.find('.git', {upward = true}) searches from the CWD, not the buffer. A
  -- runtime layout of lua/graphix.lua, ftdetect/ and queries/graphix/ (symlinks, as
  -- Zed's are) needs none of this path arithmetic. (ide-tooling-11)
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

--- Format graphix buffers through the LSP before each write, and indent
--- them the way the formatter does.
function M.setup_format_on_save()
  vim.api.nvim_create_autocmd('FileType', {
    pattern = 'graphix',
    callback = function(args)
      vim.bo[args.buf].shiftwidth = 4
      vim.bo[args.buf].softtabstop = 4
      vim.bo[args.buf].expandtab = true
      vim.api.nvim_create_autocmd('BufWritePre', {
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
