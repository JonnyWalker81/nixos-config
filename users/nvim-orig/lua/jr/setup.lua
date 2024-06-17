-- require('lazy').setup({
--  {'folke/tokyonight.nvim'},
--  {
--    'VonHeikemen/lsp-zero.nvim',
--    branch = 'v2.x',
--    dependencies = {
--      -- LSP Support
--      {'neovim/nvim-lspconfig'},             -- Required
--      {                                      -- Optional
--        'williamboman/mason.nvim',
--        build = function()
--          pcall(vim.cmd, 'MasonUpdate')
--        end,
--      },
--      {'williamboman/mason-lspconfig.nvim'}, -- Optional
-- 
--      -- Autocompletion
--      {'hrsh7th/nvim-cmp'},     -- Required
--      {'hrsh7th/cmp-nvim-lsp'}, -- Required
--      {'L3MON4D3/LuaSnip'},     -- Required
--    }
--  }
-- })

require("lazy").setup("jr.plugins", {
  -- defaults = { lazy = true },
  install = {
    -- install missing plugins on startup. This doesn't increase startup time.
    missing = false,
  },
  change_detection = {
    -- automatically check for config file changes and reload the ui
    enabled = false,
    notify = true, -- get a notification when changes are found
  },
  debug = false,
})

-- Set colorscheme
vim.opt.termguicolors = true
vim.cmd.colorscheme('tokyonight')

-- LSP
local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
  lsp.default_keymaps({buffer = bufnr})
end)

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

lsp.setup()
