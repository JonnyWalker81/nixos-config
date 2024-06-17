return {
  'stevearc/conform.nvim',
  opts = {},
  event = { 'BufReadPre', 'BufNewFile' }, -- to disable, comment this out,
  config = function()
    local function execute_cmd(command)
      local handle = io.popen(command)
      local result = handle:read('*a')
      handle:close()
      return result
    end

    local conform = require('conform')
    require('conform').setup({
      formatters_by_ft = {
        lua = { 'stylua' },
        -- Conform will run multiple formatters sequentially
        -- python = { "isort", "black" },
        -- Use a sub-list to run only the first available formatter
        -- javascript = { { "prettierd", "prettier" } },
        format_on_save = {
          lsp_fallback = true,
          async = false,
          timeout_ms = 1000,
        },
      },
      formatters = {
        stylua = {
          -- Change where to find the command
          command = execute_cmd('!which stylua'),
          -- Adds environment args to the yamlfix formatter
          env = {
          },
        },
      },
    })
    vim.keymap.set({ 'n', 'v' }, '<leader>mp', function()
      conform.format({
        lsp_fallback = true,
        async = false,
        timeout_ms = 1000,
      })
    end, { desc = 'Format file or range (in visual mode)' })
  end,
}
