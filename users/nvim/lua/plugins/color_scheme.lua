return {

    {
        'catppuccin/nvim',
        config = function()
            -- require("catppuccin").setup({
            -- 	integrations = {
            -- 		cmp = true,
            -- 		gitsigns = true,
            -- 		harpoon = true,
            -- 		illuminate = true,
            -- 		indent_blankline = {
            -- 			enabled = false,
            -- 			scope_color = "sapphire",
            -- 			colored_indent_levels = true,
            -- 		},
            -- 		mason = true,
            -- 		native_lsp = { enabled = true },
            -- 		notify = true,
            -- 		nvimtree = true,
            -- 		neotree = true,
            -- 		symbols_outline = true,
            -- 		telescope = true,
            -- 		treesitter = true,
            -- 		treesitter_context = true,
            -- 	},
            -- })

            -- vim.cmd.colorscheme("catppuccin-mocha")

            -- Hide all semantic highlights until upstream issues are resolved (https://github.com/catppuccin/nvim/issues/480)
            -- for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
            -- 	vim.api.nvim_set_hl(0, group, {})
            -- end
        end,
    },

    {
        'rose-pine/neovim',
        name = 'rose-pine',
        config = function()
            require('rose-pine').setup({
                variant = 'moon',
                styles = {
                    bold = true,
                    italic = true,
                    transparency = false,
                },
            })

            vim.cmd.colorscheme('rose-pine-moon')
        end,
    },

    {
        'folke/tokyonight.nvim',
        -- lazy = false,
        -- priority = 1000,
        -- style = "moon",
        -- styles = {
        --   -- Style to be applied to different syntax groups
        --   -- Value is any valid attr-list value for `:help nvim_set_hl`
        --   comments = { italic = true },
        --   keywords = { italic = true },
        --   functions = {},
        --   variables = {},
        --   -- Background styles. Can be "dark", "transparent" or "normal"
        --   sidebars = "dark", -- style for sidebars, see below
        --   floats = "dark", -- style for floating windows
        -- },
        -- opts = {},
        -- require("tokyonight").setup({
        --   on_highlights = function(hl, c)
        --     local prompt = "#090a12"
        --     hl.TelescopeNormal = {
        --       bg = c.bg_dark,
        --       fg = c.fg_dark,
        --     }
        --     hl.TelescopeBorder = {
        --       bg = c.bg_dark,
        --       fg = c.bg_dark,
        --     }
        --     hl.TelescopePromptNormal = {
        --       bg = prompt,
        --     }
        --     hl.TelescopePromptBorder = {
        --       bg = prompt,
        --       fg = prompt,
        --     }
        --     hl.TelescopePromptTitle = {
        --       bg = prompt,
        --       fg = prompt,
        --     }
        --     hl.TelescopePreviewTitle = {
        --       bg = c.bg_dark,
        --       fg = c.bg_dark,
        --     }
        --     hl.TelescopeResultsTitle = {
        --       bg = c.bg_dark,
        --       fg = c.bg_dark,
        --     }
        --   end,
        -- })
    },
}
