require("tokyonight").setup({
    -- your configuration comes here
    -- or leave it empty to use the default settings
    style = "moon", -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
    light_style = "day", -- The theme is used when the background is set to light
    transparent = false, -- Enable this to disable setting the background color
    terminal_colors = true, -- Configure the colors used when opening a `:terminal` in Neovim
    styles = {
        -- Style to be applied to different syntax groups
        -- Value is any valid attr-list value for `:help nvim_set_hl`
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
        -- Background styles. Can be "dark", "transparent" or "normal"
        sidebars = "dark", -- style for sidebars, see below
        floats = "dark", -- style for floating windows
    },
    sidebars = { "qf", "help" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
    day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
    hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
    dim_inactive = false, -- dims inactive windows
    lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold

    --- You can override specific color groups to use other groups or a hex color
    --- function will be called with a ColorScheme table
    ---@param colors ColorScheme
    on_colors = function(colors) end,

    --- You can override specific highlights to use other groups or a hex color
    --- function will be called with a Highlights and ColorScheme table
    ---@param highlights Highlights
    ---@param colors ColorScheme
    on_highlights = function(hl, c)
        local prompt = "#2d3149"
        hl.TelescopeNormal = {
            bg = c.bg_dark,
            fg = c.fg_dark,
        }
        hl.TelescopeBorder = {
            bg = c.bg_dark,
            fg = c.bg_dark,
        }
        hl.TelescopePromptNormal = {
            bg = prompt,
        }
        hl.TelescopePromptBorder = {
            bg = prompt,
            fg = prompt,
        }
        hl.TelescopePromptTitle = {
            bg = prompt,
            fg = prompt,
        }
        hl.TelescopePreviewTitle = {
            bg = c.bg_dark,
            fg = c.bg_dark,
        }
        hl.TelescopeResultsTitle = {
            bg = c.bg_dark,
            fg = c.bg_dark,
        }
    end,
})

vim.cmd [[colorscheme tokyonight]]

-- You don't need to set any of these options.
-- IMPORTANT!: this is only a showcase of how you can set default options!
require("telescope").setup {
    extensions = {
        file_browser = {
            theme = "ivy",
            -- disables netrw and use telescope-file-browser in its place
            hijack_netrw = true,
            mappings = {
                ["i"] = {
                    -- your custom insert mode mappings
                },
                ["n"] = {
                },
            },
        },
    },
    pickers = {
        find_files = {
            mappings = {
                n = {
                    -- ["cd"] = function(prompt_bufnr)
                    --     local selection = require("telescope.actions.state").get_selected_entry()
                    --     local dir = vim.fn.fnamemodify(selection.path, ":p:h")
                    --     require("telescope.actions").close(prompt_bufnr)
                    --     -- Depending on what you want put `cd`, `lcd`, `tcd`
                    --     vim.cmd(string.format("silent lcd %s", dir))
                    -- end
                },
            },
        },
    },
}
-- To get telescope-file-browser loaded and working with telescope,
-- you need to call load_extension, somewhere after setup function:
require("telescope").load_extension "file_browser"

require('toggleterm').setup({
    open_mapping = '<C-g>',
    direction = 'horizontal',
    shade_terminals = true
})

local lspconfig = require('lspconfig')
local lsp_defaults = lspconfig.util.default_config

lspconfig.sumneko_lua.setup({
    single_file_support = true,
    flags = {
        debounce_text_changes = 150,
    },
})

vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function()
        local bufmap = function(mode, lhs, rhs)
            local opts = { buffer = true }
            vim.keymap.set(mode, lhs, rhs, opts)
        end

        -- Displays hover information about the symbol under the cursor
        bufmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>')

        -- Jump to the definition
        bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>')

        -- Jump to declaration
        bufmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>')

        -- Lists all the implementations for the symbol under the cursor
        bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>')

        -- Jumps to the definition of the type symbol
        bufmap('n', 'go', '<cmd>lua vim.lsp.buf.type_definition()<cr>')

        -- Lists all the references
        bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>')

        -- Displays a function's signature information
        bufmap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<cr>')

        -- Renames all references to the symbol under the cursor
        bufmap('n', '<F2>', '<cmd>lua vim.lsp.buf.rename()<cr>')

        -- Selects a code action available at the current cursor position
        bufmap('n', '<F4>', '<cmd>lua vim.lsp.buf.code_action()<cr>')
        bufmap('x', '<F4>', '<cmd>lua vim.lsp.buf.range_code_action()<cr>')

        -- Show diagnostics in a floating window
        bufmap('n', 'gl', '<cmd>lua vim.diagnostic.open_float()<cr>')

        -- Move to the previous diagnostic
        bufmap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<cr>')

        -- Move to the next diagnostic
        bufmap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<cr>')
    end
})

lsp_defaults.capabilities = vim.tbl_deep_extend(
    'force',
    lsp_defaults.capabilities,
    require('cmp_nvim_lsp').default_capabilities()
)


require('luasnip.loaders.from_vscode').lazy_load()

local cmp = require('cmp')
local luasnip = require('luasnip')
local lspkind = require('lspkind')

local select_opts = { behavior = cmp.SelectBehavior.Select }

cmp.setup({
    completion = { completeopt = 'menu,menuone,noinsert' },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end
    },
    sources = {
        { name = 'path' },
        { name = 'nvim_lsp', keyword_length = 3 },
        { name = 'buffer', keyword_length = 3 },
        { name = 'luasnip', keyword_length = 2 },
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered()
    },
    -- formatting = {
    --   fields = {'menu', 'abbr', 'kind'},
    --   format = function(entry, item)
    --     local menu_icon = {
    --       nvim_lsp = 'λ',
    --       luasnip = '⋗',
    --       buffer = 'Ω',
    --       path = '🖫',
    --     }

    --     item.menu = menu_icon[entry.source.name]
    --     return item
    --   end,
    -- },
    formatting = {
        format = function(entry, vim_item)
            if vim.tbl_contains({ 'path' }, entry.source.name) then
                local icon, hl_group = require('nvim-web-devicons').get_icon(entry:get_completion_item().label)
                if icon then
                    vim_item.kind = icon
                    vim_item.kind_hl_group = hl_group
                    return vim_item
                end
            end
            return lspkind.cmp_format({ with_text = false })(entry, vim_item)
        end
    },
    mapping = {
        ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
        ['<Down>'] = cmp.mapping.select_next_item(select_opts),

        ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
        ['<C-n>'] = cmp.mapping.select_next_item(select_opts),

        ['<C-u>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),

        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),

        ['<C-d>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(1) then
                luasnip.jump(1)
            else
                fallback()
            end
        end, { 'i', 's' }),

        ['<C-b>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { 'i', 's' }),

        ['<Tab>'] = cmp.mapping(function(fallback)
            local col = vim.fn.col('.') - 1

            if cmp.visible() then
                cmp.select_next_item(select_opts)
            elseif col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
                fallback()
            else
                cmp.complete()
            end
        end, { 'i', 's' }),

        ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item(select_opts)
            else
                fallback()
            end
        end, { 'i', 's' }),
    },
})


if not pcall(require, "Comment") then
    return
end

require("Comment").setup {

    -- LHS of operator-pending mapping in NORMAL + VISUAL mode
    opleader = {
        -- line-comment keymap
        line = "gc",
        -- block-comment keymap
        block = "gb",
    },

    -- Create basic (operator-pending) and extended mappings for NORMAL + VISUAL mode
    mappings = {

        -- operator-pending mapping
        -- Includes:
        --  `gcc`               -> line-comment  the current line
        --  `gcb`               -> block-comment the current line
        --  `gc[count]{motion}` -> line-comment  the region contained in {motion}
        --  `gb[count]{motion}` -> block-comment the region contained in {motion}
        basic = true,

        -- extra mapping
        -- Includes `gco`, `gcO`, `gcA`
        extra = true,
    },

    -- LHS of toggle mapping in NORMAL + VISUAL mode
    toggler = {
        -- line-comment keymap
        --  Makes sense to be related to your opleader.line
        line = "gcc",

        -- block-comment keymap
        --  Make sense to be related to your opleader.block
        block = "gbc",
    },

    -- Pre-hook, called before commenting the line
    --    Can be used to determine the commentstring value
    pre_hook = nil,

    -- Post-hook, called after commenting is done
    --    Can be used to alter any formatting / newlines / etc. after commenting
    post_hook = nil,

    -- Can be used to ignore certain lines when doing linewise motions.
    --    Can be string (lua regex)
    --    Or function (that returns lua regex)
    ignore = nil,
}

local comment_ft = require "Comment.ft"
comment_ft.set("lua", { "--%s", "--[[%s]]" })

local rt = require("rust-tools")

rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})

require("mason").setup()

vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.formatting_sync()]]

require('matchparen').setup({
    on_startup = true,
    hl_group = 'MatchParen',
    augroup_name = 'matchparen',
})

local status_ok, npairs = pcall(require, "nvim-autopairs")
if not status_ok then
    return
end

-- change default fast_wrap
npairs.setup({
    check_ts = true,
    diable_filetype = { "TelescopePrompt", "spectre_panel" },
    fast_wrap = {
        map = '<M-e>',
        chars = { '{', '[', '(', '"', "'" },
        pattern = [=[[%'%"%)%>%]%)%}%,]]=],
        offset = 0,
        end_key = '$',
        keys = 'qwertyuiopzxcvbnmasdfghjkl',
        check_comma = true,
        highlight = 'PmenuSel',
        highlight_grey = 'LineNr'
    },
})

local cmp_autopairs = require("nvim-autopairs.completion.cmp")
local cmp_status_ok, cmp = pcall(require, "cmp")

if not cmp_status_ok then
    return
end
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })


require("go").setup({
    go = "go", -- go command, can be go[default] or go1.18beta1
    goimport = "gopls", -- goimport command, can be gopls[default] or goimport
    fillstruct = "gopls", -- can be nil (use fillstruct, slower) and gopls
    gofmt = "gofumpt", -- gofmt cmd,
    max_line_len = 120, -- max line length in goline format
    tag_transform = false, -- tag_transfer  check gomodifytags for details
    test_template = "", -- default to testify if not set; g:go_nvim_tests_template  check gotests for details
    test_template_dir = "", -- default to nil if not set; g:go_nvim_tests_template_dir  check gotests for details
    comment_placeholder = "", -- comment_placeholder your cool placeholder e.g. ﳑ       
    -- icons = { breakpoint = icons.ui.Yoga, currentpos = icons.ui.RunningMan },
    verbose = false, -- output loginf in messages
    lsp_cfg = true, -- true: use non-default gopls setup specified in go/lsp.lua
    -- false: do nothing
    -- if lsp_cfg is a table, merge table with with non-default gopls setup in go/lsp.lua, e.g.
    --   lsp_cfg = {settings={gopls={matcher='CaseInsensitive', ['local'] = 'your_local_module_path', gofumpt = true }}}
    lsp_gofumpt = false, -- true: set default gofmt in gopls format to gofumpt
    lsp_diag_underline = false,
    -- lsp_on_attach = function(client, bufnr)
    --     local utils = require("core.plugins.lsp.utils")
    --     utils.custom_lsp_attach(client, bufnr)
    --     local wk = require("which-key")
    --     local default_options = { silent = true }
    --     wk.register({
    --         c = {
    --             name = "Coding",
    --             a = { "<cmd>GoCodeAction<cr>", "Code action" },
    --             e = { "<cmd>GoIfErr<cr>", "Add if err" },
    --             h = {
    --                 name = "Helper",
    --                 a = { "<cmd>GoAddTag<cr>", "Add tags to struct" },
    --                 r = { "<cmd>GoRMTag<cr>", "Remove tags to struct" },
    --                 c = { "<cmd>GoCoverage<cr>", "Test coverage" },
    --                 g = { "<cmd>lua require('go.comment').gen()<cr>", "Generate comment" },
    --                 v = { "<cmd>GoVet<cr>", "Go vet" },
    --                 t = { "<cmd>GoModTidy<cr>", "Go mod tidy" },
    --                 i = { "<cmd>GoModInit<cr>", "Go mod init" },
    --             },
    --             i = { "<cmd>GoToggleInlay<cr>", "Toggle inlay" },
    --             l = { "<cmd>GoLint<cr>", "Run linter" },
    --             o = { "<cmd>GoPkgOutline<cr>", "Outline" },
    --             r = { "<cmd>GoRun<cr>", "Run" },
    --             s = { "<cmd>GoFillStruct<cr>", "Autofill struct" },
    --             t = {
    --                 name = "Tests",
    --                 r = { "<cmd>GoTest<cr>", "Run tests" },
    --                 a = { "<cmd>GoAlt!<cr>", "Open alt file" },
    --                 s = { "<cmd>GoAltS!<cr>", "Open alt file in split" },
    --                 v = { "<cmd>GoAltV!<cr>", "Open alt file in vertical split" },
    --                 u = { "<cmd>GoTestFunc<cr>", "Run test for current func" },
    --                 f = { "<cmd>GoTestFile<cr>", "Run test for current file" },
    --             },
    --             x = {
    --                 name = "Code Lens",
    --                 l = { "<cmd>GoCodeLenAct<cr>", "Toggle Lens" },
    --                 a = { "<cmd>GoCodeAction<cr>", "Code Action" },
    --             },
    --         },
    --     }, { prefix = "<leader>", mode = "n", default_options })
    --     wk.register({
    --         c = {
    --             -- name = "Coding",
    --             j = { "<cmd>'<,'>GoJson2Struct<cr>", "Json to struct" },
    --         },
    --     }, { prefix = "<leader>", mode = "v", default_options })
    -- end, -- nil: use on_attach function defined in go/lsp.lua,
    --      when lsp_cfg is true
    -- if lsp_on_attach is a function: use this function as on_attach function for gopls
    lsp_codelens = true, -- set to false to disable codelens, true by default
    lsp_keymaps = false, -- set to false to disable gopls/lsp keymap
    lsp_diag_hdlr = true, -- hook lsp diag handler
    -- lsp_diag_virtual_text = { space = 0, prefix = icons.arrows.Diamond }, -- virtual text setup
    lsp_diag_signs = true,
    lsp_diag_update_in_insert = true,
    lsp_document_formatting = true,
    -- set to true: use gopls to format
    -- false if you want to use other formatter tool(e.g. efm, nulls)
    lsp_inlay_hints = {
        enable = true,
        -- Only show inlay hints for the current line
        only_current_line = false,
        -- Event which triggers a refersh of the inlay hints.
        -- You can make this "CursorMoved" or "CursorMoved,CursorMovedI" but
        -- not that this may cause higher CPU usage.
        -- This option is only respected when only_current_line and
        -- autoSetHints both are true.
        only_current_line_autocmd = "CursorHold",
        -- whether to show variable name before type hints with the inlay hints or not
        -- default: false
        show_variable_name = true,
        -- prefix for parameter hints
        parameter_hints_prefix = " ",
        show_parameter_hints = true,
        -- prefix for all the other hints (type, chaining)
        other_hints_prefix = "=> ",
        -- whether to align to the length of the longest line in the file
        max_len_align = false,
        -- padding from the left if max_len_align is true
        max_len_align_padding = 1,
        -- whether to align to the extreme right or not
        right_align = false,
        -- padding from the right if right_align is true
        right_align_padding = 6,
        -- The color of the hints
        highlight = "Comment",
    },
    gopls_cmd = nil, -- if you need to specify gopls path and cmd, e.g {"/home/user/lsp/gopls", "-logfile","/var/log/gopls.log" }
    gopls_remote_auto = true, -- add -remote=auto to gopls
    gocoverage_sign = "█",
    dap_debug = false, -- set to false to disable dap
    dap_debug_keymap = false, -- true: use keymap for debugger defined in go/dap.lua
    -- false: do not use keymap in go/dap.lua.  you must define your own.
    dap_debug_gui = false, -- set to true to enable dap gui, highly recommended
    dap_debug_vt = false, -- set to true to enable dap virtual text
    build_tags = "", -- set default build tags
    textobjects = true, -- enable default text jobects through treesittter-text-objects
    test_runner = "go", -- richgo, go test, richgo, dlv, ginkgo
    run_in_floaterm = false, -- set to true to run in float window.
    -- float term recommended if you use richgo/ginkgo with terminal color
    luasnip = true,
})
