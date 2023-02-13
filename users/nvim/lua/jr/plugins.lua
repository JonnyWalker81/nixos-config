_ = vim.cmd [[packadd packer.nvim]]
_ = vim.cmd [[packadd vimball]]

-- vim.api.nvim_cmd({
--   cmd = "packadd",
--   args = { "packer.vim" },
-- }, {})

local has = function(x)
    return vim.fn.has(x) == 1
end

local executable = function(x)
    return vim.fn.executable(x) == 1
end

local is_mac = has "macunix"
local is_linux = not is_wsl and not is_mac

local max_jobs = nil
if is_mac then
    max_jobs = 32
end


return require("packer").startup {
    function(use)

        local local_use = function(first, second, opts)
            opts = opts or {}

            local plug_path, home
            if second == nil then
                plug_path = first
                home = "cipher"
            else
                plug_path = second
                home = first
            end

            if vim.fn.isdirectory(vim.fn.expand("~/plugins/" .. plug_path)) == 1 then
                opts[1] = "~/plugins/" .. plug_path
            else
                opts[1] = string.format("%s/%s", home, plug_path)
            end

            use(opts)
        end

        local py_use = function(opts)
            if not has "python3" then
                return
            end

            use(opts)
        end
        use "wbthomason/packer.nvim"
        use "neovim/nvim-lspconfig"
        use "wbthomason/lsp-status.nvim"
        use "j-hui/fidget.nvim"
        use {
            "ericpubu/lsp_codelens_extensions.nvim",
            config = function()
                require("codelens_extensions").setup()
            end,
        }
        use "jose-elias-alvarez/null-ls.nvim"
        -- local_use "lsp_extensions.nvim"
        use "onsails/lspkind-nvim"


        use "jose-elias-alvarez/nvim-lsp-ts-utils"

        use {
            "folke/lsp-trouble.nvim",
            cmd = "Trouble",
            config = function()
                -- Can use P to toggle auto movement
                require("trouble").setup {
                    auto_preview = false,
                    auto_fold = true,
                }
            end,
        }

        use "rcarriga/nvim-notify"

        -- TODO: Investigate
        -- use 'jose-elias-alvarez/nvim-lsp-ts-utils'

        use "nvim-lua/popup.nvim"
        use "nvim-lua/plenary.nvim"

        use "nvim-telescope/telescope.nvim"
        use "nvim-telescope/telescope-rs.nvim"
        use "nvim-telescope/telescope-fzf-writer.nvim"
        use "nvim-telescope/telescope-packer.nvim"
        use "nvim-telescope/telescope-fzy-native.nvim"
        use "nvim-telescope/telescope-github.nvim"
        use "nvim-telescope/telescope-symbols.nvim"

        use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
        use { "nvim-telescope/telescope-hop.nvim" }
        use { "nvim-telescope/telescope-file-browser.nvim" }
        use { "nvim-telescope/telescope-ui-select.nvim" }

        use {
            "AckslD/nvim-neoclip.lua",
            config = function()
                require("neoclip").setup()
            end,
        }

        -- TODO: When i'm back w/ some npm stuff, get this working.
        -- elianiva/telescope-npm.nvim

        -- local_use "telescope-hacks.nvim"
        -- local_use "sg.nvim"
        -- local_use "green_light.nvim"


        use {
            "antoinemadec/FixCursorHold.nvim",
            run = function()
                vim.g.curshold_updatime = 1000
            end,
        }

        use "nanotee/luv-vimdocs"
        use "milisims/nvim-luaref"

        -- PRACTICE:
        use {
            "tpope/vim-projectionist", -- STREAM: Alternate file editting and some helpful stuff,
            enable = false,
        }

        -- For narrowing regions of text to look at them alone
        use {
            "chrisbra/NrrwRgn",
            cmd = { "NarrowRegion", "NarrowWindow" },
        }

        -- use {
        --   "luukvbaal/stabilize.nvim",
        --   config = function()
        --     require("stabilize").setup()
        --   end,
        -- }

        use "tweekmonster/spellrotate.vim"
        use "haya14busa/vim-metarepeat" -- Never figured out how to use this, but looks like fun.
        --
        -- VIM EDITOR:

        -- Little know features:
        --   :SSave
        --   :SLoad
        --       These are wrappers for mksession that work great. I never have to use
        --       mksession anymore or worry about where things are saved / loaded from.
        use {
            "mhinz/vim-startify",
            cmd = { "SLoad", "SSave" },
            config = function()
                vim.g.startify_disable_at_vimenter = true
            end,
        }

        -- Better profiling output for startup.
        use {
            "dstein64/vim-startuptime",
            cmd = "StartupTime",
        }

        -- Pretty colors
        use "norcalli/nvim-colorizer.lua"
        use {
            "norcalli/nvim-terminal.lua",
            config = function()
                require("terminal").setup()
            end,
        }

        -- Make comments appear IN YO FACE
        use {
            "tjdevries/vim-inyoface",
            config = function()
                vim.api.nvim_set_keymap("n", "<leader>cc", "<Plug>(InYoFace_Toggle)", {})
            end,
        }

        -- Show only what you're searching for.
        -- STREAM: Could probably make this a bit better. Definitely needs docs
        -- use "tjdevries/fold_search.vim"

        use {
            "tweekmonster/haunted.vim",
            cmd = "Haunt",
        }

        use {
            "tpope/vim-scriptease",
            cmd = {
                "Messages", --view messages in quickfix list
                "Verbose", -- view verbose output in preview window.
                "Time", -- measure how long it takes to run some stuff.
            },
        }

        -- Quickfix enhancements. See :help vim-qf
        use "romainl/vim-qf"

        use {
            "glacambre/firenvim",
            run = function()
                vim.fn["firenvim#install"](0)
            end,
        }

        -- TODO: Eventually statusline should consume this.
        use "mkitt/tabline.vim"

        --use "kyazdani42/nvim-web-devicons"
        -- if is_linux then
        --  use "yamatsum/nvim-web-nonicons"
        -- end

        -- TODO: This would be cool to add back, but it breaks sg.nvim for now.
        -- use "lambdalisue/vim-protocol"

        -- Undo helper
        use "sjl/gundo.vim"

        -- Crazy good box drawing
        use "gyim/vim-boxdraw"

        -- Better increment/decrement
        use "monaqa/dial.nvim"

        --   FOCUSING:
        local use_folke = true
        if use_folke then
            use "folke/zen-mode.nvim"
            use "folke/twilight.nvim"
        else
            use {
                "junegunn/goyo.vim",
                cmd = "Goyo",
                disable = use_folke,
            }

            use {
                "junegunn/limelight.vim",
                after = "goyo.vim",
                disable = use_folke,
            }
        end

        -- Sources
        use "hrsh7th/nvim-cmp"
        -- use "hrsh7th/cmp-cmdline"
        use "hrsh7th/cmp-buffer"
        use "hrsh7th/cmp-path"
        use "hrsh7th/cmp-nvim-lua"
        use "hrsh7th/cmp-nvim-lsp"
        use "hrsh7th/cmp-nvim-lsp-document-symbol"
        use "saadparwaiz1/cmp_luasnip"
        use "tamago324/cmp-zsh"

        -- Comparators
        use "lukas-reineke/cmp-under-comparator"

        -- Completion stuff
        -- local_use "rofl.nvim"

        -- Cool tags based viewer
        --   :Vista  <-- Opens up a really cool sidebar with info about file.
        use { "liuchengxu/vista.vim", cmd = "Vista" }

        -- Find and replace
        use "windwp/nvim-spectre"

        -- Debug adapter protocol
        use "mfussenegger/nvim-dap"
        use "rcarriga/nvim-dap-ui"
        use "theHamsta/nvim-dap-virtual-text"
        use "nvim-telescope/telescope-dap.nvim"

        use "mfussenegger/nvim-dap-python"
        use "jbyuki/one-small-step-for-vimkind"

        -- use {
        --   "rcarriga/vim-ultest",

        --   enable = false,
        --   requires = { "vim-test/vim-test" },
        --   run = ":UpdateRemotePlugins",
        --   config = function()
        --     vim.cmd [[nmap ]t <Plug>(ultest-next-fail)]]
        --     vim.cmd [[nmap [t <Plug>(ultest-prev-fail)]]
        --   end,
        -- }

        -- TREE SITTER:
        use "nvim-treesitter/nvim-treesitter"
        use "nvim-treesitter/playground"
        use "vigoux/architext.nvim"

        -- TODO: YouTube Highlight
        use "danymat/neogen"

        use "nvim-treesitter/nvim-treesitter-textobjects"
        use "JoosepAlviste/nvim-ts-context-commentstring"
        use {
            "mfussenegger/nvim-ts-hint-textobject",
            config = function()
                vim.cmd [[omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>]]
                vim.cmd [[vnoremap <silent> m :lua require('tsht').nodes()<CR>]]
            end,
        }

        -- use {
        --   "romgrk/nvim-treesitter-context",
        --   config = function()
        --     require("treesitter-context.config").setup {
        --       enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
        --     }

        --     -- TODO: Decide on a better highlighting color
        --     -- vim.cmd [[highlight TreesitterContext link NormalFloat]]
        --   end,
        -- }

        -- Grammars
        -- local_use "tree-sitter-lua"
        -- use { "m-novikov/tree-sitter-sql" }
        -- use { "DerekStride/tree-sitter-sql" }
        -- local_use "tree-sitter-sql"

        --
        -- NAVIGATION:
        -- STREAM: Show off edit_alternate.vim
        --use {
        --  "tjdevries/edit_alternate.vim",
        --  config = function()
        --    vim.fn["edit_alternate#rule#add"]("go", function(filename)
        --      if filename:find "_test.go" then
        --        return (filename:gsub("_test%.go", ".go"))
        --      else
        --        return (filename:gsub("%.go", "_test.go"))
        --      end
        --    end)

        --    vim.api.nvim_set_keymap("n", "<leader>ea", "<cmd>EditAlternate<CR>", { silent = true })
        --  end,
        --}

        use "google/vim-searchindex"

        use "tamago324/lir.nvim"
        use "tamago324/lir-git-status.nvim"


        use "godlygeek/tabular" -- Quickly align text by pattern
        use "tpope/vim-repeat" -- Repeat actions better
        use "tpope/vim-abolish" -- Cool things with words!
        use "tpope/vim-characterize"
        use { "tpope/vim-dispatch", cmd = { "Dispatch", "Make" } }

        use "numToStr/Comment.nvim"

        use {
            "AndrewRadev/splitjoin.vim",
            keys = { "gJ", "gS" },
        }

        -- TODO: Check out macvhakann/vim-sandwich at some point
        use "tpope/vim-surround" -- Surround text objects easily

        --
        -- GIT:
        use "TimUntersberger/neogit"

        use "ruifm/gitlinker.nvim"

        -- Sweet message committer
        use "rhysd/committia.vim"
        use "sindrets/diffview.nvim"

        -- Floating windows are awesome :)
        use {
            "rhysd/git-messenger.vim",
            keys = "<Plug>(git-messenger)",
        }

        -- Async signs!
        use "lewis6991/gitsigns.nvim"

        -- Git worktree utility
        use {
            "ThePrimeagen/git-worktree.nvim",
            config = function()
                require("git-worktree").setup {}
            end,
        }

        use { "junegunn/fzf", run = "./install --all" }
        use { "junegunn/fzf.vim" }

        -- Lua
        use {
            "folke/which-key.nvim",
            config = function()
                require("which-key").setup {
                    -- your configuration comes here
                    -- or leave it empty to use the default settings
                    -- refer to the configuration section below
                }
            end
        }

        use 'folke/tokyonight.nvim'

        use { "akinsho/toggleterm.nvim", tag = '*', config = function()
            require("toggleterm").setup()
        end }

        use { "L3MON4D3/LuaSnip", tag = 'v<CurrentMajor>.*' }

        use { 'nvim-tree/nvim-web-devicons' }

        use { 'simrat39/rust-tools.nvim' }

        use 'williamboman/mason.nvim'
        use 'williamboman/mason-lspconfig.nvim'

        use 'ray-x/guihua.lua'

        use 'ray-x/go.nvim'

        use 'kdheepak/lazygit.nvim'

        use 'monkoose/matchparen.nvim'
        use {
            "windwp/nvim-autopairs",
            config = function() require("nvim-autopairs").setup {} end
        }
    end
}
