-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

_G._packer = _G._packer or {}
_G._packer.inside_compile = true

local time
local profile_info
local should_profile = false
if should_profile then
  local hrtime = vim.loop.hrtime
  profile_info = {}
  time = function(chunk, start)
    if start then
      profile_info[chunk] = hrtime()
    else
      profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
    end
  end
else
  time = function(chunk, start) end
end

local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end
  if threshold then
    table.insert(results, '(Only showing plugins that took longer than ' .. threshold .. ' ms ' .. 'to load)')
  end

  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/cipher/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/cipher/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/cipher/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/cipher/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/cipher/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["FixCursorHold.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/FixCursorHold.nvim",
    url = "https://github.com/antoinemadec/FixCursorHold.nvim"
  },
  NrrwRgn = {
    commands = { "NarrowRegion", "NarrowWindow" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/NrrwRgn",
    url = "https://github.com/chrisbra/NrrwRgn"
  },
  ["dial.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/dial.nvim",
    url = "https://github.com/monaqa/dial.nvim"
  },
  ["fidget.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/fidget.nvim",
    url = "https://github.com/j-hui/fidget.nvim"
  },
  firenvim = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/firenvim",
    url = "https://github.com/glacambre/firenvim"
  },
  ["gundo.vim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/gundo.vim",
    url = "https://github.com/sjl/gundo.vim"
  },
  ["haunted.vim"] = {
    commands = { "Haunt" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/haunted.vim",
    url = "https://github.com/tweekmonster/haunted.vim"
  },
  ["lsp-status.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/lsp-status.nvim",
    url = "https://github.com/wbthomason/lsp-status.nvim"
  },
  ["lsp-trouble.nvim"] = {
    commands = { "Trouble" },
    config = { "\27LJ\2\nU\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\2\14auto_fold\2\17auto_preview\1\nsetup\ftrouble\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/lsp-trouble.nvim",
    url = "https://github.com/folke/lsp-trouble.nvim"
  },
  ["lsp_codelens_extensions.nvim"] = {
    config = { "\27LJ\2\nA\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\24codelens_extensions\frequire\0" },
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/lsp_codelens_extensions.nvim",
    url = "https://github.com/ericpubu/lsp_codelens_extensions.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/lspkind-nvim",
    url = "https://github.com/onsails/lspkind-nvim"
  },
  ["luv-vimdocs"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/luv-vimdocs",
    url = "https://github.com/nanotee/luv-vimdocs"
  },
  ["null-ls.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua",
    url = "https://github.com/norcalli/nvim-colorizer.lua"
  },
  ["nvim-lsp-ts-utils"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-lsp-ts-utils",
    url = "https://github.com/jose-elias-alvarez/nvim-lsp-ts-utils"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-luaref"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-luaref",
    url = "https://github.com/milisims/nvim-luaref"
  },
  ["nvim-neoclip.lua"] = {
    config = { "\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fneoclip\frequire\0" },
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-neoclip.lua",
    url = "https://github.com/AckslD/nvim-neoclip.lua"
  },
  ["nvim-notify"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-notify",
    url = "https://github.com/rcarriga/nvim-notify"
  },
  ["nvim-terminal.lua"] = {
    config = { "\27LJ\2\n6\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\rterminal\frequire\0" },
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-terminal.lua",
    url = "https://github.com/norcalli/nvim-terminal.lua"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/popup.nvim",
    url = "https://github.com/nvim-lua/popup.nvim"
  },
  ["spellrotate.vim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/spellrotate.vim",
    url = "https://github.com/tweekmonster/spellrotate.vim"
  },
  ["tabline.vim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/tabline.vim",
    url = "https://github.com/mkitt/tabline.vim"
  },
  ["telescope-file-browser.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-file-browser.nvim",
    url = "https://github.com/nvim-telescope/telescope-file-browser.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzf-native.nvim"
  },
  ["telescope-fzf-writer.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-fzf-writer.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzf-writer.nvim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzy-native.nvim"
  },
  ["telescope-github.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-github.nvim",
    url = "https://github.com/nvim-telescope/telescope-github.nvim"
  },
  ["telescope-hop.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-hop.nvim",
    url = "https://github.com/nvim-telescope/telescope-hop.nvim"
  },
  ["telescope-packer.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-packer.nvim",
    url = "https://github.com/nvim-telescope/telescope-packer.nvim"
  },
  ["telescope-rs.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-rs.nvim",
    url = "https://github.com/nvim-telescope/telescope-rs.nvim"
  },
  ["telescope-symbols.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-symbols.nvim",
    url = "https://github.com/nvim-telescope/telescope-symbols.nvim"
  },
  ["telescope-ui-select.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope-ui-select.nvim",
    url = "https://github.com/nvim-telescope/telescope-ui-select.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["twilight.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/twilight.nvim",
    url = "https://github.com/folke/twilight.nvim"
  },
  ["vim-boxdraw"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/vim-boxdraw",
    url = "https://github.com/gyim/vim-boxdraw"
  },
  ["vim-inyoface"] = {
    config = { "\27LJ\2\nh\0\0\6\0\6\0\t6\0\0\0009\0\1\0009\0\2\0'\2\3\0'\3\4\0'\4\5\0004\5\0\0B\0\5\1K\0\1\0\28<Plug>(InYoFace_Toggle)\15<leader>cc\6n\20nvim_set_keymap\bapi\bvim\0" },
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/vim-inyoface",
    url = "https://github.com/tjdevries/vim-inyoface"
  },
  ["vim-metarepeat"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/vim-metarepeat",
    url = "https://github.com/haya14busa/vim-metarepeat"
  },
  ["vim-projectionist"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/vim-projectionist",
    url = "https://github.com/tpope/vim-projectionist"
  },
  ["vim-qf"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/vim-qf",
    url = "https://github.com/romainl/vim-qf"
  },
  ["vim-scriptease"] = {
    commands = { "Messages", "Verbose", "Time" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/vim-scriptease",
    url = "https://github.com/tpope/vim-scriptease"
  },
  ["vim-startify"] = {
    commands = { "SLoad", "SSave" },
    config = { "\27LJ\2\n>\0\0\2\0\3\0\0056\0\0\0009\0\1\0+\1\2\0=\1\2\0K\0\1\0!startify_disable_at_vimenter\6g\bvim\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/vim-startify",
    url = "https://github.com/mhinz/vim-startify"
  },
  ["vim-startuptime"] = {
    commands = { "StartupTime" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/opt/vim-startuptime",
    url = "https://github.com/dstein64/vim-startuptime"
  },
  ["zen-mode.nvim"] = {
    loaded = true,
    path = "/Users/cipher/.local/share/nvim/site/pack/packer/start/zen-mode.nvim",
    url = "https://github.com/folke/zen-mode.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: vim-inyoface
time([[Config for vim-inyoface]], true)
try_loadstring("\27LJ\2\nh\0\0\6\0\6\0\t6\0\0\0009\0\1\0009\0\2\0'\2\3\0'\3\4\0'\4\5\0004\5\0\0B\0\5\1K\0\1\0\28<Plug>(InYoFace_Toggle)\15<leader>cc\6n\20nvim_set_keymap\bapi\bvim\0", "config", "vim-inyoface")
time([[Config for vim-inyoface]], false)
-- Config for: nvim-terminal.lua
time([[Config for nvim-terminal.lua]], true)
try_loadstring("\27LJ\2\n6\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\rterminal\frequire\0", "config", "nvim-terminal.lua")
time([[Config for nvim-terminal.lua]], false)
-- Config for: nvim-neoclip.lua
time([[Config for nvim-neoclip.lua]], true)
try_loadstring("\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fneoclip\frequire\0", "config", "nvim-neoclip.lua")
time([[Config for nvim-neoclip.lua]], false)
-- Config for: lsp_codelens_extensions.nvim
time([[Config for lsp_codelens_extensions.nvim]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\24codelens_extensions\frequire\0", "config", "lsp_codelens_extensions.nvim")
time([[Config for lsp_codelens_extensions.nvim]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.api.nvim_create_user_command, 'NarrowRegion', function(cmdargs)
          require('packer.load')({'NrrwRgn'}, { cmd = 'NarrowRegion', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'NrrwRgn'}, { cmd = 'NarrowRegion' }, _G.packer_plugins)
          return vim.fn.getcompletion('NarrowRegion ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'Haunt', function(cmdargs)
          require('packer.load')({'haunted.vim'}, { cmd = 'Haunt', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'haunted.vim'}, { cmd = 'Haunt' }, _G.packer_plugins)
          return vim.fn.getcompletion('Haunt ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'Messages', function(cmdargs)
          require('packer.load')({'vim-scriptease'}, { cmd = 'Messages', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-scriptease'}, { cmd = 'Messages' }, _G.packer_plugins)
          return vim.fn.getcompletion('Messages ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'Trouble', function(cmdargs)
          require('packer.load')({'lsp-trouble.nvim'}, { cmd = 'Trouble', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'lsp-trouble.nvim'}, { cmd = 'Trouble' }, _G.packer_plugins)
          return vim.fn.getcompletion('Trouble ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'Time', function(cmdargs)
          require('packer.load')({'vim-scriptease'}, { cmd = 'Time', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-scriptease'}, { cmd = 'Time' }, _G.packer_plugins)
          return vim.fn.getcompletion('Time ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'SSave', function(cmdargs)
          require('packer.load')({'vim-startify'}, { cmd = 'SSave', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-startify'}, { cmd = 'SSave' }, _G.packer_plugins)
          return vim.fn.getcompletion('SSave ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'NarrowWindow', function(cmdargs)
          require('packer.load')({'NrrwRgn'}, { cmd = 'NarrowWindow', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'NrrwRgn'}, { cmd = 'NarrowWindow' }, _G.packer_plugins)
          return vim.fn.getcompletion('NarrowWindow ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'StartupTime', function(cmdargs)
          require('packer.load')({'vim-startuptime'}, { cmd = 'StartupTime', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-startuptime'}, { cmd = 'StartupTime' }, _G.packer_plugins)
          return vim.fn.getcompletion('StartupTime ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'Verbose', function(cmdargs)
          require('packer.load')({'vim-scriptease'}, { cmd = 'Verbose', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-scriptease'}, { cmd = 'Verbose' }, _G.packer_plugins)
          return vim.fn.getcompletion('Verbose ', 'cmdline')
      end})
pcall(vim.api.nvim_create_user_command, 'SLoad', function(cmdargs)
          require('packer.load')({'vim-startify'}, { cmd = 'SLoad', l1 = cmdargs.line1, l2 = cmdargs.line2, bang = cmdargs.bang, args = cmdargs.args, mods = cmdargs.mods }, _G.packer_plugins)
        end,
        {nargs = '*', range = true, bang = true, complete = function()
          require('packer.load')({'vim-startify'}, { cmd = 'SLoad' }, _G.packer_plugins)
          return vim.fn.getcompletion('SLoad ', 'cmdline')
      end})
time([[Defining lazy-load commands]], false)


_G._packer.inside_compile = false
if _G._packer.needs_bufread == true then
  vim.cmd("doautocmd BufRead")
end
_G._packer.needs_bufread = false

if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
