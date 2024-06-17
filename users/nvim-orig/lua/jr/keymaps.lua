local builtin = require('telescope.builtin')
-- vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
vim.keymap.set('n', '<leader>bb', builtin.buffers, {})


local wk = require("which-key")

local fileutils = {}

function fileutils.find_files_in_dir(opts)
    local buffer_path = vim.fn.expand('%:p:h')
    print(buffer_path)
    if buffer_path == nil then
        buffer_path = "~"
    end
    -- local rev_buffer_path = buffer_path:reverse()
    -- local last_index = rev_buffer_path:find("/")
    -- if last_index ~= nil then
    --     local rev_buffer_dir = rev_buffer_path:sub(last_index)
    --     local buffer_dir = rev_buffer_dir:reverse()
    builtin.find_files({ search_dirs = { buffer_path } })
    -- else
    -- builtin.find_files({ search_dirs = { "~" } })
    -- end
end

vim.keymap.set('n', '<leader>fs', fileutils.find_files_in_dir, {})


wk.register({
    f = {
        name = "files",
        f = { "<cmd>Telescope find_files<cr>", "Find Files" },
        e = { "<cmd>Telescope file_browser<cr>", "File Browser" },
    },
    b = {
        name = "files",
        b = { "<cmd>Telescope buffers<cr>", "Buffers" },
    },
    t = {
        name = "terminal",
        h = { "<cmd>ToggleTerm direction=horizontal<cr>", "Terminal Horizontal" },
    },
    g = {
        name = "git",
        g = { "<cmd>LazyGit<cr>", "LazyGit" },
    },
    l = {
        name = "lsp",
        a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
        d = { "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>", "Buffer Diagnostics" },
        w = { "<cmd>Telescope diagnostics<cr>", "Diagnostics" },
        -- f = { require("lvim.lsp.utils").format, "Format" },
        i = { "<cmd>LspInfo<cr>", "Info" },
        I = { "<cmd>Mason<cr>", "Mason Info" },
        j = {
            vim.diagnostic.goto_next,
            "Next Diagnostic",
        },
        k = {
            vim.diagnostic.goto_prev,
            "Prev Diagnostic",
        },
        l = { vim.lsp.codelens.run, "CodeLens Action" },
        q = { vim.diagnostic.setloclist, "Quickfix" },
        r = { vim.lsp.buf.rename, "Rename" },
        s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
        S = {
            "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
            "Workspace Symbols",
        },
        e = { "<cmd>Telescope quickfix<cr>", "Telescope Quickfix" },
    },
    s = {
        name = "Search",
        b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
        c = { "<cmd>Telescope colorscheme<cr>", "Colorscheme" },
        f = { "<cmd>Telescope find_files<cr>", "Find File" },
        h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
        H = { "<cmd>Telescope highlights<cr>", "Find highlight groups" },
        M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
        r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
        R = { "<cmd>Telescope registers<cr>", "Registers" },
        t = { "<cmd>Telescope live_grep<cr>", "Text" },
        k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
        C = { "<cmd>Telescope commands<cr>", "Commands" },
        p = {
            "<cmd>lua require('telescope.builtin').colorscheme({enable_preview = true})<cr>",
            "Colorscheme with Preview",
        },
    },
    w = {
        name = "window",
        s = { "<cmd>hsplit<cr>", "Split Horizontal" },
        v = { "<cmd>vsplit<cr>", "Split Vertical" },
        c = { "<cmd>close<cr>", "Close Split" },
    },
}, { prefix = "<leader>" })

vim.api.nvim_set_keymap(
    "n",
    "<space>fe",
    ":Telescope file_browser",
    { noremap = true }
)

vim.keymap.set('n', '<C-h>', '<C-w>h', {})
vim.keymap.set('n', '<C-j>', '<C-w>j', {})
vim.keymap.set('n', '<C-k>', '<C-w>k', {})
vim.keymap.set('n', '<C-l>', '<C-w>l', {})

vim.keymap.set('t', '<C-h>', '<C-\\><C-N><C-w>h', {})
vim.keymap.set('t', '<C-j>', '<C-\\><C-N><C-w>j', {})
vim.keymap.set('t', '<C-k>', '<C-\\><C-N><C-w>k', {})
vim.keymap.set('t', '<C-l>', '<C-\\><C-N><C-w>l', {})

vim.keymap.set('n', '<C-Up>', ':resize -2<CR>', {})
vim.keymap.set('n', '<C-Down>', ':resize +2<CR>', {})
vim.keymap.set('n', '<C-Left>', ':vertical resize -2<CR>', {})
vim.keymap.set('n', '<C-Right>', ':vertical resize +2<CR>', {})

vim.keymap.set('n', '<A-j>', ':m .+1<CR>==', {})
vim.keymap.set('n', '<A-k>', ':m .-2<CR>==', {})
