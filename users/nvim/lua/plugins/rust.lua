return {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    ft = { 'rust' },
    inlay_hints = {
        auto = true,
        show_parameter_hints = false,
        parameter_hints_prefix = '',
        other_hints_prefix = '',
    },
    server = {
        on_attach = function(client, bufnr)
            -- vim.lsp.inlay_hint.enable(bufnr, true)
            vim.lsp.codelens.refresh({ bufnr = bufnr })

            local il_hints = require('lsp-inlayhints')
            il_hints.on_attach(client, bufnr)
        end,
    },
}
