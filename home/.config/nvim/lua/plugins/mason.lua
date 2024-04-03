return {
--     {
--         'simrat39/rust-tools.nvim',
--         config = function()
--             local rt = require('rust-tools')
--
--             rt.setup({
--
--                 server = {
--                     capabilities = require('cmp_nvim_lsp').default_capabilities(),
--                     on_attach = function(_, bufnr)
--                         vim.keymap.set('n', '<leader>k', rt.hover_actions.hover_actions, { buffer = bufnr })
--                         vim.keymap.set('n', '<leader>a', rt.code_action_group.code_action_group, { buffer = bufnr })
--                     end
--                 },
--                 tools = {
--                     hover_actions = {
--                         auto_focus = true,
--                     },
--                 },
--             })
--         end
--     },
    {
        "mrcjkb/rustaceanvim",
        version = "^4",
        ft = { "rust" },
        opts = {
            server = {
                on_attach = function(client, bufnr)
                    vim.keymap.set('n', '<leader>cR', function() vim.cmd.RustLsp("codeAction") end, { buffer = bufnr })
                    vim.keymap.set('n', '<leader>dr', function() vim.cmd.RustLsp("debuggables") end, { buffer = bufnr })
                    vim.keymap.set('n', 'J', function() vim.cmd.RustLsp("joinLines") end, { buffer = bufnr })
                end,
                default_settings = {
                    ["rust_analyzer"] = {
                        cargo = {
                            allFeatures = true,
                            loadOutDirsFromCheck = true,
                            runBuildScripts = true,
                        },
                        checkOnSave = {
                            allFeatures = true,
                            command = "clippy",
                            extraArgs = { "--no-deps" },
                        },
                        procMacro = {
                            enable = true,
                            ignored = {
                                ["async-trait"] = { "async_train" },
                                ["napi-derive"] = { "napi" },
                                ["async-recursion"] = { "async_recursion" },
                            },
                        },
                    },
                },
            }
        },
        config = function(_, opts)
            vim.g.rustaceanvim = vim.tbl_deep_extend("force",
                {},
                opts or {})
        end
    },
    {
        "williamboman/mason.nvim",
    },
     {
       "jay-babu/mason-nvim-dap.nvim",
         config = function()
            require("mason-nvim-dap").setup({
              ensure_installed = {
                 "codelldb",
                 "cpptools",
              }
            })
         end,
     },
    {
        "williamboman/mason-lspconfig.nvim",
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
                    "lua_ls",
                    "tsserver"
                }
            })
        end
    },
    {
        "jay-babu/mason-null-ls.nvim",
        event = { "BufReadPre", "BufNewFile" },
        dependencies = {
            "williamboman/mason.nvim",
            "nvimtools/none-ls.nvim",
        },
        config = function()
            require("mason-null-ls").setup({
                ensure_installed = {
                    "stylua",
                    "jq",
                    "prettier",
                    "eslint_d",
                    "rust-analyzer"
                }
            })
        end,
    },
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                rust_analyzer = {},
            },
            setup = {
                rust_analyzer = function(_, opts)
                    return true
                end,
            },
        },
        config = function()
            local capabilities = require('cmp_nvim_lsp').default_capabilities()


            local opts = {}
            local lspconfig = require("lspconfig")
            lspconfig.lua_ls.setup({
                capabilities = capabilities,
            })
            lspconfig.tsserver.setup({
                capabilities = capabilities,
            })
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
            vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
            vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
        end
    },
    {
        "nvim-neotest/neotest",
        optional = true,
        opts = function(_, opts)
            opts.adapters = opts.adapters or {}
            vim.list_extend(opts.adapters, {
                require("rustaceanvim.neotest"),
            })
        end
    },
}
