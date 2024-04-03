return {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    opts = function (_, opts)
        opts.ensure_installed = opts or {}
        vim.list_extend(opts.ensure_installed, { "lua", "c", "go","gomod","haskell", "markdown", "jq", "json", "javascript", "python", "typescript", "rust", "toml","bash", "yaml","xml","zig" })
    end,
    config = function ()
        local configs = require("nvim-treesitter.configs")
        configs.setup({
            highlight = { enable = true},
            indent = {enable = true}
        })
   end
}
