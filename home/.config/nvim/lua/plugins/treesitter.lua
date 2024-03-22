return {
    "nvim-treesitter/nvim-treesitter", 
    build = ":TSUpdate",
    config = function ()
        local configs = require("nvim-treesitter.configs")
        configs.setup({
            ensure_installed = { "lua", "c", "go","gomod","haskell", "markdown", "jq", "json", "javascript", "python", "typescript", "rust", "toml","bash", "yaml","xml","zig" },
            highlight = { enable = true},
            indent = {enable = true}
        })
   end
}
