return { 
    "Mofiqul/dracula.nvim", 
    name="dracula", 
    priority = 1000,
    config = function()
        local dracula = require("dracula")
        dracula.setup({
            transparent_bg = true,
            lualine_bg_color = "#44475a",
        })
        -- set color scheme to dracula
        vim.cmd.colorscheme "dracula"
        -- vim.api.nvim_set_hl(0,'Normal', { bg = 'none' })
        -- vim.api.nvim_set_hl(0,'NormalFloat', { bg = 'none' })
    end
}
