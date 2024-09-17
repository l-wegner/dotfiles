require "nvchad.options"

-- add yours here!

local o = vim.o
-- o.cursorlineopt ='both' -- to enable cursorline!
 
o.foldmethod = "expr"
o.foldexpr = "v:lua.vim.treesitter.foldexpr()"

o.scrolloff = 10
o.cursorline = true


local api = vim.api


api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking text",
  group = api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
