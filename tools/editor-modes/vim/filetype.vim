" If ~/.vim/filetype.vim does not exist, install this file there.
" Otherwise, hand merge it with the existing ~/.vim/filetype.vim.

if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
  au! BufRead,BufNewFile *.nc   setfiletype nesc
augroup END

