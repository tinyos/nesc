" Install this file to ~/.vim/syntax/nesc.vim.

runtime! syntax/c.vim
let b:current_syntax = "nesc"

syn keyword     cStatement      abstract as async atomic call command
syn keyword     cStatement      components configuration event implementation
syn keyword     cStatement      includes interface module norace post provides
syn keyword     cStatement      signal task uses

syn keyword     cType           result_t

syn keyword     cConstant       SUCCESS FAIL TRUE FALSE

