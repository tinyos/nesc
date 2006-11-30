" Install this file to ~/.vim/syntax/nesc.vim.

runtime! syntax/c.vim
let b:current_syntax = "nesc"

syn keyword     cStatement      abstract as async atomic call command
syn keyword     cStatement      components configuration event generic
syn keyword     cStatement      implementation includes interface module
syn keyword     cStatement      new norace post provides signal task unique
syn keyword     cStatement      uniqueCount uses

syn keyword     cType           result_t error_t

syn keyword     cConstant       SUCCESS FAIL TRUE FALSE

