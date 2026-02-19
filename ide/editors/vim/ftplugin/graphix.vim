" Filetype plugin for Graphix
" Language: Graphix
" Maintainer: Graphix Authors

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal commentstring=//\ %s
setlocal comments=://
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal expandtab
setlocal suffixesadd=.gx

let b:undo_ftplugin = 'setlocal commentstring< comments< shiftwidth< softtabstop< expandtab< suffixesadd<'
