"
" LaTeX config
"

let g:vimtex_enabled = 1
let g:vimtex_compiler_enabled = 1
let g:tex_flavor = 'latex'
let g:vimtex_view_mathod='zathura'
let g:vimtex_quickfix_mode=0
let g:tex_conceal='abdmg'

augroup fold_tex
    autocmd!
    autocmd FileType tex setlocal foldmethod=manual
    autocmd FileType tex setlocal spell spelllang=ru,en

augroup END
" let g:vimtex_compiler_progname = 'nvr'
