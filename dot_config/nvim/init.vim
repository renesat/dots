"
"-- Base config --"
"

" autocmd BufEnter * silent! lcd %:p:h

augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END

set nocompatible
filetype plugin indent on
syntax on
set encoding=UTF-8

"-- Local leader key -- "
let maplocalleader="\<space>"

"-- Leader key -- "
let mapleader = ","

" -- Mouse -- "
set mouse=a

" -- Russian -- "
" set keymap=russian-jcukenwin
" set iminsert=0
" set imsearch=0
" highlight lCursor guifg=NONE guibg=Cyan

" -- Smarttab -- "
" https://habr.com/ru/post/64224/
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab
set smartindent
set autoindent


" -- Clipboard -- "
set clipboard=unnamedplus

" " if hidden is not set, TextEdit might fail.
set hidden

" " Some servers have issues with backup files, see #649
" set nobackup
" set nowritebackup

" " don't give |ins-completion-menu| messages.
" set shortmess+=c

" " always show signcolumns
" set signcolumn=yes



"
" -- Plugin configs -- "
"
call plug#begin()

  " -- Sudo save -- "
  Plug 'lambdalisue/suda.vim'
 
  " -- Graphviz -- "
  " Plug 'wannesm/wmgraphviz.vim'

  " -- Surround -- "
  Plug 'tpope/vim-surround'

  " -- Lang switch -- "
  " Plug 'lyokha/vim-xkbswitch'
" -- Grammar -- "
  Plug 'rhysd/vim-grammarous'
  
  " -- Nuke -- "
  Plug 'Lenovsky/nuake'

  " -- VimWiki -- "
  Plug 'mattn/calendar-vim'
  Plug 'vimwiki/vimwiki'

  " -- Multiple cursor -- "
  Plug 'terryma/vim-multiple-cursors'

  " -- Snippets -- "
  Plug 'SirVer/ultisnips'

  " -- Complete -- "
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

  " -- Json -- "
  Plug 'elzr/vim-json'

  " -- Start Screen -- "
  Plug 'mhinz/vim-startify'

  " -- Prolog -- "
  Plug 'adimit/prolog.vim'

  " -- C/C++ -- "
  Plug 'deoplete-plugins/deoplete-clang'

  " -- Haskell -- "
  Plug 'eagletmt/ghcmod-vim'

  " -- Air line -- "
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  " -- Theme -- "
  " Plug 'arcticicestudio/nord-vim'
  " Plug 'dracula/vim', { 'as': 'dracula' }
  Plug 'chriskempson/base16-vim'

  " -- Fuzzy finder -- "
  Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
  
  " -- Comment -- "
  Plug 'tpope/vim-commentary'
  
  " -- Auto pair --"
  Plug 'jiangmiao/auto-pairs'
  
  " -- Python -- "
  Plug 'deoplete-plugins/deoplete-jedi'
  Plug 'davidhalter/jedi-vim'

  " -- Markdown -- "
  Plug 'godlygeek/tabular'
  Plug 'plasticboy/vim-markdown'

  " -- Carp -- "
  Plug 'hellerve/carp-vim'
  let g:syntastic_carp_checkers = ['carp']

  " -- Org Mode -- "
  Plug 'jceb/vim-orgmode'
  
  " -- Sentax checker -- "
  Plug 'w0rp/ale'

  " -- Other  -- "
  Plug 'tpope/vim-dispatch'
  Plug 'radenling/vim-dispatch-neovim'

  " -- Clojure -- "
  Plug 'eraserhd/parinfer-rust', {'do': 'cargo build --release'} " Paringer
  Plug 'tpope/vim-fireplace' " REPL
  Plug 'guns/vim-clojure-highlight' " syntax highlighting
  Plug 'guns/vim-clojure-static'

  " -- Git -- "
  Plug 'airblade/vim-gitgutter'
  Plug 'tpope/vim-fugitive'
  
  " -- File tree -- "
  Plug 'scrooloose/nerdtree', {}
  Plug 'jistr/vim-nerdtree-tabs'

  " -- Undo tree -- "
  Plug 'mbbill/undotree'
  
  " -- LaTeX -- "
  Plug 'lervag/vimtex'

  " -- Rmarkdown -- "
  Plug 'vim-pandoc/vim-rmarkdown', { 'do': ':UpdateRemotePlugins' }

  " -- Pandoc -- "
  Plug 'vim-pandoc/vim-pandoc'
  Plug 'vim-pandoc/vim-pandoc-syntax'

  " -- Icons -- "
  Plug 'ryanoasis/vim-devicons'

  " -- GoYo -- "
  Plug 'junegunn/goyo.vim'
  
  " -- Tag bar -- "
  Plug 'majutsushi/tagbar'
call plug#end()

" -- Graphviz -- "
augroup graphviz
    autocmd!
    autocmd! FileType dot nmap <silent> <leader>ll :execute ':! dot -Tpng ' . '%' . ' -o ' . expand('%:t:r') . '.png'<CR>
    autocmd! FileType dot nmap <silent> <leader>lv :execute ':! sxiv ' . expand('%:t:r') . '.png'<CR>
augroup END


" let g:WMGraphviz_dot
" let g:WMGraphviz_viewer = 'sxiv'

" -- Lang switch -- "
let g:XkbSwitchEnabled = 1
let g:XkbSwitchIMappings = ['ru']
let g:XkbSwitchAssistNKeymap = 1    " for commands r and f 
let g:XkbSwitchAssistSKeymap = 1    " for search lines
let g:XkbSwitchDynamicKeymap = 1
let g:XkbSwitchIMappingsTr = { 
      \ 'ru': 
      \ {'<': 'qwertyuiop[]asdfghjkl;''zxcvbnm,.`/'. 
      \ 'QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?~@#$^&|', 
      \ '>': 'йцукенгшщзхъфывапролджэячсмитьбюё.'. 
      \ 'ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,Ё"№;:?/'}
      \ }

" -- Theme -- "
source ~/.config/nvim/config/theme.vim

" -- Fuzzy finder -- "
noremap <leader>b :Denite buffer<CR>
" KEY MAPPINGS
let s:insert_mode_mappings = [
      \ ['jk', '<denite:enter_mode:normal>', 'noremap'],
      \ ['<Tab>', '<denite:move_to_next_line>', 'noremap'],
      \ ['<C-j>', '<denite:move_to_next_line>', 'noremap'],
      \ ['<S-tab>', '<denite:move_to_previous_line>', 'noremap'],
      \ ['<C-k>', '<denite:move_to_previous_line>', 'noremap'],
      \ ['<C-t>', '<denite:do_action:tabopen>', 'noremap'],
      \ ['<C-v>', '<denite:do_action:vsplit>', 'noremap'],
      \ ['<C-s>', '<denite:do_action:split>', 'noremap'],
      \ ['<Esc>', '<denite:enter_mode:normal>', 'noremap'],
      \ ['<C-N>', '<denite:assign_next_matched_text>', 'noremap'],
      \ ['<C-P>', '<denite:assign_previous_matched_text>', 'noremap'],
      \ ['<Up>', '<denite:assign_previous_text>', 'noremap'],
      \ ['<Down>', '<denite:assign_next_text>', 'noremap'],
      \ ['<C-Y>', '<denite:redraw>', 'noremap'],
      \ ]

let s:normal_mode_mappings = [
      \ ["'", '<denite:toggle_select_down>', 'noremap'],
      \ ['<C-n>', '<denite:jump_to_next_source>', 'noremap'],
      \ ['<C-p>', '<denite:jump_to_previous_source>', 'noremap'],
      \ ['<Tab>', '<denite:move_to_next_line>', 'noremap'],
      \ ['<C-j>', '<denite:move_to_next_line>', 'noremap'],
      \ ['<S-tab>', '<denite:move_to_previous_line>', 'noremap'],
      \ ['<C-k>', '<denite:move_to_previous_line>', 'noremap'],
      \ ['gg', '<denite:move_to_first_line>', 'noremap'],
      \ ['<C-t>', '<denite:do_action:tabopen>', 'noremap'],
      \ ['<C-v>', '<denite:do_action:vsplit>', 'noremap'],
      \ ['<C-s>', '<denite:do_action:split>', 'noremap'],
      \ ['q', '<denite:quit>', 'noremap'],
      \ ['r', '<denite:redraw>', 'noremap'],
      \ ]

for s:m in s:insert_mode_mappings
  call denite#custom#map('insert', s:m[0], s:m[1], s:m[2])
endfor
for s:m in s:normal_mode_mappings
  call denite#custom#map('normal', s:m[0], s:m[1], s:m[2])
endfor

unlet s:m s:insert_mode_mappings s:normal_mode_mappings


" -- Tag bar -- "
noremap <leader>w :TagbarToggle<CR>

" -- GoYo -- "
nmap <leader>g :Goyo<CR>

" -- Nuke -- "
nnoremap <leader>q :Nuake<CR>
inoremap <leader>q <C-\><C-n>:Nuake<CR>
tnoremap <leader>q <C-\><C-n>:Nuake<CR>
let g:nuake_position =  'top'

" -- VimWiki -- "
let myVimWiki = {}
let myVimWiki.path = '~/KnowledgeBase/VimWiki'
let myVimWiki.auto_toc = 1
let myVimWiki.auto_diary = 1
let myVimWiki.syntax = 'markdown'
let myVimWiki.ext = '.mdwiki'

let g:vimwiki_folding = 'syntax'

let g:vimwiki_list = [myVimWiki]

" -- Multiple cursor -- "
let g:multi_cursor_select_all_word_key = '<leader>n'

" -- Snippets -- "
source ~/.config/nvim/config/snippet.vim

" -- Complete -- "
source ~/.config/nvim/config/complete.vim

" -- Sessions -- "
source ~/.config/nvim/config/sessions.vim

" -- Start Screen -- "
source ~/.config/nvim/config/startify.vim

" -- Air line -- "
let g:airline_powerline_fonts = 1
let g:airline#extensions#hunks#enabled = 0

" -- Theme -- "
let base16colorspace=256

" -- Markdown -- "
source ~/.config/nvim/config/markdown.vim

" -- File tree -- "
" Toggle
noremap <silent> <leader>t :NERDTreeToggle<CR>

" How can I close vim if the only window left open is a NERDTree?
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" -- Undo tree -- "
nnoremap <leader>ut :UndotreeToggle<cr>

" -- LaTeX -- "
source ~/.config/nvim/config/latex.vim

" -- Pandoc -- "
augroup pandoc_syntax
    au! BufNewFile,BufFilePre,BufRead *.md set filetype=markdown.pandoc
    au! BufNewFile,BufFilePre,BufRead *.rmd set filetype=markdown.pandoc
augroup END

" -- Sentax checker -- "
augroup haskell_lsp
  au!
  au! FileType haskell let b:ale_linters = ['brittany', 'floskell', 'hdevtools', 'hfmt', 'hie', 'hlint', 'stack-build', 'stack-ghc', 'stylish-haskell']
augroup END

