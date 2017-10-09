set nocompatible
syntax enable
set clipboard=unnamed
set number
set ts=4
set autoindent
set expandtab
set shiftwidth=4
set cursorline
set showmatch
set smarttab
set backspace=indent,eol,start
set autoread
set ignorecase
set smartcase
set hlsearch
set incsearch
set updatetime=250
set spell spelllang=en_us
let mapleader="\<Space>"

" Bad whitespace highlightion
highlight BadWhitespace ctermbg=red guibg=darkred
match BadWhitespace /\s\+$/

" Movement
noremap j gj
noremap k gk

" folding
set foldmethod=indent
set foldlevel=99

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

" Plugins here
Plugin 'vim-scripts/indentpython.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-surround'

call vundle#end()
filetype plugin indent on

" Theme settings
set background=light
colorscheme solarized

call togglebg#map("<F5>")
set laststatus=2

" Python stuff
au BufNewFile,BufRead *.py,*.pyw
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix |
    \ set encoding=utf-8 |
let python_highlight_all=1
let g:SimpylFold_docstring_preview=1

" Web stuff (html, js, css)
au BufNewFile,BufRead *.js,*.html,*.css
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2 |
