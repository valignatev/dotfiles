"" Plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'flazz/vim-colorschemes'
Plug 'iCyMind/NeoSolarized'
Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

call plug#end()

"" General
let mapleader="\<Space>"

"""""""""""""""""""""""""""""""""""""""""""""""
" Movement
"""""""""""""""""""""""""""""""""""""""""""""""
noremap j gj
noremap k gk

" Emacs-like bindings in the command line from `:h emacs-keys`
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Del>
cnoremap <C-e>  <End>
cnoremap <M-b>  <S-Left>
cnoremap <M-f>  <S-Right>
cnoremap <M-d>  <S-right><Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-g> <C-c>

"" Tabs
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

"" Backup, Swap and Undo
set undofile " Persistent Undo
set directory=$HOME/.vim/swap,/tmp
set backupdir=$HOME/.vim/backup,/tmp
set undodir=~/.vim/undo,/tmp

"" Colors
set cursorline
set hlsearch
set list
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:· " Unicode characters for various things
set termguicolors
let g:neosolarized_contrast = "high"
let g:neosolarized_bold = 1
let g:neosolarized_underline = 2
let g:neosolarized_italic = 1
colorscheme NeoSolarized

"" NetRW
let g:netrw_banner = 0

"" Autocompletion
set omnifunc=syntaxcomplete#Complete
set completeopt=menuone,preview,noinsert,noselect

"" Linting
let g:ale_sign_column_always = 1
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'jsx': ['eslint'],
\   'python': ['flake8'],
\}
