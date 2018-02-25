"" Leader
let mapleader="\<Space>"

"" Plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'flazz/vim-colorschemes'
Plug 'iCyMind/NeoSolarized'
Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'godlygeek/tabular'  " vim-markdown dependency
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'sheerun/vim-polyglot'
Plug 'airblade/vim-gitgutter'
Plug 'sgur/vim-editorconfig'

call plug#end()

"" Interface
set updatetime=250
set mouse=a
set cursorline
set title
set linebreak
set scrolloff=3
set sidescrolloff=5
set confirm
" Unicode characters for various things
set list
set listchars=tab:»\ ,extends:›,precedes:‹,nbsp:·,trail:·
" Postpone redraw on macro execution. It could be very slow otherwise
set lazyredraw
set termguicolors
colorscheme NeoSolarized
let g:neosolarized_contrast = "high"
let g:neosolarized_bold = 1
let g:neosolarized_underline = 2
let g:neosolarized_italic = 1

"" Spelling
set spelllang=en_us,ru

"" Buffers
set hidden

"" Windows
" Open new split window to the right for vertical
" and below for horizontal. Both for regular windows
" and for netrw.
set splitbelow
set splitright
let g:netrw_altv=1
let g:netrw_alto=1

"" Default Indentation
set tabstop=4
set shiftwidth=4
set expandtab

"" Search
set ignorecase
set smartcase
" No search highlighting while in insert mode
autocmd InsertEnter * :setlocal nohlsearch
autocmd InsertLeave * :setlocal hlsearch
" Mnemonic is C-l in unix shell - clear all
nnoremap <C-l> :nohlsearch<CR><C-l>

"" Session
set nobackup
set undofile

" Markdown
let g:vim_markdown_folding_disabled = 1

"" NetRW
let g:netrw_banner = 0

"" Linting
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'jsx': ['eslint'],
\   'python': ['flake8'],
\}


"" Autocompletion
set omnifunc=syntaxcomplete#Complete
set completeopt=menuone,preview,noinsert,noselect

"" Filetypes
autocmd BufRead,BufNewFile *.jinja2 setfiletype jinja2
