"" Leader
let mapleader="\<Space>"

"" Plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'morhetz/gruvbox'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'godlygeek/tabular'  " vim-markdown dependency
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'airblade/vim-gitgutter'
Plug 'sgur/vim-editorconfig'
Plug 'alvan/vim-closetag'
Plug 'ziglang/zig.vim'

call plug#end()

"" Interface
set updatetime=250
set mouse=a
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
set background=light
let g:gruvbox_bold = 1
let g:gruvbox_contrast_light = 'hard'
let g:gruvbox_invert_selection = 0
colorscheme gruvbox

"" Spelling
set spelllang=en_us,ru

"" Buffers
set hidden
set autoread

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

"" Autocompletion
set omnifunc=syntaxcomplete#Complete
"" :help completeopt
set completeopt=menuone,preview,noinsert

"" Filetypes
autocmd FileType * setlocal formatoptions-=ro
autocmd BufRead,BufNewFile *.jinja2 setfiletype jinja2
autocmd BufRead,BufNewFile *.css,*.scss,*.js,*.json set shiftwidth=2 softtabstop=2

"" Mappings
nnoremap <leader>f :FZF<cr>
nnoremap <leader>w :w<cr>

"" HTML
let g:closetag_filenames = '*.html,*.jinja2'
