" vim: set tw=72 sw=2 ts=2 et:


" =========================== Initialization ===========================

" Disable vi compatibility
set nocompatible


" ============================= Appearance =============================

syntax on

" Use dark background
set background=dark

" Set colorscheme
colorscheme elflord


" ============================== Behavior ==============================

filetype plugin indent on

" Tab settings
set expandtab
set tabstop=4
set shiftwidth=4
set shiftround     " indent to a multiple of shiftwidth

" Show matching brackets.
set showmatch
set mat=5  " for half a sec

set scrolloff=1
set display+=lastline

" Flexible backspace: allow backspacing over autoindent, line breaks, start of
" insert.
set backspace=indent,eol,start

" Don't force save when changing buffers
set hidden

" File-specific settings
augroup MyAutoCmds
  autocmd!
  autocmd FileType make setlocal noexpandtab
  autocmd FileType gitconfig setlocal noexpandtab
  autocmd BufRead gitconfig setlocal filetype=gitconfig
  autocmd FileType rst setlocal tw=72
  autocmd FileType python setlocal tw=80 fo+=croq
  autocmd FileType zsh,sh,bash setlocal sw=2 ts=2 tw=72
augroup END

set ruler

set incsearch
set hlsearch

" No bell
set noerrorbells visualbell t_vb=
autocmd MyAutoCmds GUIEnter * set visualbell t_vb=

" Don't set the window title
set notitle

" showcmd helps sometimes
set showcmd

" useful settings for file opening menu
set wildmenu
set wildmode=list:longest,full
" Ignore some files for filename completion
set wildignore=*.pyc,*.swp

" get rid of *~ and *.swp files
set nobackup
set nowritebackup
set noswapfile

" ================================ Keys ================================

" Set leader to comma.
let mapleader = ","
let maplocalleader = ","

" Command mode without Shift
nnoremap ; :
vnoremap ; :

" alternative to ESC
inoremap <C-n> <Esc>
inoremap <S-Tab> <Esc>

" quicker save
nnoremap <leader>w :w<cr>
" faster quit
nnoremap <leader>q :q<cr>

"quicker sub shortcuts
nnoremap <leader>s :s/
nnoremap <leader>S :%s/

" unbind annoying default command
map s <Nop>
nmap sw ysiw
nmap sW ysiW
nmap S ds

" quicker(?) keys for buffer navigation
nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
nnoremap <leader>l :b#<cr>
nnoremap <leader>k :BD<cr>
nnoremap <leader>K :BD!<cr>

" tab navigation keys
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ts :tab split<cr>
nnoremap <leader>th :tab help<Space>
nnoremap <leader>tc :tabclose<bar>tabprevious<cr>
nnoremap <leader>tm :tabm<cr>
nnoremap <leader>tml :tabm -1<cr>
nnoremap <leader>tmr :tabm +1<cr>

" quick turn off diff-mode
nnoremap <leader>do :diffoff<cr>
nnoremap <leader>dwo <c-w>o:diffoff<cr>

" repeat command over visual selection
vnoremap . :normal .<cr>

" From sensible.vim (github.com/tpope/vim-sensible)
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

" Other mappings from sensible.vim
inoremap <C-U> <C-G>u<C-U>
nnoremap & :&&<CR>
xnoremap & :&&<CR>
" Make Y consistent with C and D.  See :help Y.
nnoremap Y y$
