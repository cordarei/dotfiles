" vim: set tw=72:


" =========================== Initialization ===========================

" Disable vi compatibility
set nocompatible

" Load plugins with Pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()


" ============================= Appearance =============================

syntax on

" Use dark background
set background=dark

" Settings for the solarized colorscheme
let g:solarized_termcolors=256
let g:solarized_termtrans=1

" Use solarized colorscheme
colorscheme solarized

" Settings for python syntax highlighting
let g:python_highlight_all=1


" ============================== Behavior ==============================

filetype plugin indent on

" Tab settings
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround     " indent to a multiple of shiftwidth

" Show matching brackets.
set showmatch
set mat=5  " for half a sec

" Flexible backspace: allow backspacing over autoindent, line breaks, start of
" insert.
set backspace=indent,eol,start

" Don't force save when changing buffers
set hidden

" File-specific settings
autocmd FileType make setlocal noexpandtab
autocmd FileType gitconfig setlocal noexpandtab
autocmd BufRead gitconfig setlocal filetype=gitconfig
autocmd FileType rst setlocal tw=72
autocmd FileType python setlocal tw=80 fo+=croq

set ruler

set incsearch
set hlsearch

" No bell
set visualbell
set noerrorbells

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

" enable UniCycle plugin automatically for rst files
autocmd FileType rst UniCycleOn

" ================================ Keys ================================

" Set leader to comma.
let mapleader = ","

" alternative to ESC
imap <C-n> <Esc>

" quicker save
nmap <leader>w :w<cr>
" faster quit
nmap <leader>q :q<cr>

"quicker sub shortcuts
nmap <leader>s :s/
nmap <leader>S :%s/

" shortcuts to enable/disable UniCycle plugin
nmap <leader>u :UniCycleOn
nmap <leader>U :UniCycleOff

" undo shortcut
nmap <C-u> :u<cr>

" unbind annoying default command
nmap s <Nop>

" quicker(?) keys for buffer navigation
nmap <leader>n :bn<cr>
nmap <leader>p :bp<cr>
nmap <leader>l :b#<cr>
nmap <leader>k :BD<cr>

" quick turn off diff-mode
nmap <leader>do :diffoff<cr>
nmap <leader>dwo <c-w>o:diffoff<cr>

" repeat command over visual selection
vnoremap . :normal .<cr>

"set key to toggle paste mode
set pastetoggle=<F6>

" Use F7 to toggle line wrapping
nmap <silent> <F7> :set nowrap!<cr>

" Clear search highlights
nmap <silent> <leader>h :nohlsearch<cr>


" ======================= Functions and Commands =======================

" Convenient command to see the difference between the current buffer and the
" " file it was loaded from, thus the changes you made.
" " Only define it when not defined already.
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif

" call make silently and redraw the screen afterward
function! Make()
    silent make
    redraw!
endfunction

nmap <leader>m :call Make()<cr>
