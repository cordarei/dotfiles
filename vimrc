set nocompatible

runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

" colors
syntax on
set background=dark
let g:solarized_termcolors=256
let g:solarized_termtrans=1
colorscheme solarized

" Set leader to comma.
let mapleader = ","

filetype plugin indent on

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround     " indent to a multiple of shiftwidth

set autoindent
set smartindent

" Show matching brackets.
set showmatch
set mat=5  " for half a sec

" Flexible backspace: allow backspacing over autoindent, line breaks, start of
" insert.
set backspace=indent,eol,start

" This sets soft wrapping:
" set wrap linebreak textwidth=0

" quicker save
nmap <leader>w :w<cr>
" faster quit
nmap <leader>q :q<cr>
" edit and reload .vimrc from inside vim
nmap <silent> <leader>ev :e ~/.vim/vimrc<cr>
"nmap <silent> <leader>sv :so $MYVIMRC<cr>
"quicker sub shortcuts
nmap <leader>s :s/
nmap <leader>S :%s/
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

set pastetoggle=<F6> "set key to toggle paste mode
nmap <silent> <F7> :set nowrap!<cr>

" Don't force save when changing buffers
set hidden

autocmd FileType make setlocal noexpandtab
autocmd FileType gitconfig setlocal noexpandtab
autocmd BufRead gitconfig setlocal filetype=gitconfig

set ruler
set incsearch
set hlsearch
nmap <silent> <leader>h :nohlsearch<cr>

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

" alternative to ESC
imap <C-n> <Esc>

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
