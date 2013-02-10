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
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

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

" neocomplcache
let g:neocomplcache_enable_at_startup = 1

" vimfiler
let g:vimfiler_as_default_explorer = 1
" ================================ Keys ================================

" Set leader to comma.
let mapleader = ","

" Command mode without Shift
nnoremap ; :
vnoremap ; :

" alternative to ESC
imap <C-n> <Esc>
imap <S-Tab> <Esc>

" quicker save
nmap <leader>w :w<cr>
" faster quit
nmap <leader>q :q<cr>

"quicker sub shortcuts
nmap <leader>s :s/
nmap <leader>S :%s/

" shortcuts to enable/disable UniCycle plugin
nmap <leader>u :UniCycleOn<cr>
nmap <leader>U :UniCycleOff<cr>

" Unite shortcuts
nnoremap <C-u><C-f> :Unite file<cr>
nnoremap <C-u><C-b> :Unite buffer<cr>

" VimFiler
nnoremap <leader>f :VimFiler .<cr>

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


"
" reST
" TODO: consider a plugin
"

" Get a character from the user
function! InChr()
  let c = getchar()
  if c =~ '^\d\+$'
    let c = nr2char(c)
  endif
  return c
endfunction

" Transform the current line into a reST section header
function! RstHead(chr, above)
    let c = substitute(a:chr, "&", "\\\\&", "") " fix ampersand
    echomsg c
    "let c = a:chr
    normal! mq"xyy
    let @x = substitute(@x, ".", c, "g")
    let @x = substitute(@x, ".$", "", "")
    if (a:above)
        execute "normal! O\<esc>\"xpj"
    endif
    execute "normal! o\<esc>"
    normal! "xp`q
endfunction

" Get a character from the user and make a header using that character
function! DoRstHead(above)
    let c = InChr()
    call RstHead(c, a:above)
endfunction

" nnoremap <silent> <leader>rh :call DoRstHead(0)<cr>
" nnoremap <silent> <leader>rt :call DoRstHead(1)<cr>

autocmd FileType rst nnoremap <silent> <leader>rh :call DoRstHead(0)<cr>
autocmd FileType rst nnoremap <silent> <leader>rt :call DoRstHead(1)<cr>
