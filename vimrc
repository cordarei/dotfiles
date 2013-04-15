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
let g:vimfiler_enable_auto_cd = 1

" html5.vim: disable unneeded options
let g:html5_event_handler_attributes_complete = 0
let g:html5_rdfa_attributes_complete = 0
let g:html5_microdata_attributes_complete = 0
let g:html5_aria_attributes_complete = 0

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

" Toggle UniCycle plugin
nnoremap <C-u><C-u> :UniCycleToggle<cr>

" Unite shortcuts
nnoremap <C-u><C-f> :Unite file<cr>
nnoremap <C-u><C-b> :Unite buffer<cr>

" VimFiler
nnoremap <leader>f :VimFilerExplorer<cr>

" fugitive maps
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
" make fugitive more convenient with tabs
nnoremap <leader>tgs :tab split<bar>Gstatus<bar>on<cr>
nnoremap <leader>tgd :tab split<bar>Gdiff<cr>
nnoremap <leader>tgc :tab split<bar>execute 'Gcommit'<bar>silent! on<cr>

" unbind annoying default command
nnoremap s <Nop>

" quicker(?) keys for buffer navigation
nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
nnoremap <leader>l :b#<cr>
nnoremap <leader>k :BD<cr>

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

"set key to toggle paste mode
set pastetoggle=<F6>

" Use F7 to toggle line wrapping
nnoremap <silent> <F7> :set nowrap!<cr>

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

" ======================= Functions and Commands =======================

" Convenient command to see the difference between the current buffer
" and the file it was loaded from, thus the changes you made.
command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                \ | wincmd p | diffthis

" call make silently and redraw the screen afterward
command! Make silent make | redraw!

nnoremap <leader>m :Make<cr>

command! Scratch edit ~/.vim/scratch

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
    let savereg = @@
    normal! mqyy
    let @@ = substitute(@@, ".", c, "g")
    let @@ = substitute(@@, ".$", "", "")
    if (a:above)
        execute "normal! O\<esc>pj"
    endif
    execute "normal! o\<esc>"
    normal! p`q
    let @@ = savereg
endfunction

autocmd FileType rst nnoremap <silent> <leader>rh :call RstHead(InChr(), 0)<cr>
autocmd FileType rst nnoremap <silent> <leader>rt :call RstHead(InChr(), 1)<cr>
