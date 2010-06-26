set nocompatible

" colors
colorscheme ir_black
set background=dark
syntax on

filetype plugin on

set autoindent
set smartindent

set tabstop=4
set shiftwidth=4
set expandtab

autocmd FileType make setlocal noexpandtab

set ruler
set incsearch

" alternative to ESC
imap <C-n> <Esc>
