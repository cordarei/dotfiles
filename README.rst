.. vim: set tw=72 filetype=rst:

====================================
My Command-Line Environment
====================================

This repository contains the customizations I use for Vim, Zsh, Git, and
Tmux.

Vim
===

My `.vimrc` is constantly evolving. Here is a list of the plugins I use:

- vim-pathogen http://github.com/tpope/vim-pathogen/
- vim-fugitive http://github.com/tpope/vim-fugitive/
- vim-surround http://github.com/tpope/vim-surround/
- vim-repeat http://github.com/tpope/vim-repeat/
- vim-unimpaired http://github.com/tpope/vim-unimpaired/
- bufkill http://github.com/vim-scripts/bufkill.vim/
- tabular http://github.com/godlygeek/tabular/
- colorchart http://github.com/godlygeek/colorchart/
- vim-colors-solarized http://github.com/altercation/vim-colors-solarized/
- vim-vividchalk http://github.com/tpope/vim-vividchalk/
- vim-distinguished http://github.com/Lokaltog/vim-distinguished/
- project.vim http://github.com/cordarei/project.tar.gz/
- vim-python-pep8-indent http://github.com/cordarei/vim-python-pep8-indent/
- vim-python-syntax http://github.com/cordarei/vim-python-syntax/
- vim-unicycle http://github.com/cordarei/UniCycle/
- vim-cpp (forked from google.vim) http://github.com/cordarei/vim-cpp/

Zsh
===

I use Prezto for my Zsh environment. Prezto was originally forked from
Oh-My-Zsh. I have my own fork with some customizations at
`<http://github.com/cordarei/prezto/>`_\ .

Git
===

My `.gitconfig` defines colors and my preferred editor. It also has some
old aliases which are obsoleted by Prezto’s git module. I have a
git_template directory with hooks for running etags; for more info see
`<http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html>`_\ .

Tmux
====

Tmux is much better than screen; I recommend switching unless you are
simply a screen god.
