.. vim: set tw=72 filetype=rst:

==============================
My Computing Environment
==============================

This repository contains the customizations I use for Emacs, Vim, Zsh, Git, and
Tmux.


Permissions
===========

This repo is a collection of configuration files and scripts put together
largely from code snippets from around the web; to the extent that I authored
these files, I hereby dedicate them to the public domain through the `CC0
dedication`_. See the link or the file COPYING for the full dedication.

As an exception, ``emacs.d/elisp/julia-mode.el`` is NOT written by me; it
is included from the Julia_ project which is released under the MIT license.
Copyright information for submodules may be found at their various
repositories on GitHub.

.. _`CC0 dedication`: http://creativecommons.org/publicdomain/zero/1.0/

.. _Julia: https://github.com/JuliaLang/julia


Emacs
=====

See `<emacs.d/init.el>`_\ . I use Emacs with Evil_\ . I actually prefer
Emacs to GVim now, but I still use vim when I’m in a terminal.

.. _Evil: https://gitorious.org/evil/pages/Home


Vim
===

I use Pathogen_ to load Vim plugins. I include the plugins as git submodules
in the ``vim/bundle`` directory.

.. _Pathogen: https://github.com/tpope/vim-pathogen

Zsh
===

I use Prezto for my Zsh environment. Prezto was originally forked from
Oh-My-Zsh. I have my own fork with some customizations at
`<http://github.com/cordarei/prezto/>`_\ .


Git
===

My ``.gitconfig`` defines colors and my preferred editor. It also has some
old aliases which are obsoleted by Prezto’s git module. I have a
git_template directory with hooks for running etags; for more info see
`<http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html>`_\ .


Tmux
====

Tmux is much better than screen; I recommend switching unless you are
simply a screen god.
