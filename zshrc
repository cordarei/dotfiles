#Base Z-shell configuration file
#
# This file is my base z-shell config file; it should be symlinked to
# ~/.zshrc. It serves to initialize the system environment variables
# etc. and then calls out to other scripts. Most functionality is
# provided by oh-my-zsh, with some personal conveniences which are more
# appropriate separated provided by other scripts.

# Initialize XDG env vars
[ $XDG_DATA_HOME ] || export XDG_DATA_HOME="$HOME/.local/share"
[ $XDG_CONFIG_HOME ] || export XDG_CONFIG_HOME="$HOME/.config"

export DOTFILES_DIR=$XDG_CONFIG_HOME/dotfiles


# Add local install dirs to path
[[ -d $HOME/.local/bin ]] && export PATH=$HOME/.local/bin:$PATH
[[ -d $HOME/.local/man ]] && export MANPATH=$HOME/.local/man:$MANPATH


# Source specific zshrc scripts:
for zshrc in zshrc.oh-my-zsh; do
    source $DOTFILES_DIR/$zshrc
done


# Define custom aliases (too specific to go into oh-my-zsh)
alias tm='~/Development/cordarei-dotfiles/bin/start-tmux.sh'


# Initialize pythonbrew if installed
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc

