#Base Z-shell configuration file
#
# This file is my base z-shell config file; it should be symlinked to
# ~/.zshrc. It serves to initialize the system environment variables
# etc. and then calls out to other scripts. Most functionality is
# provided by oh-my-zsh, with some personal conveniences which are more
# appropriate separated provided by other scripts.


# Initialize XDG env vars
function __init_xdg_vars() {
    #get basic dirs or use defaults
    XDG_DATA_HOME=${XDG_DATA_HOME:-"$HOME/.local/share"}
    XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME/.config"}
    XDG_CACHE_HOME=${XDG_CACHE_HOME:-"$HOME/.cache"}
    #source user dir defs
    [[ -f "$XDG_CONFIG_HOME/user-dirs.dirs" ]] && source "$XDG_CONFIG_HOME/user-dirs.dirs"
    export \
        XDG_DATA_HOME \
        XDG_CONFIG_HOME \
        XDG_CACHE_HOME \
        XDG_DESKTOP_DIR \
        XDG_DOWNLOAD_DIR \
        XDG_TEMPLATES_DIR \
        XDG_PUBLICSHARE_DIR \
        XDG_DOCUMENTS_DIR \
        XDG_MUSIC_DIR \
        XDG_PICTURES_DIR \
        XDG_VIDEOS_DIR
}
__init_xdg_vars

export DOTFILES_DIR=$XDG_CONFIG_HOME/dotfiles


# Fix VTE/Terminal misfeature
if [[ "$COLORTERM" == "Terminal" ]] && [[ "$TERM" == "xterm" ]]; then
    TERM="xterm-256color"
fi

# Source specific zshrc scripts:
for script in $DOTFILES_DIR/zshrc.*; do
    source $script
done


# Define function to start tmux
function tm() {
    local session=$1
    [[ -n "$session" ]] || session=$USER

    if tmux has-session -t $session >/dev/null 2>/dev/null; then
        tmux attach -t $session
    else
        tmux new-session -s $session
    fi
}

# Define function to run pip in a temporary directory
function pip() {
    local tmp=$(mktemp -d)
    ( cd $tmp; command pip "$@" )
    rm -rf $tmp
}


# Initialize pythonbrew if installed
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc

