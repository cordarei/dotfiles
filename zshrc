# Zshell Configuration
#
# This file should be symlinked to ~/.zshrc


# Prezto Configuration:

# Make sure the zsh cache directory exists.
zsh_cache_dir="$XDG_CACHE_HOME/zsh"
[[ -d "$zsh_cache_dir" ]] || mkdir -p "$zsh_cache_dir"
unset zsh_cache_dir

# Set the key mapping style to 'emacs' or 'vi'.
zstyle ':omz:module:editor' keymap 'emacs'

# Auto convert .... to ../..
zstyle ':omz:module:editor' dot-expansion 'yes'

# Set case-sensitivity for completion, history lookup, etc.
zstyle ':omz:*:*' case-sensitive 'yes'

# Color output (auto set to 'no' on dumb terminals).
zstyle ':omz:*:*' color 'yes'

# Don't auto set the tab and window titles.
zstyle ':omz:module:terminal' auto-title 'no'

# Set the Oh My Zsh modules to load (browse modules).
# The order matters.
zstyle ':omz:load' omodule \
  'environment' \
  'terminal' \
  'editor' \
  'history' \
  'directory' \
  'spectrum' \
  'utility' \
  'completion' \
  'prompt' \
  'git' \
  'cabal'

# Set password file for gen-pass
#zstyle ':omz:plugin:gen-pass' passfile "${XDG_CONFIG_HOME}/passwd"

# Set the prompt theme to load.
# Setting it to 'random' loads a random theme.
# Auto set to 'off' on dumb terminals.
zstyle ':omz:module:prompt' theme 'cordarei'

# Load prezto
source "$PREZTO/init.zsh"


# Utility Functions:

export EVENT_NOEPOLL=1 # work around a bug in tmux/libevent
# Define function to start tmux
function tm {
    local session="${1:-$USER}"

    if tmux has-session -t "$session" >/dev/null 2>&1 ; then
        tmux attach -t "$session"
    else
        tmux new-session -s "$session"
    fi
}

# Define function to run pip in a temporary directory
function pip {
    local tmp=$(mktemp -d)
    ( cd $tmp; command pip "$@" )
    rm -rf $tmp
}

function eglob {
    setopt localoptions extendedglob
    local c=$1
    shift
    $c $~*
}

# Initialize pythonbrew if installed
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc

