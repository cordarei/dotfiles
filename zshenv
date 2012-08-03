#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Joseph Irwin <joseph.irwin.gt@gmail.com>
#


# Paths
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

# XDG directories
#get basic dirs or use defaults
XDG_DATA_HOME=${XDG_DATA_HOME:-"$HOME/.local/share"}
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME/.config"}
XDG_CACHE_HOME=${XDG_CACHE_HOME:-"$HOME/.cache"}
#source user dir defs
if [[ -f "$XDG_CONFIG_HOME/user-dirs.dirs" ]]; then
    source "$XDG_CONFIG_HOME/user-dirs.dirs"
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
fi

# Set the path to prezto
export PREZTO="$XDG_CONFIG_HOME/dotfiles/prezto"

datadir="$XDG_DATA_HOME"

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that info searches for manuals.
infopath=(
  $datadir/info
  ${datadir%/share}/info
  /usr/local/share/info
  /usr/share/info
  $infopath
)

# Set the list of directories that man searches for manuals.
manpath=(
  $datadir/man
  ${datadir%/share}/man
  /usr/local/share/man
  /usr/share/man
  $manpath
)

for path_file in /etc/manpaths.d/*(.N); do
  manpath+=($(<$path_file))
done
unset path_file

# Set the list of directories that Zsh searches for programs.
path=(
  ${datadir%/share}/bin
  /usr/local/{bin,sbin}
  /usr/{bin,sbin}
  /{bin,sbin}
  $path
)

for path_file in /etc/paths.d/*(.N); do
  path+=($(<$path_file))
done
unset path_file

unset datadir


# Fix VTE/Terminal misfeature
if [[ "$COLORTERM" == "Terminal" ]] && [[ "$TERM" == "xterm" ]]; then
    TERM="xterm-256color"
fi

# Language
if [[ -z "$LANG" ]]; then
  eval "$(locale)"
fi

# Editors
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

# Browser (Default)
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

# Less

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-i -M -R -S -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

# Prevent Ubuntu from calling compinit in /etc/zsh/zshenv
skip_global_compinit=1 
