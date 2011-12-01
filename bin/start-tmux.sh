#!/bin/sh

SESSION=$1
if [ -z "$1" ]; then
    SESSION=$USER
fi

if tmux has-session -t $SESSION >/dev/null 2>/dev/null; then
    tmux attach -t $SESSION
    exit 0;
fi

tmux new-session -s $SESSION
