#!/bin/sh

SESSION=$1
if [ -z "$1" ]; then
    SESSION=$USER
fi

if tmux has-session -t $SESSION; then
    tmux attach -t $SESSION
fi

tmux new-session -s $SESSION
