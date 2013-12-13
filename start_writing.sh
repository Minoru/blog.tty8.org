#!/bin/sh

# Author: Alexander Batischev <eual.jp@gmail.com>
#
# This script fires up tmux with my writing setup

tmux has-session -t debiania || (
    # session doesn't exist, let's create one

    # first window is for editing
    tmux new-session -d -s debiania 'vim'
    tmux rename-window -t debiania:0 vim

    # none of the windows need activity monitoring, so I disable it
    tmux set-option -t debiania:preview -g monitor-activity off

    # second window is for version control
    tmux new-window -d -n git -t debiania

    # third window runs "debiania preview"
    tmux new-window -d -n preview -t debiania 'cabal run preview'
)

tmux attach -t debiania

