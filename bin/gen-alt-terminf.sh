#!/bin/sh

TMPFILE=$(mktemp) || exit 1
infocmp xterm-256color | sed -r \
    -e 's/^xterm-256color/xterm/' \
    -e 's/\\E\[\?1049l,/\\E[49m&/' \
    >> $TMPFILE \
    && tic $TMPFILE

rm -f $TMPFILE
