===========
Shell Notes
===========

Notes on shell command line usage.


Examples
--------

These examples are most likely to be run from `zsh`; however, they
should work with at most minimal modification under `bash` or `/bin/sh`.

To randomly select `N` files to do something with::

    find -name '*sentences' | sort -R | head | while read file; do echo '++++' $file; cat $file; echo '----'; cat "${file%sentences}semafor"; done >analyze_semafor_output

To re-format a list of counts where the count is at the end of the line
so that the count comes at the beginning in order to sort them with
`sort`::

    cat counts | sed 's/^\(.*\)[\t ]\([0-9]\+\)$/\2 \1/' | sort -nr >sorted_counts


To find files that contain non-ASCII characters::

    LANG=C grep ‘[^ -~]‘ -r train | head
