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
