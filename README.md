ai-project-2
============

To install simply run make in the top level directory. This will
install the `metric-ff' binary in the bin directory as well as
compiling all *.gf files and the planner-haskell binary. The latter
assumes that cabal-dev is in the $PATH. As a last step run either
`make parser-python2' or `make parser-python3' depending on which
python interpreter is the standard in the current operating system.
Also a working web server configuration is assumed. This equates to a
web server with cgi support and that is allowed to follow symlinks.

TL;DR to install on most systems:

$ make
$ make parser-python2
