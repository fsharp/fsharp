#!/usr/bin/env sh
which autoreconf > /dev/null || (echo "Please install autoconf" && exit 1)
# on OSX autoconf may need a little help with these paths
aclocal -I /opt/local/share/aclocal -I /usr/local/share/aclocal 2> /dev/null
autoreconf && ./configure $@
