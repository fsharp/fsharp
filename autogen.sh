#!/usr/bin/env sh
which autoreconf > /dev/null && autoreconf || echo "Please install autoconf"
./configure $@
