#!/usr/bin/env sh
echo "Running optimized..." && \
fsharpc --define:TESTS_AS_APP --define:Portable --define:MONO --define:UNIX --define:TESTS_AS_APP --quotations-debug+ --optimize+ --debug+ ./run-all.fsx -o run-all-opt.exe && \
mono ./run-all-opt.exe

