#!/usr/bin/env sh
echo "Running optimized..." && \
fsharpc --define:Portable --define:MONO --define:UNIX --define:ALL_IN_ONE --quotations-debug+ --optimize+ --debug+ ./run-all.fsx -o run-all-opt.exe && \
mono ./run-all-opt.exe

