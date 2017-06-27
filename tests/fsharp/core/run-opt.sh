#!/usr/bin/env sh
echo "Running optimized..." && \
which fsharpc && \
cat `which fsharpc` && \
ls -xlR `which fsharpc` && \
(ls -xlR /Library/Frameworks/Mono.framework/Versions/5.0.1/lib/mono/fsharp/fsc.exe || true) && \
echo fsharpc --define:TESTS_AS_APP --define:Portable --define:MONO --define:UNIX --define:TESTS_AS_APP --quotations-debug+ --optimize+ --debug+ ./run-all.fsx -o run-all-opt.exe && \
fsharpc --define:TESTS_AS_APP --define:Portable --define:MONO --define:UNIX --define:TESTS_AS_APP --quotations-debug+ --optimize+ --debug+ ./run-all.fsx -o run-all-opt.exe && \
mono ./run-all-opt.exe

