#!/bin/sh

# Perform any necessary setup prior to running builds
# (e.g., restoring NuGet packages).
echo "prepare-mono.sh..."

./prepare-mono.sh

rc=$?;
if [ $rc -ne 0 ]; then
    echo "prepare-mono script failed."
    exit $rc
fi
echo "done prepare-mono.sh, building..."

chmod +x travis-autogen.sh

# Generate the makefiles 
# Bootstrap the compiler
# Install the compiler
./travis-autogen.sh && \
make && \
sudo make install

