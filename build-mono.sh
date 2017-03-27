#!/bin/sh

# At the moment all we build is the Mono version of the F# compiler
export BUILD_NET40=1

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
./travis-autogen.sh &&
make &&
sudo make install

echo "------------------------------ INSTALLED FILES --------------"
ls -xR /usr/lib/mono/fsharp
ls -xR /usr/lib/mono/xbuild
ls -xR /usr/lib/mono/gac/FSharp*
ls -xR /usr/lib/mono/Microsoft*
ls -xR /usr/lib/mono/Reference\ Assemblies
echo "------------------------------ INSTALLED FILES --------------"

