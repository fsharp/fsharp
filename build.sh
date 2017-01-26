#!/bin/sh

# At the moment all we build is the Mono version of the F# compiler
export BUILD_NET40=1

# Perform any necessary setup prior to running builds
# (e.g., restoring NuGet packages).
echo "before_install.sh..."

./before_install.sh

rc=$?;
if [ $rc -ne 0 ]; then
    echo "before_install script failed."
    exit $rc
fi
echo "done before_install.sh, building..."

chmod +x travis-autogen.sh

# Generate the makefiles 
# Bootstrap the compiler
# Run some project building tests
# Run some a variation of the tests/fsharp suite
# Run the FSharp.Core.Unittests suite
./travis-autogen.sh &&
make &&
sudo make install &&
(cd tests/projects; ./build.sh) &&
(cd tests/fsharp/core; ./run-opt.sh) &&
mono packages/NUnit.Console.3.0.0/tools/nunit3-console.exe --agents=1 Release/net40/bin/FSharp.Core.Unittests.dll

