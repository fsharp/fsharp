#!/usr/bin/env sh

if [[ $TRAVIS_OS_NAME == osx ]];
    then
        monoVer=$(mono --version | head -n 1 | cut -d' ' -f 5)
        prefix="/Library/Frameworks/Mono.framework/Versions/$monoVer";
    else 
    	prefix="/usr";
fi

./autogen.sh --prefix=$prefix