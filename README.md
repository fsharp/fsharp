This is the F# compiler, core library and core tools (open source edition). It uses the Apache 2.0 license.

The `master` branch is for the latest version of F# (currently F# 3.0).

To bootstrap the compiler, binaries built from an earlier version of this project are used.


## Requirements

Requires mono 2.9 or higher.  Prefer Mono 3.0.

On OSX, requires automake 2.69. To install from [homebrew](http://mxcl.github.com/homebrew):
```
brew install automake
```


## Building

To build and install on non-MacOS systems:
```
./autogen.sh
make
sudo make install
```

On MacOS (OSX), use a prefix to your version of Mono:
```
./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
make
sudo make install
```

You can also build using xbuild:
```
cd src
xbuild fsharp-build.proj
```
However the binaries produced are NOT yet usable because they are not correctly strong-name signed. Further, building using
xbuild does not create a Mono-ready distribution (see src/fsharp/targets.make).


## What you get

On 'make' the main compiler binaries produced go in 
    lib/release/4.0

There are versions of FSharp.Core for .NET 2.0 and MonoAndroid (Mono profile 2.1) in 
     lib/release/2.0
     lib/release/2.1

On 'make install' the binaries etc. go in the prefix, e.g. 

   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/2.0/FSharp.Core.dll
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/2.1/FSharp.Core.dll
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.0/fsc.exe
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.0/FSharp.Compiler.dll
   ...
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.5/fsc.exe
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.5/FSharp.Compiler.dll
   ...
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/gac/.../FSharp.Compiler.dll
   /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/gac/.../FSharp.Compiler.dll
   ...

plus some files for xbuild support 

    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/Microsoft\ F#/v4.0/*
    /Library/Frameworks/Mono.framework/Versions/Current/lib/mono/Microsoft\ SDKs/F#/3.0/Framework/*

(these names are the canonical names for Microsoft.FSharp.targets used by project files coming from Visual Studio)

plus scripts

   /usr/bin/fsharpc   (F# compiler)
   /usr/bin/fsharpi   (F# Interactive)


## Development notes

We have a CI build set up with the JetBrains/Teamcity server as part of the F# community projects there:

http://teamcity.codebetter.com/project.html?projectId=project61&tab=projectOverview

@forki controls access. Right now this builds both a Mono 'make' install  and a Windows 'cd src; msbuild fsharp-build.proj' build.  No binaries are saved from  the build, it is just for sanity checking.

Uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

F# compiler sources dropped by Microsoft are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).
