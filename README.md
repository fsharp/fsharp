## The Open Edition of the F# Compiler, Core Library & Tools

The main purpose of this repository is to package the open editions of the F# compiler, core library 
and core tools for use across multiple platforms.  

### Contributing to the F# Compiler, Core Library and Tools

Most contributions to the F# compiler/library/tools go first via the  
repository at https://github.com/Microsoft/visualfsharp.  This ensures that the main
packaging of F# on Windows (the Visual F# Tools) also includes any contributions that are made, and
ensures that the versions do not diverge.

If you are using Windows, you should fork that repo and contribute directly there. Your contributions will 
then be merged into this repo.

If you are using Linux or OSX, you can prepare your contributions by forking this repository (the code is 
essentially the same). This will give you access to the cross-platform testing 
available from this repo. At the moment the process is:

1. Fork this repo.
2. Build and test using the subset of tests available in this repo. If you like, submit a PR to this repo in order to trigger an automatic Travis run, or set up a Travis hook in your fork.
3. Seek initial review by posting an issue in this repository or https://github.com/Microsoft/visualfsharp. Make it clear you are working on Linux or OSX.
4. Cherry-pick your changes into a pull request for https://github.com/Microsoft/visualfsharp and submit for final testing and clearance.
5. The change will then be merged into this repo at a later point.

If you don't have access to Windows in order to run final tests at step 4. If you need help, email fsharp-opensource@googlegroups.com and ask to make a final test run on Windows.

Contributions specifically related to the packaging of the Open Edition should be made here.



## Status


The `master` branch is for the latest version of F# (currently F# 3.1).

To bootstrap the compiler, binaries built from an earlier version of this project are used.

This codebase uses the Apache 2.0 license.

## Current Build Status

Head (F# 3.1) (branch ``master``), Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=master)](https://travis-ci.org/fsharp/fsharp/branches)

F# 3.0 (branch ``fsharp_30``), Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp_30)](https://travis-ci.org/fsharp/fsharp/branches)

F# 4.0 work-in-progress (branch ``fsharp4``, Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp4)](https://travis-ci.org/fsharp/fsharp/branches)


Head (branch ``master``), Windows Server 2012 (Appveyor)  [![Build status](https://ci.appveyor.com/api/projects/status/7m5e2yr0snbbr7t9)](https://ci.appveyor.com/project/fsgit/fsharp)

## Build Requirements

Requires mono 3.0 or higher.

OS X requires automake 2.69. To install from [homebrew](http://mxcl.github.com/homebrew):

	brew install automake

## How to Build

### Linux and other Unix systems:
The usual:

	./autogen.sh --prefix=/usr
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```


### OS X

Use a prefix to your version of Mono:

	./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```

### Windows, using msbuild

Build using:

    build.bat

This build the proto compiler, then the library, then the final compiler.

You can also build these independently using:

    msbuild src\fsharp-proto-build.proj
    ngen install ..\lib\proto\fsc-proto.exe 
    msbuild src\fsharp-library-build.proj /p:Configuration=Release
    msbuild src\fsharp-compiler-build.proj /p:Configuration=Release

You can also build FSharp.Core.dll for other profiles:

    msbuild src\fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
    
You can also build FSharp.Core.dll for various specific runtimes:

    msbuild src\fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=monodroid /p:Configuration=Release
    msbuild src\fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release

You can also build the FSharp.Core for use with a browser-hosted FSharp.Compiler.Service using Silverlight 5.0:

    msbuild src\fsharp-library-build.proj /p:TargetFramework=sl5-compiler  /p:Configuration=Release

Change to ``` /p:Configuration=Debug``` for debug binaries.

Add ``` /p:FSharpCoreBackVersion=...``` to build back versions of FSharp.Core.dll:

	msbuild src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
	msbuild src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1
	msbuild src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release /p:FSharpCoreBackVersion=3.1

### Windows, using xbuild (e.g. if only Mono is installed):

    xbuild src\fsharp-proto-build.proj
    xbuild src\fsharp-library-build.proj
    xbuild src\fsharp-compiler-build.proj

Building using xbuild does not yet lay down a Mono-ready distribution (see src/fsharp/targets.make), so should only
be used for private development rather than preparing distributions. 

## Build Note: Strong Names

The FSharp.Core.dll produced is only delay-signed (Mono does not require strong names). 

If a strong-name signed FSharp.Core.dll is needed then use these:

    lib\bootstrap\signed\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll
    lib\bootstrap\signed\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll

## How to Install 

Built main compiler binaries go to
    lib/release/4.0

Additionally, versions of FSharp.Core for .NET 2.0, MonoAndroid, MonoTouch (Mono profile 2.1) go to 
     lib/release/2.0
     lib/release/2.1
     lib/release/2.1monotouch

`make install` sends the binaries to the `prefix` location, e.g.

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

(these names are the canonical names for Microsoft.FSharp.Targets used by project files coming from Visual Studio)

plus scripts

    /usr/bin/fsharpc   (F# compiler)
    /usr/bin/fsharpi   (F# Interactive)

## Development Notes

### Integrating changes from 'visualfsharp'

To integrate latest changes from https://github.com/Microsoft/visualfsharp, use
```
git remote add visualfsharp https://github.com/Microsoft/visualfsharp
git pull visualfsharp master
```

### Continuous Integration Build 

A continuous integration build is set up with Travis. See above.

### Editing the Compiler with Visual Studio, Xamarin Studio or MonoDevelop

Open `all-vs2013.sln`, and edit in modes Debug or Release. The compiler takes a good while to compile and that
can be a bit invasive to the work flow, so it's normally better to do the actual compilation from 
the command line, see above.

Historically it is difficult to edit the compiler with Xamarin Studio or MonoDevelop because of bugs in loading the hand-edited project files and targets used in the F# compiler build. These are generally in the process of being fixed, your mileage will vary.

## How to Test and Validate

### Linux and OSX

Only a subset of the tests are currently enabled.

After building and installing, run
```
cd tests/fsharp/core
./run-all.sh
```

### Windows

See the [TESTGUIDE.md](https://github.com/Microsoft/visualfsharp/blob/master/TESTGUIDE.md) for instructions for how to test on Windows. Use that repository
to develop and test on Windows.

## History 

F# compiler sources as initially dropped are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).

On 4 April 2014, Microsoft Open Tech published the F# compiler sources  at http://visualfsharp.codeplex.com and began
accepting contributions to the F# compiler/library and tools.  This repository is a modified version of that.

This repository uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

### Wheezy build

```
vagrant up
vagrant ssh
cd /vagrant
sudo apt-get install dos2unix autoconf
./autogen.sh --prefix=/usr
make
sudo make install
```

### FSharp.Core via NuGet
FSharp.Core is also available via NuGet. Packages are available for F# 3.0 and F# 3.1

[http://www.nuget.org/packages/FSharp.Core](http://www.nuget.org/packages/FSharp.Core)

The packages include all of the FSharp.Core redistributables from Visual F#. In addition, they include assemblies for MonoAndroid and MonoTouch built from this repository.

