## The Open Edition of the F# Compiler, Core Library & Tools

The main purpose of this repository is to package the open editions of the F# compiler, core library 
and core tools for use across multiple platforms.  

### Contributing to the F# Compiler, Core Library and Tools

Most contributions to the F# compiler/library/tools are currently facilitated via the  
repository at http://visualfsharp.codeplex.com.  This arrangement helps ensure that the one of the major
packagings of F# (the Visual F# Tools) also includes any contributions that are made.

If you are using Windows, you should fork that repo and contribute directly.

If you are using Linux or OSX, you should prepare your contributions by forking this repository (the code is 
essentially the same). This will give you access to the cross-platform testing 
available from this repo. At the moment the process is:

1. Fork this repo.
2. Build and test using the subset of tests available in this repo. If you like, submit a PR to this repo in order to trigger an automatic Travis run, or set up a Travis hook in your fork.
3. Seek initial review by posting an issue in this repository or http://visualfsharp.codeplex.com. Make it clear you are working on Linux or OSX.
4. Cherry-pick your changes into a pull request for http://visualfsharp.codeplex.com and submit for final testing and clearance.
5. The change will then be mergerd into this repo at a later point.

It is likely that some people don't have access to Windows in order to run final tests at step 4. If you need help, email fsharp-opensource@googlegroups.com and ask to make a final test run on Windows.

Contributions specifically related to the packaging of the Open Edition should be made here.



## Status


The `master` branch is for the latest version of F# (currently F# 3.1).

To bootstrap the compiler, binaries built from an earlier version of this project are used.

This codebase uses the Apache 2.0 license.

## Current Build Status

Head (branch ``master``), Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=master)](https://travis-ci.org/fsharp/fsharp/branches)

F# 3.1 (branch ``fsharp_31``, Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp_31)](https://travis-ci.org/fsharp/fsharp/branches)

F# 3.0 (branch ``fsharp_30``), Mono 3.x, OSX + some unit tests (Travis) [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp_30)](https://travis-ci.org/fsharp/fsharp/branches)

F# 3.1, Mono 3.x (TeamCity) [![Build Status (Mono, TeamCity)][teamcity mono icon]][teamcity mono url]  

F# 3.1, .NET 4.0, Windows (TeamCity) [![Windows .net 4.0 (TeamCity)][teamcity net40 icon]][teamcity net40 url]

F# 3.1, .NET 2.0 etc., Windows (TeamCity) [![Windows .net 2.0 etc.][teamcity net20 icon]][teamcity net20 url]

## Build Requirements

Requires mono 3.0 or higher.

OS X requires automake 2.69. To install from [homebrew](http://mxcl.github.com/homebrew):
```
brew install automake
```

## How to Build

### Linux and other Unix systems:
The usual:
```
./autogen.sh --prefix=/usr
make
sudo make install
```
By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```


### OS X

Use a prefix to your version of Mono:
```
./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
make
sudo make install
```
By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```

### Windows, using msbuild

If you have only VS2012 or VS2013 installed, and not VS2010, you'll need to install the F# 2.0 Runtime (http://www.microsoft.com/en-us/download/details.aspx?id=13450).

Build using:
```
build.bat
```
This build the proto compiler, then the library, then the final compiler.

You can also build these independently using:
```
cd src
msbuild fsharp-proto-build.proj
ngen install ..\lib\proto\fsc-proto.exe 
msbuild fsharp-library-build.proj /p:Configuration=Release
msbuild fsharp-compiler-build.proj /p:Configuration=Release
```

You can also build the FSharp.Core for .NET 2.0, Xamarin Android, Xamarin iOS, 
Portable Profile47 (net45+sl5+win8), XNA 4.0 for Xbox 360, and Silverlight 5.0 profiles:
```
msbuild fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=portable-net45+sl5+win8 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=portable-windows8+net45 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release

msbuild fsharp-library-build.proj /p:TargetFramework=monodroid /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=net40-xna40-xbox360 /p:Configuration=Release
```
You can also build the FSharp.Core and FSharp.Compiler.Silverlight.dll for Silverlight 5.0:
```
msbuild fsharp-library-build.proj /p:TargetFramework=sl5-compiler  /p:Configuration=Release
msbuild fsharp-compiler-build.proj /p:TargetFramework=sl5-compiler /p:Configuration=Release
```
Change to ``` /p:Configuration=Debug``` for debug binaries.

Add ``` /p:FSharpCoreBackVersion=3.0``` to build a back version of FSharp.Core.dll with a 
version number suitable for use when building libaries that have usable with both F# 3.0 and F# 3.1 libraries.
```
msbuild fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=portable-net45+sl5+win8 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=portable-windows8+net45 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0

msbuild fsharp-library-build.proj /p:TargetFramework=monodroid /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
msbuild fsharp-library-build.proj /p:TargetFramework=net40-xna40-xbox360 /p:Configuration=Release /p:FSharpCoreBackVersion=3.0
```

### Windows, using xbuild (e.g. if no .NET is installed and only Mono 3.0 is installed):

```
cd src
xbuild fsharp-proto-build.proj
xbuild fsharp-library-build.proj
xbuild fsharp-compiler-build.proj
```

Building using xbuild does not yet lay down a Mono-ready distribution (see src/fsharp/targets.make), so should only
be used for private development rather than preparing distributions. 

## Build Note: Strong Names

The FSharp.Core.dll produced is only delay-signed (Mono does not require strong names). 
If a strong-name signed FSharp.Core.dll is needed then use the one in 
```
   lib\bootstrap\signed\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll
```

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

### Continuous Integration Build 

A continuous integration build is set up with the JetBrains/Teamcity server as part of the F# community projects there:

http://teamcity.codebetter.com/project.html?projectId=project61&tab=projectOverview

[@forki](https://github.com/forki) controls access. Presently this builds both a Mono `make install`  and a Windows `cd src; msbuild fsharp-build.proj` build.  No binaries are saved from the build, it is just for sanity checking.

### Editing the Compiler with Visual Studio or MonoDevelop

Open `all-vs2012.sln`, and edit in modes Debug or Release. The compiler takes a good while to compile and that
can be a bit invasive to the work flow, so it's normally better to do the actual compilation from 
the command line, see above.

Historically it is difficult to edit the compiler with Xamarin Studio or MonoDevelop because of bugs in loading the hand-edited project files and targets used in the F# compiler build. These are generally in the process of being fixed, your mileage will vary.

## How to Test and Validate

### Linux

Only a subset of the tests are currently enabled.

After building and installing, run
```
cd tests/fsharp/core
./run-all.sh
```

### Windows

There are language tests under `tests\fsharp\core`. The test apparatus is primitive and unfortunately uses batch.
Set up a shell (release mode):

```
cd tests
run40.bat   
ngen install ..\lib\release\fsc.exe
ngen install ..\lib\release\fsi.exe
```
Run the tests:
```
.\build-and-run.bat
```
Examine `build-and-run.log`. This will contain one entry for each test directory, plus any reported errors.

```
tests\fsharp\core
tests\fsharp\core\queriesCustomQueryOps
tests\fsharp\core\queriesLeafExpressionConvert
tests\fsharp\core\queriesNullableOperators
tests\fsharp\core\queriesOverIEnumerable
...
```

Some tests for LINQ queries require SQL Server be installed. A failing test will look like this:

```
ERRORLEVEL=1: in tests\fsharp\core\csfromfs\build.bat
```

You can then go to the relevant directory and run `build.bat` and `run.bat`.

### F# Core Unit Tests for .NET 4.x (optional)

This uses the proto compiler to build the unit tests that check some parts of `FSharp.Core.dll` and `FSharp.Compiler.dll`. There is also another set of tests under `tests\fsharp`.

```
msbuild fsharp-library-unittests-build.proj /p:TargetFramework=net40
```

*Note: You must have [NUnit](http://www.nunit.org/) installed to build the unit tests.*

### How to Validate

Here are some simple tests to validate what you have built by checking `fsi.exe` (F# Interactive) starts up:

```
lib\debug\4.0\fsi.exe
1 + 1;;
\#q;;
lib\debug\4.0\fsi.exe /help
lib\debug\4.0\fsc.exe /help
echo printfn "hello world" > hello.fs
lib\debug\4.0\fsc.exe hello.fs
hello.exe
```

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

[teamcity mono icon]: http://teamcity.codebetter.com/app/rest/builds/buildType:(id:bt814)/statusIcon
[teamcity mono url]: http://teamcity.codebetter.com/viewType.html?buildTypeId=bt814&guest=1
[teamcity net40 icon]: http://teamcity.codebetter.com/app/rest/builds/buildType:(id:bt817)/statusIcon
[teamcity net40 url]: http://teamcity.codebetter.com/viewType.html?buildTypeId=bt817&guest=1
[teamcity net20 icon]: http://teamcity.codebetter.com/app/rest/builds/buildType:(id:bt827)/statusIcon
[teamcity net20 url]: http://teamcity.codebetter.com/viewType.html?buildTypeId=bt827&guest=1
