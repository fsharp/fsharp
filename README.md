## The Cross-Platform Packaging of the F# Compiler, Core Library & Tools

The main purpose of this repository is to package the open editions of the F# compiler, core library
and core tools for use across multiple platforms.  

### Contributing to the F# Compiler, Core Library and Tools

Most contributions to the F# compiler/library/tools go first via the  
repository at https://github.com/Microsoft/visualfsharp.  This ensures that the main
packaging of F# on Windows (the Visual F# Tools) also includes any contributions that are made, and
ensures that the versions do not diverge, and that very extensive QA is done.

If you are using Windows, you should fork the https://github.com/Microsoft/visualfsharp repo and contribute directly there. Your contributions will then be merged into this repo.

If you are using Linux or OSX, you can still contribute directly to  https://github.com/Microsoft/visualfsharp if you like,
Your contributions will then be merged into this repo. Alternatively, you can prepare your contributions by forking this repository (the code is
essentially the same). This will give you access to some additional the cross-platform testing
available from this repo. 


## Status

The `master` branch is for F# 4.x.  To bootstrap the compiler, binaries built from an earlier version of this project are used. This codebase uses the Apache 2.0 license.

| F#   | Branch        | OSX/Linux | Windows |
|------|---------------|-----------|---------|
| 4.1+ | ``master``    | [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=master)](https://travis-ci.org/fsharp/fsharp/branches) | [![Build status](https://ci.appveyor.com/api/projects/status/7m5e2yr0snbbr7t9)](https://ci.appveyor.com/project/fsgit/fsharp) |
| 4.0  | ``fsharp4``   | [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp4)](https://travis-ci.org/fsharp/fsharp/branches) |


## Outputs of this repository

### The ``FSharp.Core`` NuGet package

This repo is currently used to make [the FSharp.Core NuGet package](http://www.nuget.org/packages/FSharp.Core). This package includes
* FSharp.Core.dll for .NET Framework/Mono
* FSharp.Core.dll for .NET Core
* FSharp.Core.dll for portable profiles


The FSharp.Core NuGet package includes all of the FSharp.Core redistributables from Visual F#. In addition, they include assemblies for MonoAndroid and MonoTouch built from this repository.

### The ``FSharp.Compiler.Tools`` NuGet package

This repo is currently used to make [the FSharp.Compiler.Tools NuGet package]((http://www.nuget.org/packages/FSharp.Compiler.Tools). This package includes the
following for both .NET Core and .NET Framework/Mono:
* the F# compiler `fsc.exe` 
* F# Interactive `fsi.exe`
* build support,
* a copy of FSharp.Core used to run the tools
* related DLLs.

The ``FSharp.Compiler.Tools`` NuGet package can be used if you wish to use the latest F# compiler on a computer without relying on the installed version of Visual Studio.
Adding it via nuget to a project will override the in-box compiler with the compiler from the nuget package.
Note: you will need to manually modify your project file once (see https://github.com/fsharp/fsharp/issues/676). 

### The ``fsharp`` Debian Linux Package

Usage: See http://fsharp.org/use/linux

    apt-get install fsharp

See https://github.com/mono/linux-packaging-fsharp/, a downstream variant of this repo, where this package is actually made.

* There is a tag for each upstream source tag
* There is a tag for each "debianized" package
* Packaging metadata lives in debian/
* install files are files installed to disk
* cligacinstall are GAC-installed libraries
* `control` is the manifest of packages
* rules is the Makefile which handles build/install.

Jo Shields (@directhex) says:

> I tend to only update the published packages when a) the same update has already been pulled in on Mac by Jason, and either b) something breaks horribly in the existing version on a new Mono, or c) someone explicitly asks me to.

### F# in Mono + OSX 

F# is pacakged as part of Mono on OSX. Jason Imison says:

> We use a system called BockBuild that pushes versions of F# (sometimes with patches) out with Mono for OSX (F# is bundled with mono here, not a separate package).

> You can see an example build script here (if you have access, ping me if not) https://github.com/xamarin/bockbuild/blob/2017-02/packages/fsharp.py. Unfortunately, you need to know the branch name here – 2017-02 is what is going to be released with VS for Mac aka Mono 4.9.x


### Package feeds

A feed of nuget packages from builds is available from AppVeyor using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp

If using Paket, add the source at the top of `paket.dependencies`.

    source https://www.nuget.org/api/v2
    source https://ci.appveyor.com/nuget/fsgit-fsharp

Add the dependency on `FSharp.Core` and run `paket update`. See the AppVeyor [build history](https://ci.appveyor.com/project/fsgit/fsharp/history) for a list of available versions. Here are some options for specifying the dependency:

```
nuget FSharp.Core
nuget FSharp.Core prerelease
nuget FSharp.Core 3.1.2.3
nuget FSharp.Core 3.1.2.3-b208
```

If using NuGet Package Manager, add the source to the list of available package sources.

![Available Package Sources](https://cloud.githubusercontent.com/assets/80104/8576204/3cf077f4-2555-11e5-80cc-5db185af7d1e.png)



## Development Guide

### Build Requirements

Building F# on Unix-type platforms requires
[Mono](http://www.mono-project.com/download/) 4.4 or higher. If you
get a complaint in subsequent steps about `xbuild` being missing, it means
you don't have Mono installed.

Building on OS X requires several development tools that are not installed
by default. Most can be installed via [Homebrew](http://brew.sh/):

	brew install autoconf automake pkg-config

Building on OS X also requires Xcode. If you don't want to install
the full GUI development environment, the command line tools are sufficient.
At a shell prompt, say:

	xcode-select --install


### Building on Linux and other Unix systems:
The usual:

	./autogen.sh --prefix=/usr
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```


### Building on OS X

Use a prefix to your version of Mono:

	./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
	make
	sudo make install

By default that makes optimized binaries. To make debug, use ```make CONFIG=debug```

### Building on Windows

Build using:

    build.bat

This build the proto compiler, then the library, then the final compiler.

### Build Note: Strong Names

The FSharp.Core.dll produced is only delay-signed (Mono does not require strong names).
If a strong-name signed FSharp.Core.dll is needed then use the one in

    lib\bootstrap\signed\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll
    lib\bootstrap\signed\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll


### Building on Linux  (Wheezy build)

    vagrant up
    vagrant ssh
    cd /vagrant
    sudo apt-get install dos2unix autoconf
    ./autogen.sh --prefix=/usr
    make
    sudo make install

### Integrating changes from 'visualfsharp'

To integrate latest changes from https://github.com/Microsoft/visualfsharp, use
```
git remote add visualfsharp https://github.com/Microsoft/visualfsharp
git pull visualfsharp master
```

There are certain guidelines that need to be followed when integrating changes from upstream:
* this repository does not undergo the QA test process that upstream does, so the `tests/fsharpqa` folder and all files within should be removed when merging
* this repository does not contain any of the Visual Studio tooling or integrations, so the `vsintegration` directory and all files within should be removed when merging
* anything referencing `FSharp.LaunguageService.Compiler` is a Microsoft-internal version of the open FSharp.Compiler.Service repository, and as such should be removed when merging
* Windows-specific `update.cmd` and `runtests.cmd` aren't used in this repository, and so should be removed when merging
* anything that references the `Salsa` testing library is used by Microsoft to test the Visual Studio integrations, and as such should be removed when merging
* the foremost example of the above is the `root/unittests` folder, which contains tests for the Visual Studio integration using Salsa, and as such should be removed when merging

### Continuous Integration Build

A continuous integration build is set up with Travis and AppVeyor. See above.

### Editing the Compiler with Visual Studio, Xamarin Studio or MonoDevelop

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
accepting contributions to the F# compiler/library and tools.  

In 2016 the Microsoft http://visualfsharp.codeplex.com repo moved to GitHub at http://github.com/Microsoft/visualfsharp.

This repository uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.



Maintainers
-----------

The maintainers of this repository appointed by the F# Core Engineering Group are:

 - [Enrico Sada](https://github.com/enricosada), [Don Syme](http://github.com/dsyme)
 - with help and guidance from [Tomas Petricek](http://github.com/tpetricek), [Robin Neatherway](https://github.com/rneatherway), [Cameron Taggart](http://github.com/ctaggart), [Dave Thomas](http://github.com/7sharp9), [Jo Shields](http://github.com/directhex),  [Kevin Ransom](http://github.com/KevinRansom) and [Henrik Feldt](http://github.com/haf) and many others
