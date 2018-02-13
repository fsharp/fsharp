## The F# Compiler, Core Library & Tools

This repository is the [F# Software Foundation](http://fsharp.org) repository for F#, as described in [the mission statement of the Foundation](http://foundation.fsharp.org/):

> The F# Software Foundation... maintains a core open-source F# code repository and distributions made available to the public free of charge for use across multiple platforms. This includes the F# compiler, F# language specification, the F# core library and assorted tools and applications.

The process for contributing to the F# Compiler, Core Library and Tools is described [here](https://fsharp.github.io/2014/06/18/fsharp-contributions.html). 

The main day-to-day purpose of this repository is to deliver extra packagings of the F# compiler, core library and core tools for use in different settings.  This repo accepts direct contributions related to the cross-platform packaging of F#. Most other contributions to the F# compiler/library/tools go first via the [upstream repository](https://github.com/Microsoft/visualfsharp) which is also used to package the Visual F# Tools and .NET SDK tooling for F#.  This repo mirrors the core implementation of the F# language from the upstream repository. This arrangement ensures that  versions do not diverge, and that very extensive QA is done on all core contributions.


The F# community use this repo and others to publish these components:

* FSharp.Compiler.Tools NuGet package (published from this repo)

* FSharp.Compiler.Service NuGet package (published from [derivative repo](https://github.com/fsharp/FSharp.Compiler.Service))

* [Fable](http://fable.io/), the F# compiler for JavaScript, published from its own repo but using FSharp.Compiler.Service NuGet package

* “fsharp” Debian Linux packages for F# + Mono (published from [derivative repo](https://github.com/mono/linux-packaging-fsharp/)) 

* “fsharp” as bundled in macOS tooling for F# + Mono by Xamarin and installed either from [the Mono Project Download page](http://www.mono-project.com/download/#download-mac) or via homebrew cask as part of the [`mono-mdk`](https://github.com/caskroom/homebrew-cask/blob/master/Casks/mono-mdk.rb) cask (`brew cask install mono-mdk`).  The mono repository includes F# into Mono itself using [this script](https://github.com/mono/mono/blob/master/packaging/MacSDK/fsharp.py), note that some patches may be added as defined by that script.

* “fsharp” docker image (published from [related repo](https://github.com/fsprojects/docker-fsharp))

* “fsharp” homebrew formula (published as part of [the mono homebrew formula](https://github.com/Homebrew/homebrew-core/blob/master/Formula/mono.rb))

* other packagings such as: the [F# support in Jupyter Notebooks - iFSharp](https://github.com/fsprojects/IfSharp); the F# support in Azure Functions; and [WebSharper](https://github.com/intellifactory/websharper) all using the FSharp.Compiler.Service NuGet package


See notes below for most of these. Because the core logic of F# is made available as [a library component](https://github.com/fsharp/FSharp.Compiler.Service), an unlimited number of other packagings of F# are possible. Please contribute additional notes to this `README.md` if you are packaging F# for other settings.

### Contributing to the F# Compiler, Core Library and Tools


If you are using Windows, you should normally fork the [upstream repository](https://github.com/Microsoft/visualfsharp) repo and contribute directly there. Your contributions will then be merged into this repo.

If you are using Linux or macOS, you can  contribute directly to  [upstream repository](https://github.com/Microsoft/visualfsharp) if you like. Some CI for that repo runs on Linux. Your contributions will then be merged into this repo. Alternatively, you can prepare your contributions by forking this repository (the code is essentially the same). This will give you access to some additional testing
available from this repo.


## Status

The `master` branch is for F# 4.x.  To bootstrap the compiler, binaries built from an earlier version of this project are used. This codebase uses the Apache 2.0 license.

| F#   | Branch        | macOS/Linux | Windows |
|------|---------------|-----------|---------|
| 4.1+ | ``master``    | [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=master)](https://travis-ci.org/fsharp/fsharp/branches) | [![Build status](https://ci.appveyor.com/api/projects/status/7m5e2yr0snbbr7t9)](https://ci.appveyor.com/project/fsgit/fsharp) |
| 4.0  | ``fsharp4``   | [![Build Status](https://travis-ci.org/fsharp/fsharp.png?branch=fsharp4)](https://travis-ci.org/fsharp/fsharp/branches) |


## Details on the various Alternative Packagings

### The ``FSharp.Core`` NuGet package

[The FSharp.Core NuGet package](https://www.nuget.org/packages/FSharp.Core) was previously published from this repo.
With the consent of the F# Software Foundation this package is now published by Microsoft.
* FSharp.Core.dll for .NET Framework/Mono
* FSharp.Core.dll for .NET Standard

### The ``FSharp.Compiler.Tools`` NuGet package

This repo is currently used to make [the FSharp.Compiler.Tools NuGet package](https://www.nuget.org/packages/FSharp.Compiler.Tools). This package includes the following for both .NET Core and .NET Framework/Mono:
* the F# compiler `fsc.exe` 
* F# Interactive `fsi.exe`
* build support,
* a copy of FSharp.Core used to run the tools
* related DLLs.

The NuGet packages are exactly the ones produced by AppVeyor CI, e.g. [for version 4.1.2](https://ci.appveyor.com/project/fsgit/fsharp/build/4.1.2/artifacts).  They are pushed to https://nuget.org
by someone with appropriate permissions.

The ``FSharp.Compiler.Tools`` NuGet package can be used if you wish to use the latest F# compiler on a computer without relying on the installed version of Visual Studio.
Adding it via NuGet to a project will override the in-box compiler with the compiler from the NuGet package.
Note: you will need to manually modify your project file once (see https://github.com/fsharp/fsharp/issues/676). 

### The ``fsharp`` Debian Linux Package

Usage: See http://fsharp.org/use/linux

    apt-get install fsharp

See [the mono packaging repo](https://github.com/mono/linux-packaging-fsharp/), which is a downstream variant of this repo, where this package is actually made.

* There is a tag for each upstream source tag
* There is a tag for each "debianized" package
* Packaging metadata lives in debian/
* install files are files installed to disk
* cligacinstall are GAC-installed libraries
* `control` is the manifest of packages
* rules is the Makefile which handles build/install.

Jo Shields (@directhex) has done much of this work and says:

> I tend to only update the published packages when a) the same update has already been pulled in on Mac by Jason, and either b) something breaks horribly in the existing version on a new Mono, or c) someone explicitly asks me to.

> Linux package delivery is (now) based on packages built on our public Jenkins instance, and published automatically as a post-build step, based on two inputs - a Git repository in standard Debian git packaging format (which https://github.com/mono/linux-packaging-fsharp already is), and a tarball to consider as the canonical source of the next release (giving the same tarball in subsequent builds is how you indicate packaging-only changes such as alterations to metadata in debian/)

> Alexander Köplinger has admin access to Jenkins, SSH access to the Jenkins and repository servers, and has taken care of things for me in my absence in the past (but isn't a Debian packaging expert, so would be trusting that metadata changes are solid)

### F# packaging in Mono + macOS 

F# is packaged as part of Mono on macOS. Jason Imison says:

> We use a system called BockBuild that pushes versions of F# (sometimes with patches) out with Mono for macOS (F# is bundled with mono here, not a separate package).

> You can see an example build script here (if you have access, ping me if not) https://github.com/xamarin/bockbuild/blob/2017-02/packages/fsharp.py. Unfortunately, you need to know the branch name here – 2017-02 is what is going to be released with VS for Mac aka Mono 4.9.x

> We build fsharp/fsharp internally so that we’re not dependent on you pushing out fixes / bumping packages.  Miguel de Icaza  likes to ‘own’ the code that we ship precisely to stop these kind of emergency issues.

@cartermp says: 

> For future reference, [dependencies and code for the F# editing and F# Interactive support in Visual Studio for Mac/Xamarin Studio is here](https://github.com/mono/monodevelop/blob/edcdc0d8daa4c25bb8ce36e2dd490c8a50439537/main/external/fsharpbinding/paket.dependencies)

### Package feeds

A feed of NuGet packages from builds is available from AppVeyor using the NuGet feed: https://ci.appveyor.com/nuget/fsgit-fsharp

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


### Building on Linux and other Unix systems:

Building F# on Unix-type platforms requires [Mono](http://www.mono-project.com/download/) 5.0 or higher. 

	./autogen.sh --prefix=/usr
	make
	sudo make install

That build and installs optimized binaries. To make debug, use ```make CONFIG=debug```


### Building on macOS

Building on macOS requires an install of the latest Xamarin tools or Mono package. Use a prefix to your version of Mono:

	./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
	make
	sudo make install

That build and installs optimized binaries. To make debug, use ```make CONFIG=debug```

### Building on Windows

Install [.NET 4.5.1](http://www.microsoft.com/en-us/download/details.aspx?id=40779) and  [MSBuild 12.0](https://www.microsoft.com/en-us/download/details.aspx?id=40760)

Build using:

    mono\build.bat

This builds the proto compiler, then the library, then the final compiler.

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

To integrate latest changes from https://github.com/Microsoft/visualfsharp, first understand that this repo is
basically a cut-down version of "visualfsharp" with these (and other) files striped out:
* `fcs`: these files are specific to the F# compiler service 
* `vsintegration` : this repos does not contain any of the Visual Studio IDE tooling 
* `tests/fsharpqa` : this repo does not undergo the full QA test process, so tests that are not run are stripped
* `tests/service` : this repo does not run the FSharp.Compiler.Service tests
* `setup`: these files are specific to the VS packaging of F#
* `src/buildfromsource`: support for the .NET SDK build-from-source, not needed in this repo
* `TESTGUIDE.md`: support for the .NET SDK build-from-source, not needed in this repo

To integrate latest changes from https://github.com/Microsoft/visualfsharp, first fork + clone, then use

    git checkout master
    git pull https://github.com/fsharp/fsharp master
    git checkout -b NAMEOFBRANCH

Choose a different branch name as necessary.  Then choose a visualfsharp commit to integrate up to.  Normally just try:

    git pull https://github.com/Microsoft/visualfsharp master

but if you run into trouble reset and try again at a specific hash. Then remove stripped files:

    git rm -fr --ignore-unmatch vsintegration setup tests/fsharpqa tests/service TESTGUIDE.md src/buildfromsource* fcs netci.groovy src/fsharp/FSharp.Compiler.nuget scripts/dotnet-install.sh before_install.sh build.cmd build.sh

then 

    git status

and resolve any remaining conflicts. Resolving conflicts should normally be easy, but anything in ``Unmerged paths`` may need attention:
* Files "deleted by us" indicate a stripped file has been changed.  We don't care about those, so  generally add them to the ``git rm -fr`` line above, or ``git rm -f`` them one by one.  To remove them all in one go on macOS or Linux, `git status | grep "deleted by us" | awk '{print $4}' | xargs git rm`.
* Outright conflicts will need work and may indicate some change in this repository hasn't yet gone back to ``visualfsharp``

One you're happy, commit any changes you needed to make

    git commit -a -m "integration from visualfsharp master"
    git push origin NAMEOFBRANCH

then submit the PR to this repo calling it "integration from visualfsharp master" and wait until it's green. 

**Tagging a release**

Add a tag by updating the version number in

    mono\appveyor.ps1

and editing the release notes in 

    CHANGELOG-fsharp.md

either by a new PR or as part of the integration PR. Then apply the tag as follows (if you have push permission) 

    git tag YOURTAG
    git push https://github.com/fsharp/fsharp --tags


**Releasing FSharp.Compiler.Tools nuget**

After the AppVeyor CI for the tag is green the Artifacts folder of the CI will contain the FSharp.Compiler.Tools nuget package release.  You can download and push this package to nuget manually. 

    set APIKEY=...
    .nuget\nuget.exe push Downloads\FSharp.Compiler.Tools...nupkg %APIKEY% -Source https://nuget.org 

We only generally push packages coming from AppVeyor CI and not locally built packages.

**Improving this**

1. Work out how to fully automate easy integrations?
2. Work out how to pick up the version tag rather than having it in `appveyor.ps1`?
3. Automate more of the release process?     

### Continuous Integration Build

A continuous integration build is set up with Travis and AppVeyor. See above.

### Editing the Compiler with Visual Studio, Xamarin Studio or MonoDevelop

Historically it is difficult to edit the compiler with Xamarin Studio or MonoDevelop because of bugs in loading the hand-edited project files and targets used in the F# compiler build. These are generally in the process of being fixed, your mileage will vary.

## How to Test and Validate

### Linux and macOS

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

In 2016 the Microsoft http://visualfsharp.codeplex.com repo moved to GitHub at https://github.com/Microsoft/visualfsharp.

This repository uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.



Maintainers
-----------

The maintainers of this repository appointed by the F# Core Engineering Group are:

 - [Enrico Sada](https://github.com/enricosada), [Don Syme](https://github.com/dsyme)
 - with help and guidance from [Tomas Petricek](https://github.com/tpetricek), [Robin Neatherway](https://github.com/rneatherway), [Cameron Taggart](https://github.com/ctaggart), [Dave Thomas](https://github.com/7sharp9), [Jo Shields](https://github.com/directhex),  [Kevin Ransom](https://github.com/KevinRansom) and [Henrik Feldt](https://github.com/haf) and many others
