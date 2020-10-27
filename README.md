## The F# repository has moved

**Please go to https://github.com/dotnet/fsharp/. All contributions to the F# compiler/library/tools now go there.**

This repository is the old repository for alternative packagings of F#.  See notes below for historical notes on these. The core logic of F# is made available as a library component called FSharp.Compiler.Service, and F# is open soruce, so an unlimited number of other packagings of F# are possible. 

## Archival: Details on the various Alternative Packagings

### Archival: The ``FSharp.Compiler.Tools`` NuGet package

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

### Archival: The ``fsharp`` Debian Linux Package

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

### Archival: F# packaging in Mono + macOS 

F# is packaged as part of Mono on macOS. Jason Imison says:

> We use a system called BockBuild that pushes versions of F# (sometimes with patches) out with Mono for macOS (F# is bundled with mono here, not a separate package).

> You can see an example build script here (if you have access, ping me if not) https://github.com/xamarin/bockbuild/blob/2017-02/packages/fsharp.py. Unfortunately, you need to know the branch name here – 2017-02 is what is going to be released with VS for Mac aka Mono 4.9.x

> We build fsharp/fsharp internally so that we’re not dependent on you pushing out fixes / bumping packages.  Miguel de Icaza  likes to ‘own’ the code that we ship precisely to stop these kind of emergency issues.

@cartermp says: 

> For future reference, [dependencies and code for the F# editing and F# Interactive support in Visual Studio for Mac/Xamarin Studio is here](https://github.com/mono/monodevelop/blob/edcdc0d8daa4c25bb8ce36e2dd490c8a50439537/main/external/fsharpbinding/paket.dependencies)


## Archival: History

See https://fsharp.org/history

F# compiler sources as initially dropped are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).

On 4 April 2014, Microsoft Open Tech published the F# compiler sources  at http://visualfsharp.codeplex.com and began
accepting contributions to the F# compiler/library and tools.  

In 2016 the http://visualfsharp.codeplex.com repo moved to GitHub at https://github.com/Microsoft/visualfsharp.

In 2019 the .NET Foundation and the F# community unified repositories at https://github.com/dotnet/fsharp.


