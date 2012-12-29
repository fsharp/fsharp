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

### On Linux and other Unix systems:
The usual:
```
./autogen.sh
make
sudo make install
```

### On MacOS (OSX)

Use a prefix to your version of Mono:
```
./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
make
sudo make install
```

### On Windows, using msbuild (e.g.. if .NET is installed) 
```
cd src
msbuild fsharp-proto-build.proj
msbuild fsharp-library-build.proj
msbuild fsharp-compiler-build.proj
```
You can also build the FSharp.Core for .NET 2.0, Mono 2.1, Silverlight 5.0, Portable Profile47 (net4+sl4+wp71+win8) and XNA 4.0 for Xbox 360 profiles:
```
msbuild fsharp-library-build.proj /p:TargetFramework=net20 
msbuild fsharp-library-build.proj /p:TargetFramework=mono21
msbuild fsharp-library-build.proj /p:TargetFramework=portable-net4+sl4+wp71+win8
msbuild fsharp-library-build.proj /p:TargetFramework=sl5
msbuild fsharp-library-build.proj /p:TargetFramework=net40-xna40-xbox360
```
You can also build the FSharp.Core and FSharp.Compiler.Silverlight.dll for Silverlight 5.0:
```
msbuild fsharp-library-build.proj /p:TargetFramework=sl5-compiler 
msbuild fsharp-compiler-build.proj /p:TargetFramework=sl5-compiler
```
### On Windows, using xbuild (e.g. if no .NET is installed and only Mono 3.0 is installed):

```
cd src
xbuild fsharp-proto-build.proj
xbuild fsharp-library-build.proj
xbuild fsharp-compiler-build.proj
```

Building using xbuild does not yet lay down a Mono-ready distribution (see src/fsharp/targets.make), so should only
be used for private development rather than preparing distributions. 


## Strong Names

The FSharp.Core.dll produced is only delay-signed (Mono does not require strong names). 
If a strong-name signed FSharp.Core.dll is needed then use the one in 
```
   lib\bootstrap\signed\3.0\v4.0\FSharp.Core.dll
```


## What you get 

Once built the main compiler binaries go in 
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

(these names are the canonical names for Microsoft.FSharp.Targets used by project files coming from Visual Studio)

plus scripts

   /usr/bin/fsharpc   (F# compiler)
   /usr/bin/fsharpi   (F# Interactive)

## Development notes

###Continuous Integration Build 

We have a CI build set up with the JetBrains/Teamcity server as part of the F# community projects there:

http://teamcity.codebetter.com/project.html?projectId=project61&tab=projectOverview

@forki controls access. Right now this builds both a Mono 'make' install  and a Windows 'cd src; msbuild fsharp-build.proj' build.  No binaries are saved from  the build, it is just for sanity checking.


###Editing the Compiler with Visual Studio or MonoDevelop

Open `all-vs2012.sln`, and edit in modes Debug or Release. The compiler takes a good while to compile and that
can be a bit invasive to the work flow, so it's normally better to do the actual compilation from 
the command line, see above.

The F# support in MonoDevelop uses an in-process background compiler. On the Mac this causes pausing garbage
collections to kick in which makes editing the compiler in MonoDevelop awkward.

### Building F# Core Unit Tests for .NET 4.x (optional)

This uses the proto compiler to build the unit tests that check some parts of `FSharp.Core.dll` and `FSharp.Compiler.dll`. There is also another set of tests under `tests\fsharp`.

```
msbuild fsharp-library-unittests-build.proj /p:TargetFramework=net40
```

*Note: You must have NUnit installed to build the unit tests.*



### Validation and Use

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

### Running Compiler tests (on Windows)

There are language tests under `tests\fsharp\core`. The test apparatus is primitive and unfortunately uses batch files. You can run these on Windows using:

```
cd ..\tests\fsharp\core
..\..\build-and-run-all-installed-ilx-configs.bat results.log
```

The results file will contain one entry for each test directory, plus any reported errors.

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


## History 

F# compiler sources dropped by Microsoft are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).

Uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

