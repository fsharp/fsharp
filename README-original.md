F# 3.0 Compiler + Library Source Code Drop, matching Visual Studio 2012 (R) RTM binary release
===

This directory contains a drop of the source code for an F# 3.0 compiler and core library. The code has been cleaned up "a little" to try to help ensure better stability as more development is done on the codebase.

The compiler is normally compiled as a set of .NET 4.0 components. The compiler can also be hosted in a browser to implement websites like [Try F#](http://tryfsharp.org).

**Before we start, are sure you're in the right place?**

* To get a free F# environment for Windows, go to [fsharp.net](http://fsharp.net).
* To get a free F# environment for Linux or Mac, go to [fsxplat.codeplex.com](http://fsxplat.codeplex.com).
* To learn what F# is and why it's interesting, go to [fsharp.net](http://fsharp.net) or [tryfsharp.org](http://tryfsharp.org).
* If you want to to use F# in Visual Studio 2010 (R) or Visual Studio 2012 (R), go to [fsharp.net](http://fsharp.net).
* Looking for F# coding samples? Go to [fsharp.net](http://fsharp.net) or [tryfsharp.org](http://tryfsharp.org) or [fssnip.net](http://fssnip.net). While the code has its nice points, it is not a model F# codebase and should not be used as guidance for F# coding style - there are plenty of things we would change if we had all the time in the world.

To emphasize, this distribution should not be seen as a way to "get" an F# compiler for immediate use. For that you're better going to [fsharp.net](http://fsharp.net) or [fsxplat.codeplex.com](http://fsxplat.codeplex.com)

**License:** subject to terms and conditions of the Apache License, Version 2.0. A copy of the license can be found in the License.html file at the root of this distribution. By using this source code in any fashion, you are agreeing to be bound by the terms of the Apache License, Version 2.0. You must not remove this notice, or any other, from this software.

**Questions?** If you have questions about the source code, please ask at the [F# Open Source Google Group](http://groups.google.com/group/fsharp-opensource). Please do not ask the F# team at Microsoft for help with this source code: they like to be friendly, but they are very busy working on improving F# and need to focus on that.

**Updates?** The F# team do not do active development in open repositories, though some changes such as cleanup or additional tools may be submitted. They aspire to update the code drop when future versions of F# compilers are released from Microsoft, usually at or around the RTM stage.

**Copyright:** Copyright 2002-2012 (c) Microsoft Corporation.


##What do I get when I compile?

When you build the compiler using the standard instructions below, you get `fsc.exe`, `fsi.exe`, `FSharp.Core.dll`, `FSharp.Compiler.dll` and some related DLLs.

The compiler binaries produced are "private" and strong-named signed with a test key (`src\fsharp\test.snk`). They use CLI assembly version nunmber **2.9.9.999**. You can place these components in the GAC but they will not replace the components used by normal Visual Studio or normal F# programs.


##Steps - Building a Proto Compiler

```
cd src 
gacutil /i ..\lkg\FSharp-2.0.50726.900\bin\FSharp.Core.dll
msbuild fsharp-proto-build.proj /p:TargetFramework=net40
```

Note: Make sure you run the .NET 4.0 `msbuild.exe`, e.g. `C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe`.

Optional: NGEN the Proto Compiler for faster future startup (optional)

```
ngen install ..\Proto\net40\bin\fsc-proto.exe
```


##Steps - Building the F# Core Library

This uses the proto compiler to build the FSharp.Core library, for Mono/.NET 4.0.

```
msbuild fsharp-library-build.proj /p:TargetFramework=net40
```

Note: Make sure you run the .NET 4.0 `msbuild.exe`, e.g. `C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe`.


##Steps - Building the F# Compiler

This uses the proto compiler to build the `FSharp.Compiler.dll` and `fsc.exe` to run on for Mono/.NET 4.0.

```
msbuild fsharp-compiler-build.proj /p:TargetFramework=net40
```

Note: Make sure you run the .NET 4.0 `msbuild.exe`, e.g. `C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe`.


##Steps - Building a compiler component for hosting in the browser with Silverlight or Moonlight

This builds `FSharp.Compiler.Silverlight.dll` which is a Silverlight 5.0 component for hosting in the browser.

*Debug configuration for browser-hosted compiler for Silverlight 5.0:*
```
msbuild fsharp-library-build.proj /p:TargetFramework=sl5-compiler
msbuild fsharp-compiler-build.proj /p:TargetFramework=sl5-compiler
```

*Release configuration for browser-hosted compiler for Silverlight 5.0:*
```
msbuild fsharp-library-build.proj /p:TargetFramework=sl5-compiler /p:Configuration=Release
msbuild fsharp-compiler-build.proj /p:TargetFramework=sl5-compiler /p:Configuration=Release
```

The binaries are placed in `Debug\sl5-compiler`, `Release\sl5-compiler`, `Debug\sl5-compiler` and/or `Release\sl5-compiler`. A custom `FSharp.Core.dll` is used for this configuration, be sure to reference it rather than any other `FSharp.Core.dll` for Silverlight. You may have to edit your project file by hand to ensure you get exactly the right reference to `FSharp.Core.dll`.


##Building the F# core library for alternative CLI/.NET/CIL implementations

```
msbuild fsharp-library-build.proj /p:TargetFramework=net20
msbuild fsharp-library-build.proj /p:TargetFramework=sl3-wp
msbuild fsharp-library-build.proj /p:TargetFramework=sl5
```

```
msbuild fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=sl3-wp /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
```

Here **net20** gives a runtime for .NET 2.0-3.5, **sl3-wp** gives a runtime for Windows Phone 7, and **sl5** gives a runtime for Silverlight 5.


##Steps - Building F# Core Unit Tests for .NET 4.x (optional)

This uses the proto compiler to build the unit tests that check some parts of `FSharp.Core.dll` and `FSharp.Compiler.dll`. There is also another set of tests under `tests\fsharp`.

```
msbuild fsharp-library-unittests-build.proj /p:TargetFramework=net40
```

*Note: You must have NUnit installed.*


##Steps - Running Compiler tests (on Windows)

There are language tests under `tests\fsharp\core`. The test apparatus is primitive and unfortunately uses batch files. You can run these on Windows using:

```
cd ..\tests\fsharp\core
..\..\build-and-run-all-installed-ilx-configs.bat results.log
```

The results file will contain one entry for each test directory, plus any reported errors.

```
C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core
C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core\queriesCustomQueryOps
C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core\queriesLeafExpressionConvert
C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core\queriesNullableOperators
C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core\queriesOverIEnumerable
...
```

Some tests for LINQ queries require SQL Server be installed. A failing test will look like this:

```
ERRORLEVEL=1: in C:\projects\openfsharp\Compiler\3.0\head\tests\fsharp\core\csfromfs\build.bat
```

You can then go to the relevant directory and run `build.bat` and `run.bat`.


##Using the FSharp.Core you built

The `FSharp.Core.dll` library produced uses the standard version number **4.3.0.0** and is delay signed with the Microsoft public key. This gives it the same identity as `FSharp.Core` for F# 3.0 in Visual Studio 2012. This means that if you have Visual Studio 2012 installed, the `FSharp.Core` you produce will not be used by default. You must replace the one in the GAC and skip strong-name verification for the DLL, e.g.

```
sn -Vr FSharp.Core,*
gacutil /i Debug\net40\bin\FSharp.Core.dll
```

However, this is not recommended except in the rare case you are adding extra functionality to `FSharp.Core` - it is better to just continue to run with the `FSharp.Core` that comes with Visual Studio 2012.


##Preparing for inclusion in Mono

Building for the **mono20** and **mono40** frameworks gives strong-named, delay-signed assemblies with the `msfinal.pub` key and standard version numbers such as **2.0.0.0**, **2.3.0.0**, **4.0.0.0** and **4.3.0.0**. You complete the signing of these assemblies using [http://github.com/fsharp/fsharp/raw/master/mono.snk](http://github.com/fsharp/fsharp/raw/master/mono.snk). These assemblies will not run if you already have a version of them installed in your GAC (e.g. if you have Visual Studio 2012).

```
sn -R ..\Debug\mono40\bin\fsc.exe mono.snk
sn -R ..\Debug\mono40\bin\fsi.exe mono.snk
sn -R ..\Debug\mono40\bin\FSharp.Core.dll mono.snk
sn -R ..\Debug\mono40\bin\FSharp.Compiler.dll mono.snk
sn -R ..\Debug\mono40\bin\FSharp.Compiler.Server.Shared.dll mono.snk
sn -R ..\Debug\mono40\bin\FSharp.Compiler.Interactive.Settings.dll mono.snk
```

This is the way the assemblies are built in a source build of the canonical GitHub repository for F# and how the binaries are shipped in Mono binary distributions.


##Notes on the build

The prerequisites and build command line for compiling the source (on Windows) are shown later in this README. Here's the logic of the build:

* We first need an existing F# compiler, usually the one available from [fsharp.net](http://fsharp.net), although it could also be another. Let's assume this compiler has an `FSharp.Core.dll` with version X.
* We use this compiler to compile the source in this distribution, to produce a "proto" compiler, in the `Proto` directory. When run, this compiler still relies on the `FSharp.Core.dll` with version X.
* We use the proto compiler to compile the source for `FSharp.Core.dll` in this distribution, producing an `FSharp.Core.dll` with the version identified in `src\source-build-version`, usually **1.9.999**.
* We use the proto compiler to compile the source for `FSharp.Compiler.dll`, `fsc.exe`, `fsi.exe` and other binaries found in this distribution. When run, these binaries will rely on the `FSharp.Core.dll` with version **1.9.999**. This is good, since it means the 1.9.999 binaries now form a consistent, bootstrapped compiler. If you like you should now be able to throw away the compiler with version X.

Some additional tools are required to build the compiler, notably `fslex.exe`, `fsyacc.exe`, `FSharp.PowerPack.Build.Tasks.dll`, `FsSrGen.exe`, `FSharp.SRGen.Build.Tasks.dll` and the other tools found in the `lkg` directory. These are "Last Known Good" binaries created from a version of the F# Power Pack on CodePlex. If you like you can throw away these binaries and use your own compiled versions of these. tools.


##Validation and Use

Here are some simple tests to validate what you have built by checking `fsi.exe` (F# Interactive) starts up:

```
ngen install ..\Debug\net40\bin\fsi.exe
..\Debug\net40\bin\fsi.exe
1 + 1;;
\#q;;
..\Debug\net40\bin\fsi.exe /help
..\Debug\net40\bin\fsc.exe /help
echo printfn "hello world" > hello.fs
..\Debug\net40\bin\fsc.exe hello.fs
copy ..\Debug\net40\bin\FSharp.Core.dll .
hello.exe
del /q FSharp.Core.dll
```


##Some alternative Steps - Building for .NET 2.0 profile

```
cd src
msbuild fsharp-proto-build.proj /p:TargetFramework=net20
msbuild fsharp-library-build.proj /p:TargetFramework=net20
msbuild fsharp-compiler-build.proj /p:TargetFramework=net20
msbuild fsharp-library-unittests-build.proj /p:TargetFramework=net20
```

```
ngen install ..\Debug\net20\bin\fsi.exe
..\Debug\net20\bin\fsi.exe
1 + 1;;
\#q;;
..\Debug\net20\bin\fsi.exe /help
..\Debug\net20\bin\fsc.exe /help
echo printfn "hello world" > hello.fs
..\Debug\net20\bin\fsc.exe hello.fs
copy ..\Debug\net20\bin\FSharp.Core.dll .
hello.exe
del /q FSharp.Core.dll
```


##Some alternative Steps - Building an optimized (Release) compiler for .NET 4.0 profile

```
msbuild fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
```

```
ngen install ..\Release\net40\bin\fsi.exe
..\Release\net40\bin\fsi.exe
1 + 1;;
\#q;;
..\Release\net40\bin\fsi.exe /help
..\Release\net40\bin\fsc.exe /help
echo printfn "hello world" > hello.fs
..\Release\net40\bin\fsc.exe hello.fs
copy ..\Release\net40\bin\FSharp.Core.dll .
hello.exe
del /q FSharp.Core.dll
```


##Some alternative Steps - Other examples of building for Release mode (choose some of these as you need)

```
msbuild fsharp-library-build.proj /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=sl3-wp /p:Configuration=Release
msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
```

```
msbuild fsharp-compiler-build.proj /p:TargetFramework=net20 /p:Configuration=Release
```


##Editing and Building on Windows using Visual Studio 2012
###Prerequisites

Visual Studio Shell 2012 (with F# CTP MSI added), Visual Studio Professional 2012 or another non-Express version of Visual Studio 2012.

###Editing and Building

Open `all-vs2012.sln`, and edit in modes Debug or Release. The compiler takes a long time to compile and that can be a bit invasive to the work flow, so it's normally better to do the actual compilation from the command line, see above.
