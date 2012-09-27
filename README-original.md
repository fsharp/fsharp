F# 2.0 Compiler + Library Source Code Drop, matching April 2010 binary releases
===

This directory contains a drop of the source code for an F# 2.0 compiler and core library. The code has been cleaned up "a little" to try to help ensure better stability as more development is done on the codebase.

**Before we start, are sure you're in the right place?**

* To get an F# compiler, go to [http://fsharp.net](fsharp.net) or [http://tryfsharp.org](tryfsharp.org)
* To learn F#, go to [http://fsharp.net](fsharp.net) or [http://tryfsharp.org](tryfsharp.org)  
* To learn what F# is and why it's interesting, go to [http://fsharp.net](fsharp.net) or [http://tryfsharp.org](tryfsharp.org)
* To download and install an F# compiler, go to [http://fsharp.net](fsharp.net) or [http://tryfsharp.org](tryfsharp.org)
* If you want to to use F# in Visual Studio 2008 (R) or Visual Studio 2010 (R), go to [http://fsharp.net](fsharp.net).
* Looking for F# coding samples? Go to [http://fsharp.net](fsharp.net) or search elsewhere.
  This code is not a model F# codebase and should not be used as guidance for F# coding style - there are plenty of things we would change if we had all the time in the world.

To emphasize, this distribution should not be seen as a way to "get" an F# compiler for immediate use. For that you're better going to fsharp.net or tryfsharp.org

**Copyright:** Copyright 2002-2010 (c) Microsoft Corporation.

**License:** subject to terms and conditions of the Apache License, Version 2.0. A copy of the license can be found in the License.html file at the root of this distribution. By using this source code in any fashion, you are agreeing to be bound by the terms of the Apache License, Version 2.0.

You must not remove this notice, or any other, from this software.

**Questions?** If you have questions about the source code, please ask on a forum, or start a forum for community discussions, or post to the forum on fsharppowerpack.codeplex.com. Please do not ask the F# team at Microsoft for help with this source code: they like to be friendly, but they are very busy working on improving F# and need to focus on that.

**Updates?** The F# team do not do active development in this repository, though some changes such as cleanup or additional tools may be submitted. They aspire to update the tree as and when future versions of F# compilers are released from Microsoft.


##What do I get when I compile?

When you compile, you get fsc.exe, fsi.exe, FSharp.Core.dll and some related DLLs.


##How do I compile?

The prerequisites and build command line for compiling the source (on Windows) are shown later in this README. Here's the logic of the build:

* We first need an existing F# compiler, usually the one available from fsharp.net, although it could also be another. Let's assume this compiler has an FSharp.Core.dll with version X.
* We use this compiler to compile the source in this distribution, to produce a "proto" compiler, in the Proto directory. When run, this compiler still relies on the FSharp.Core.dll with version X.
* We use the proto compiler to compile the source for FSharp.Core.dll in this distribution, producing an FSharp.Core.dll with the version identified in src\source-build-version, usually 1.9.999.
* We use the proto compiler to compile the source for FSharp.Compiler.dll, fsc.exe, fsi.exe and other binaries found in this distribution. When run, these binaries will rely on the FSharp.Core.dll with version 1.9.999. This is good, since it means the 1.9.999 binaries now form a consistent, bootstrapped compiler. If you like you should now be able to throw away the compiler with version X.

Some additional tools are required to build the compiler, notably fslex.exe, fsyacc.exe, FSharp.PowerPack.Build.Tasks.dll, FsSrGen.exe, FSharp.SRGen.Build.Tasks.dll and the other tools found in the lkg directory. These are "Last Known Good" binaries created from a version of the F# Power Pack on CodePlex. If you like you can throw away these binaries and use your own compiled versions of these. tools.


##Strong names and versions

When you build the compiler using the instructions below, the compiler and library binaries produced are not strong-named or signed, and use CLI assembly version nunmber 1.9.9.999. Without a strong-name you will not be able to add FSharp.Core.dll to the GAC, though that is not a problem for most purposes since the FSharp.Core.dll you compile can be copied as part of your application.

Once you have an F# compiler, you will normally be able to reference another or an existing FSharp.Core.dll and matching mscorlib.dll explicitly to target that profile.

To add strong names you would need to adjust the build settings. Our recommendation is not to use the version number 2.0 on your binaries, since that can get confusing if you already have an installation of an F# 2.0 compiler.


##Building on Windows using MSBuild from the Command Linees

* Both .NET 2.0 and .NET 4.0
* Visual Studio 2010 Shell (Integrated Edition, with F# CTP MSI added), OR Visual Studio Professional 2010, OR another non-Express version of Visual Studio 2010 OR the F# CTP MSI installed as a standlone compiler.


##Steps - Cleaning (if needed)

    cd src 
    rmdir /s /q ..\Proto
    rmdir /s /q ..\Debug
    rmdir /s /q ..\Release


##Steps - Building a Proto Compiler for Mono or .NET 2.0 using .NET 2.0/4.0 tools

    cd src 
    msbuild fsharp-proto-build.proj /p:TargetFramework=cli\2.0

Note: Make sure you run the .NET 4.0 msbuild.exe, e.g. C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe.


##Steps - NGEN the Proto Compiler for faster future startup (optional)

    ngen install ..\Proto\cli\2.0\bin\fsi.exe


##Steps - Building the F# core library and unittests for running on Mono or .NET 2.0-4.0, using the Proto compiler

    msbuild fsharp-library-build.proj /p:TargetFramework=cli\2.0

**Note:** Make sure you run the .NET 4.0 msbuild.exe, e.g. C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe.


##Steps - Building a bootstrapped F# Compiler for running on Mono or .NET 2.0-4.0, using the Proto Compiler

    msbuild fsharp-compiler-build.proj /p:TargetFramework=cli\2.0

**Note:** Make sure you run the .NET 4.0 msbuild.exe, e.g. C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe.


##Steps - Building a F# Compiler unit tests for NUnit for running on Mono or .NET 2.0-4.0, using the Proto Compiler

    msbuild fsharp-library-unittests-build.proj /p:TargetFramework=cli\2.0
    msbuild fsharp-compiler-unittests-build.proj /p:TargetFramework=cli\2.0

**Note:** You must have NUnit installed.


##Validation and Use

    ngen install ..\Debug\cli\2.0\bin\fsi.exe
    ..\Debug\cli\2.0\bin\fsi.exe
    1 + 1;;
    #q;;
    ..\Debug\cli\2.0\bin\fsi.exe /help
    ..\Debug\cli\2.0\bin\fsc.exe /help
    echo printfn "hello world" > hello.fs
    ..\Debug\cli\2.0\bin\fsc.exe hello.fs
    copy ..\Debug\cli\2.0\bin\FSharp.Core.dll .
    hello.exe
    del /q FSharp.Core.dll 


##Some alternative Steps - Building the F# core library and unittests for alternative CLI/.NET/CIL implementations, using the Proto compiler

    msbuild fsharp-library-build.proj /p:TargetFramework=Silverlight\3.0
    msbuild fsharp-library-build.proj /p:TargetFramework=Silverlight\4.0
    msbuild fsharp-library-build.proj /p:TargetFramework=WindowsPhone7\Silverlight\4.0
    msbuild fsharp-library-build.proj /p:TargetFramework=CompactFramework\2.0
    msbuild fsharp-library-build.proj /p:TargetFramework=CompactFramework\3.5

##Some alternative Steps - Building for .NET 4.0 profile

    cd src 
    msbuild fsharp-proto-build.proj /p:TargetFramework=cli\4.0
    msbuild fsharp-library-build.proj /p:TargetFramework=cli\4.0
    msbuild fsharp-compiler-build.proj /p:TargetFramework=cli\4.0
    msbuild fsharp-library-unittests-build.proj /p:TargetFramework=cli\4.0
    msbuild fsharp-compiler-unittests-build.proj /p:TargetFramework=cli\4.0

    ngen install ..\Debug\cli\4.0\bin\fsi.exe
    ..\Debug\cli\4.0\bin\fsi.exe
    1 + 1;;
    #q;;
    ..\Debug\cli\4.0\bin\fsi.exe /help
    ..\Debug\cli\4.0\bin\fsc.exe /help
    echo printfn "hello world" > hello.fs
    ..\Debug\cli\4.0\bin\fsc.exe hello.fs
    copy ..\Debug\cli\4.0\bin\FSharp.Core.dll .
    hello.exe
    del /q FSharp.Core.dll 

    msbuild fsharp-compiler-build.proj /p:TargetFramework=cli\4.0 /p:Configuration=Release

    ngen install ..\Release\cli\4.0\bin\fsi.exe
    ..\Release\cli\4.0\bin\fsi.exe
    1 + 1;;
    #q;;
    ..\Release\cli\4.0\bin\fsi.exe /help
    ..\Release\cli\4.0\bin\fsc.exe /help
    echo printfn "hello world" > hello.fs
    ..\Release\cli\4.0\bin\fsc.exe hello.fs
    copy ..\Release\cli\4.0\bin\FSharp.Core.dll .
    hello.exe
    del /q FSharp.Core.dll 


##Some alternative Steps - Building Release mode (choose some of these as you need)

    msbuild fsharp-library-build.proj /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=cli\2.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=cli\4.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=Silverlight\3.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=Silverlight\4.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=WindowsPhone7\Silverlight\4.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=CompactFramework\2.0 /p:Configuration=Release
    msbuild fsharp-library-build.proj /p:TargetFramework=CompactFramework\3.5 /p:Configuration=Release

    msbuild fsharp-compiler-build.proj /p:TargetFramework=cli\2.0 /p:Configuration=Release

    ngen install ..\Release\cli\2.0\bin\fsi.exe
    ..\Release\cli\2.0\bin\fsi.exe
    1 + 1;;
    #q;;
    ..\Release\cli\2.0\bin\fsi.exe /help
    ..\Release\cli\2.0\bin\fsc.exe /help
    echo printfn "hello world" > hello.fs
    ..\Release\cli\2.0\bin\fsc.exe hello.fs
    copy ..\Release\cli\2.0\bin\FSharp.Core.dll .
    hello.exe
    del /q FSharp.Core.dll 
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Release\cli\2.0\bin\Fsi.exe /help
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Release\cli\2.0\bin\Fsc.exe /help
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Release\cli\2.0\bin\Fsi.exe < hello.fs
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Release\cli\2.0\bin\Fsc.exe hello.fs

##Editing and Building on Windows using Visual Studio 2010
###Prerequisites

* Visual Studio Shell 2010 (with F# CTP MSI added), Visual Studio Professional 2010 or another non-Express version of Visual Studio 2010.

###Editing and Building

Open all-vs2010.sln, and edit in modes Debug or Release. The compiler takes a long time to compile and that can be a bit invasive to the work flow, so it's normally better to do the actual compilation from the command line, see above.

##Building where a Proto compiler is run on Mono 2.8, Windows

This can be useful if you want to prove to yourself that it's possible to build without running the base compiler using .NET CLR. At the moment however the steps below still use MSBuild and compile against the .NET 2.0 reference assemblies. It would need more work to get the build going against the Mono libraries.

###Prerequisites

* An existing F# compiler to start the bootstrap. You can start with the Microsoft F# release, then build a new compiler using that, then throw away the former and just use the compiler(s) you've built from then on.
* Mono 2.9 for Windows 

###Building

See below for some of the sample steps. On Windows it is best to use fsc-mono.bat files to act as a "compiler that just happens to run using Mono".

###Steps - Building a Proto Compiler for Mono or .NET 2.0 (either one)

    cd src 
    REM Prepare driver batch files that run the corresponding exe using Mono
    copy /y setups\run-as-mono.bat ..\lkg\bin\fsc-mono.bat
    copy /y setups\run-as-mono.bat  ..\lkg\FSharp-2.0.50726.900\bin\fslex-mono.bat
    copy /y setups\run-as-mono.bat  ..\lkg\FSharp-2.0.50726.900\bin\fsyacc-mono.bat
    msbuild fsharp-proto-build.proj /p:TargetFramework=mono\2.0

###Steps - Prepare proto compiler for execution on Mono by registering FSharp.Core.dll for the base compiler


    copy /y "C:\Program Files\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v2.0\FSharp.Core.dll" setups\FSharp.Core.dll
    "C:\Program Files\Mono-2.8\bin\sn" -R setups\FSharp.Core.dll setups\mono.snk  
    "C:\Program Files\Mono-2.8\bin\gacutil" /i setups\FSharp.Core.dll 
    del setups\FSharp.Core.dll

###Steps - Prepare driver batch files that run the corresponding exe with mono

    copy /y setups\run-as-mono.bat ..\Proto\cli\2.0\bin\fsc-proto-mono.bat
    copy /y setups\run-as-mono.bat  ..\lkg\FSharp-2.0.50726.900\bin\fslex-mono.bat
    copy /y setups\run-as-mono.bat  ..\lkg\FSharp-2.0.50726.900\bin\fsyacc-mono.bat

###Steps - Build the core library using the proto compiler, running the proto with Mono on Windows

    msbuild fsharp-library-build.proj /p:TargetFramework=mono\2.0

###Steps - Bootstrap the compiler using the proto compiler, running the proto with Mono on Windows

    msbuild fsharp-compiler-build.proj /p:TargetFramework=mono\2.0

###Validation and Use

    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Debug\mono\2.0\bin\fsi.exe
    1 + 1;;
    #q;;
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Debug\mono\2.0\bin\fsi.exe /help
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Debug\mono\2.0\bin\fsc.exe /help
    echo printfn "hello world" > hello.fs
    "c:\Program Files\Mono-2.8\bin\mono.exe" ..\Debug\mono\2.0\bin\fsc.exe hello.fs
    copy ..\Debug\mono\2.0\bin\FSharp.Core.dll .
    "c:\Program Files\Mono-2.8\bin\mono.exe" hello.exe
    del /q FSharp.Core.dll 
    

###Building on Unix with Mono

Because of some issues with xbuild, some shell scripts are provided to build the compiler on Unix. You may need to adapt the paths in config.sh first.

    ./make_proto.sh
    ./make_library.sh
    ./make_compiler.sh
