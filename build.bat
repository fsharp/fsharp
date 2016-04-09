@echo off

:: Check prerequisites
set _msbuildexe="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles(x86)%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% echo Error: Could not find MSBuild.exe.  Please see http://www.microsoft.com/en-us/download/details.aspx?id=40760. && goto :eof

set msbuildflags=/maxcpucount
set _ngenexe="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\ngen.exe"
if not exist %_ngenexe% echo Note: Could not find ngen.exe. 

::Clean
del /F /S /Q lib\proto
del /F /S /Q lib\release

::Build
.nuget\NuGet.exe restore -PackagesDirectory packages
%_ngenexe% install packages\FSharp.Compiler.Tools.4.0.0.1\tools\fsc.exe
%_msbuildexe% src\fsharp-proto-build.proj
%_ngenexe% install lib\proto\fsc-proto.exe
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-unittests-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-compiler-unittests-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release /p:KeyFile=..\..\..\mono.snk
