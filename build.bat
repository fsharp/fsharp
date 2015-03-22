@echo off

:: Check prerequisites
set _msbuildexe="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles(x86)%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% echo Error: Could not find MSBuild.exe.  Please see http://www.microsoft.com/en-us/download/details.aspx?id=40760. && goto :eof

::Clean
del /F /S /Q lib\proto
del /F /S /Q lib\release

::Build
%_msbuildexe% src\fsharp-proto-build.proj
ngen install lib\proto\fsc-proto.exe
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-unittests-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% src\fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% src\fsharp-compiler-unittests-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
%_msbuildexe% src\fsharp-library-build.proj /p:TargetFramework=monotouch /p:Configuration=Release /p:KeyFile=..\..\..\mono.snk
