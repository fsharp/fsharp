@echo off
::Env
set MSBuild="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"

::Clean
del /F /S /Q lib\proto
del /F /S /Q lib\release

::Build
%MSBUILD% src\fsharp-proto-build.proj
ngen install lib\proto\fsc-proto.exe
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% src\fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=net20 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=Release
%MSBUILD% src\fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
