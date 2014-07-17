@echo off
::Env
if %PROCESSOR_ARCHITECTURE%==x86 (
	set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
) else (
	set MSBUILD=%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe
)

::Clean
del /F /S /Q lib\proto
del /F /S /Q lib\release

::Build
pushd .
cd .\src
set ABS_PATH=%CD%
%MSBUILD% "%ABS_PATH%\fsharp-proto-build.proj"
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-compiler-build.proj" /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=net20 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=portable47 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=portable7 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=portable78 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=portable259 /p:Configuration=Release
%MSBUILD% "%ABS_PATH%\fsharp-library-build.proj" /p:TargetFramework=sl5 /p:Configuration=Release
popd
