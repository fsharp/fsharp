@echo off
::Env
if %PROCESSOR_ARCHITECTURE%==x86 (
	set MSBuild="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe"
) else (
	set MSBUILD=%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe
)

::Clean
rm -rf lib/proto
rm -rf lib/release

::Build
pushd .
cd ./src
set ABS_PATH=%CD%
%MSBUILD% %ABS_PATH%/fsharp-proto-build.proj /p:TargetFramework=net40 
mv "../lib/release" "../lib/proto"

%MSBUILD% %ABS_PATH%/fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% %ABS_PATH%/fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
popd
