@if "%_echo%"=="" echo off

:ARGUMENTS_VALIDATION

if /I "%1" == "/help"   (goto :USAGE)
if /I "%1" == "/h"      (goto :USAGE)
if /I "%1" == "/?"      (goto :USAGE)
goto :ARGUMENTS_OK

:USAGE

echo Build and run a subset of test suites
echo.
echo Usage:
echo.
echo build.cmd ^<all^|compiler^|coreclr^|pcls^|vs^|ci^|ci_part1^|ci_part2^|ci_part3^|build^|debug^>
echo.
echo No arguments default to 'ci' ( build all profiles, run all unit tests, cambridge Smoke, fsharpqa Smoke)
echo.
echo To specify multiple values, separate strings by comma
echo.
echo The example below run pcls, vs and qa:
echo.
echo build.cmd pcls,vs,debug
exit /b 1

:ARGUMENTS_OK

set BUILD_NET40=1
set BUILD_CORECLR=0
set BUILD_PORTABLE7=0
set BUILD_PORTABLE47=0
set BUILD_PORTABLE78=0
set BUILD_PORTABLE259=0
set BUILD_VS=1
set TEST_NET40=0
set TEST_CORECLR=0
set TEST_PORTABLE=0
set TEST_VS=0
set TEST_CAMBRIDGE_SUITE=0
set CONF_CAMBRIDGE_SUITE=
set TEST_QA_SUITE=0
set CONF_QA_SUITE=
set BUILD_CONFIG=Release
set BUILD_CONFIG_LOWER=release

setlocal enableDelayedExpansion
set /a counter=0
for /l %%x in (1 1 9) do (
    set /a counter=!counter!+1
    call :SET_CONFIG %%!counter! "!counter!"
)
setlocal disableDelayedExpansion
echo.
echo.

goto :MAIN

:SET_CONFIG
set ARG=%~1

if "%RestorePackages%"=="" ( 
    set RestorePackages=true 
)

if "%ARG%" == "1" if "%2" == "" (
    set ARG=ci
)

if "%2" == "" if not "%ARG%" == "ci" goto :EOF

echo Parse argument %ARG%

if /i '%ARG%' == 'compiler' (
    set TEST_NET40=1
)

if /i '%ARG%' == 'pcls' (
    set BUILD_PORTABLE47=1
    set TEST_PORTABLE47=1
    set BUILD_PORTABLE7=1
    set TEST_PORTABLE7=1
    set BUILD_PORTABLE78=1
    set TEST_PORTABLE78=1
    set BUILD_PORTABLE259=1
    set TEST_PORTABLE259=1
)

if /i '%ARG%' == 'vs' (
    set BUILD_VS=1
    set TEST_VS=1
)

if /i '%ARG%' == 'all' (
    set BUILD_CORECLR=1
    set BUILD_PORTABLE47=1
    set BUILD_PORTABLE7=1
    set BUILD_PORTABLE78=1
    set BUILD_PORTABLE259=1
    set BUILD_VS=1
    set TEST_NET40=1
    set TEST_CORECLR=1
    set TEST_PORTABLE47=1
    set TEST_PORTABLE7=1
    set TEST_PORTABLE78=1
    set TEST_PORTABLE259=1
    set TEST_VS=1
    set TEST_CAMBRIDGE_SUITE=1
    set TEST_QA_SUITE=1
)

REM Same as 'all' but smoke testing only
if /i '%ARG%' == 'ci' (
    set BUILD_CORECLR=1
    set BUILD_PORTABLE47=1
    set BUILD_PORTABLE7=1
    set BUILD_PORTABLE78=1
    set BUILD_PORTABLE259=1
    set BUILD_VS=1
    set TEST_NET40=1
    set TEST_CORECLR=1
    set TEST_PORTABLE=1
    set TEST_VS=1
    set TEST_CAMBRIDGE_SUITE=1
    set CONF_CAMBRIDGE_SUITE=Smoke
    set TEST_QA_SUITE=1
    set CONF_QA_SUITE=Smoke
)

REM These divide 'ci' into three chunks which can be done in parallel

if /i '%ARG%' == 'ci_part1' (
    set BUILD_PORTABLE47=1
    set BUILD_PORTABLE7=1
    set BUILD_PORTABLE78=1
    set BUILD_PORTABLE259=1
    set BUILD_VS=1
    set TEST_NET40=1
    set TEST_PORTABLE=1
    set TEST_VS=1
)

if /i '%ARG%' == 'ci_part2' (
    set TEST_CAMBRIDGE_SUITE=1
    set CONF_CAMBRIDGE_SUITE=Smoke
)


if /i '%ARG%' == 'ci_part3' (
    set TEST_QA_SUITE=1
    set CONF_QA_SUITE=Smoke
)

if /i '%ARG%' == 'ci_part4' (
    set TEST_CORECLR=1
)

if /i '%ARG%' == 'smoke' (
    set TEST_CORECLR=1
    set TEST_CAMBRIDGE_SUITE=1
    set CONF_CAMBRIDGE_SUITE=Smoke
    set TEST_QA_SUITE=1
    set CONF_QA_SUITE=Smoke
)

if /i '%ARG%' == 'debug' (
    set BUILD_CONFIG=Debug
    set BUILD_CONFIG_LOWER=debug
)

if /i '%ARG%' == 'build' (
    set TEST_NET40=0
    set TEST_CORECLR=0
    set TEST_PORTABLE47=0
    set TEST_PORTABLE7=0
    set TEST_PORTABLE78=0
    set TEST_PORTABLE259=0
    set TEST_VS=0
    set TEST_CAMBRIDGE_SUITE=0
    set TEST_QA_SUITE=0
)

goto :EOF

:MAIN

REM after this point, ARG variable should not be used, use only BUILD_* or TEST_*

echo Build/Tests configuration:
echo.
echo BUILD_NET40=%BUILD_NET40%
echo BUILD_PORTABLE47=%BUILD_PORTABLE47%
echo BUILD_PORTABLE7=%BUILD_PORTABLE7%
echo BUILD_PORTABLE78=%BUILD_PORTABLE78%
echo BUILD_PORTABLE259=%BUILD_PORTABLE259%
echo BUILD_VS=%BUILD_VS%
echo.
echo TEST_NET40=%TEST_NET40%
echo TEST_PORTABLE=%TEST_PORTABLE%
echo TEST_VS=%TEST_VS%
echo TEST_CAMBRIDGE_SUITE=%TEST_CAMBRIDGE_SUITE%
echo CONF_CAMBRIDGE_SUITE=%CONF_CAMBRIDGE_SUITE%
echo TEST_QA_SUITE=%TEST_QA_SUITE%
echo CONF_QA_SUITE=%CONF_QA_SUITE%
echo BUILD_CONFIG=%BUILD_CONFIG%
echo BUILD_CONFIG_LOWER=%BUILD_CONFIG_LOWER%
echo.

@echo on

set APPVEYOR_CI=1

:: Check prerequisites
if not '%VisualStudioVersion%' == '' goto vsversionset
if exist "%VS140COMNTOOLS%..\ide\devenv.exe" set VisualStudioVersion=14.0
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\common7\ide\devenv.exe" set VisualStudioVersion=14.0
if exist "%ProgramFiles%\Microsoft Visual Studio 14.0\common7\ide\devenv.exe" set VisualStudioVersion=14.0
if not '%VisualStudioVersion%' == '' goto vsversionset
if exist "%VS120COMNTOOLS%..\ide\devenv.exe" set VisualStudioVersion=12.0
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\common7\ide\devenv.exe" set VisualStudioVersion=12.0
if exist "%ProgramFiles%\Microsoft Visual Studio 12.0\common7\ide\devenv.exe" set VisualStudioVersion=12.0

:vsversionset
if '%VisualStudioVersion%' == '' echo Error: Could not find an installation of Visual Studio && goto :failure

if exist "%ProgramFiles(x86)%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe" set _msbuildexe="%ProgramFiles(x86)%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe"
if exist "%ProgramFiles%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe"      set _msbuildexe="%ProgramFiles%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe"
if not exist %_msbuildexe% echo Error: Could not find MSBuild.exe. && goto :failure

:: See <http://www.appveyor.com/docs/environment-variables>
if defined APPVEYOR (
    rem See <http://www.appveyor.com/docs/build-phase>
    if exist "C:\Program Files\AppVeyor\BuildAgent\Appveyor.MSBuildLogger.dll" (
	rem HACK HACK HACK
	set _msbuildexe=%_msbuildexe% /logger:"C:\Program Files\AppVeyor\BuildAgent\Appveyor.MSBuildLogger.dll"
    )
)

set _ngenexe="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\ngen.exe"
if not exist %_ngenexe% echo Error: Could not find ngen.exe. && goto :failure

%_ngenexe% install .\.nuget\NuGet.exe 

.\.nuget\NuGet.exe restore packages.config -PackagesDirectory packages -ConfigFile .nuget\nuget.config
@if ERRORLEVEL 1 echo Error: Nuget restore failed  && goto :failure

set DOTNET_HOME  .\packages\dotnet

rem check to see if the dotnet cli tool exists
set _dotnetexe=".\packages\dotnet\bin\dotnet.exe"
if not exist %_dotnetexe% (
    echo Error: Could not find %_dotnetexe%.
    rem do zipfile install nonsense
    if not exist packages ( md packages )
    if exist packages\dotnet ( rd packages /s /q )
    powershell.exe -executionpolicy unrestricted -command .\scripts\install-dotnetcli.ps1 https://dotnetcli.blob.core.windows.net/dotnet/dev/Binaries/Latest/dotnet-win-x64.latest.zip packages
    @if ERRORLEVEL 1 echo Error: fetch dotnetcli failed && goto :failure
)

pushd .\lkg & ..\%_dotnetexe% restore project.json &popd
@if ERRORLEVEL 1 echo Error: dotnet restore failed  && goto :failure
pushd .\lkg & ..\%_dotnetexe% publish project.json -f dnxcore50 -r win7-x64 -o ..\packages\lkg &popd
@if ERRORLEVEL 1 echo Error: dotnet publish failed  && goto :failure

rem rename fsc and coreconsole to allow fsc.exe to to start compiler
pushd .\packages\lkg & ren fsc.exe fsc.dll & popd
copy .\packages\lkg\coreconsole.exe .\packages\lkg\fsc.exe

rem rename fsi and coreconsole to allow fsi.exe to to start interative
pushd .\packages\lkg & ren fsi.exe fsi.dll & popd
copy .\packages\lkg\coreconsole.exe .\packages\lkg\fsi.exe

:: Build
%_msbuildexe% src\fsharp-proto-build.proj
@if ERRORLEVEL 1 echo Error: compiler proto build failed && goto :failure

%_ngenexe% install Proto\net40\bin\fsc-proto.exe
@if ERRORLEVEL 1 echo Error: NGen of proto failed  && goto :failure

%_msbuildexe% src/fsharp-library-build.proj /p:Configuration=%BUILD_CONFIG%
@if ERRORLEVEL 1 echo Error: library build failed && goto :failure

%_msbuildexe% src/fsharp-compiler-build.proj /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: compiler build failed && goto :failure

if '%BUILD_CORECLR%' == '1' (
		%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=coreclr /p:Configuration=%BUILD_CONFIG% /p:RestorePackages=%RestorePackages%
		@if ERRORLEVEL 1 echo Error: library coreclr build failed && goto :failure
)

if '%BUILD_CORECLR%' == '1' (
		%_msbuildexe% src/fsharp-compiler-build.proj /p:TargetFramework=coreclr /p:Configuration=%BUILD_CONFIG% /p:RestorePackages=%RestorePackages%
		@if ERRORLEVEL 1 echo Error: compiler coreclr build failed && goto :failure
)

if '%BUILD_PORTABLE7%' == '1' (
		%_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable7 /p:Configuration=%BUILD_CONFIG%
		@if ERRORLEVEL 1 echo Error: library portable7 build failed && goto :failure
)

if '%BUILD_PORTABLE47%' == '1' (
    %_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable47 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library portable47 build failed && goto :failure
)

if '%BUILD_PORTABLE78%' == '1' (
    %_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable78 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library portable78 build failed && goto :failure
)

if '%BUILD_PORTABLE259%' == '' (
    %_msbuildexe% src/fsharp-library-build.proj /p:TargetFramework=portable259 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library portable259 build failed && goto :failure
)

if '%TEST_NET40%' == '1' (
    %_msbuildexe% src/fsharp-compiler-unittests-build.proj /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: compiler unittests build failed && goto :failure

    %_msbuildexe% src/fsharp-library-unittests-build.proj /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library unittests build failed && goto :failure
)

if '%TEST_PORTABLE47%' == '1' (
  	%_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable47 /p:Configuration=%BUILD_CONFIG%
	  @if ERRORLEVEL 1 echo Error: library unittests build failed portable47 && goto :failure
)

if '%TEST_PORTABLE7%' == '1' (
    %_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable7 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library unittests build failed portable7 && goto :failure
)

if '%TEST_PORTABLE78%' == '1' (
    %_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable78 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library unittests build failed portable78 && goto :failure
)

if '%TEST_PORTABLE259%' == '1' (
    %_msbuildexe% src/fsharp-library-unittests-build.proj /p:TargetFramework=portable259 /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: library unittests build failed portable259 && goto :failure
)

if '%BUILD_VS%' == '1' (
    %_msbuildexe% VisualFSharp.sln /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: VS integration build failed && goto :failure
)

if '%TEST_VS%' == '1' (
    %_msbuildexe% vsintegration\fsharp-vsintegration-unittests-build.proj /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: VS integration unit tests build failed && goto :failure
)

@echo on
call src\update.cmd %BUILD_CONFIG_LOWER% -ngen

REM Remove lingering copies of the OSS FSharp.Core from the GAC
gacutil /u "FSharp.Core, Version=4.4.1.9055, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, processorArchitecture=MSIL"

REM This clobbers the installed F# SDK on the machine
REM call vsintegration\update-vsintegration.cmd %BUILD_CONFIG_LOWER%
pushd tests

@echo on
call BuildTestTools.cmd %BUILD_CONFIG_LOWER% 
@if ERRORLEVEL 1 echo Error: 'BuildTestTools.cmd %BUILD_CONFIG_LOWER%' failed && goto :failure

@echo on

if '%TEST_CAMBRIDGE_SUITE%' == '1' (
    set FSHARP_TEST_SUITE_USE_NUNIT_RUNNER=true

    %_msbuildexe% fsharp\fsharp.tests.fsproj /p:Configuration=%BUILD_CONFIG%
    @if ERRORLEVEL 1 echo Error: fsharp cambridge tests for nunit failed && goto :failure

    call RunTests.cmd %BUILD_CONFIG_LOWER% fsharp %CONF_CAMBRIDGE_SUITE%
    @if ERRORLEVEL 1 type testresults\fsharp_failures.log && echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% fsharp %CONF_CAMBRIDGE_SUITE%' failed && goto :failure
    set FSHARP_TEST_SUITE_USE_NUNIT_RUNNER=
)

if '%TEST_QA_SUITE%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% fsharpqa %CONF_QA_SUITE%
    @if ERRORLEVEL 1 type testresults\fsharpqa_failures.log && echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% fsharpqa %CONF_QA_SUITE%' failed && goto :failure
)

if '%TEST_NET40%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% compilerunit
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% compilerunit' failed && goto :failure

    if '%TEST_PORTABLE%' == '1' (
        call RunTests.cmd release coreunitall
        @if ERRORLEVEL 1 echo Error: 'RunTests.cmd release coreunitall' failed && goto :failure
    )

    call RunTests.cmd %BUILD_CONFIG_LOWER% coreunit
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunit' failed && goto :failure
)

if '%TEST_PORTABLE47%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable47
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable47' failed && goto :failure
)

if '%TEST_PORTABLE7%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable7
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable7' failed && goto :failure
)

if '%TEST_PORTABLE78%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable78
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable78' failed && goto :failure
)

if '%TEST_PORTABLE259%' == '1' (
    call RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable259
    @if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunitportable259' failed && goto :failure
)

if '%TEST_CORECLR%' == '1' (
		call RunTests.cmd release fsharp coreclr
		@if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreclr' failed && goto :failure

		call RunTests.cmd release coreunitcoreclr
		@if ERRORLEVEL 1 echo Error: 'RunTests.cmd %BUILD_CONFIG_LOWER% coreunit' failed && goto :failure
)

popd

goto :eof

:failure
exit /b 1
