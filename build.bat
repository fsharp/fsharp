@echo off

:: Try find installation path of VS2017 with vswhere.exe
if "%VS150COMNTOOLS%" EQU "" if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\" (
    for /f "usebackq delims=" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -prerelease -property installationPath`) do set VS_INSTALLATION_PATH=%%i
)

if "%VS_INSTALLATION_PATH%" NEQ "" (
    call "%VS_INSTALLATION_PATH%\Common7\Tools\VsDevCmd.bat"
)

:: If there's no installation of VS2017 or VS2017 Preview, use the build tools
if "%VS150COMNTOOLS%" EQU "" if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\BuildTools\Common7\Tools\VsDevCmd.bat" (
    call "%ProgramFiles(x86)%\Microsoft Visual Studio\2017\BuildTools\Common7\Tools\VsDevCmd.bat"
)

echo.
echo Environment
set
echo.
echo.
:: Check prerequisites
if not "%VisualStudioVersion%" == "" goto vsversionset
if exist "%VS150COMNTOOLS%\..\ide\devenv.exe" set VisualStudioVersion=15.0
if not "%VisualStudioVersion%" == "" goto vsversionset

if not "%VisualStudioVersion%" == "" goto vsversionset
if exist "%VS150COMNTOOLS%\..\..\ide\devenv.exe" set VisualStudioVersion=15.0
if not "%VisualStudioVersion%" == "" goto vsversionset

if exist "%VS140COMNTOOLS%\..\ide\devenv.exe" set VisualStudioVersion=14.0
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\common7\ide\devenv.exe" set VisualStudioVersion=14.0
if exist "%ProgramFiles%\Microsoft Visual Studio 14.0\common7\ide\devenv.exe" set VisualStudioVersion=14.0
if not "%VisualStudioVersion%" == "" goto vsversionset

if exist "%VS120COMNTOOLS%\..\ide\devenv.exe" set VisualStudioVersion=12.0
if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\common7\ide\devenv.exe" set VisualStudioVersion=12.0
if exist "%ProgramFiles%\Microsoft Visual Studio 12.0\common7\ide\devenv.exe" set VisualStudioVersion=12.0

:vsversionset
if "%VisualStudioVersion%" == "" echo Error: Could not find an installation of Visual Studio && goto :failure

:: Check prerequisites
if exist "%VS150COMNTOOLS%\..\..\MSBuild\15.0\Bin\MSBuild.exe" (
    set _msbuildexe="%VS150COMNTOOLS%\..\..\MSBuild\15.0\Bin\MSBuild.exe"
    goto :havemsbuild
)
if exist "%ProgramFiles(x86)%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe" (
    set _msbuildexe="%ProgramFiles(x86)%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe"
    goto :havemsbuild
)
if exist "%ProgramFiles%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe" (
    set _msbuildexe="%ProgramFiles%\MSBuild\%VisualStudioVersion%\Bin\MSBuild.exe"
    goto :havemsbuild
)
echo Error: Could not find MSBuild.exe. && goto :failure
goto :eof

:havemsbuild

echo "_msbuildexe=%_msbuildexe%"
set msbuildflags=/maxcpucount
set _ngenexe="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\ngen.exe"
if not exist %_ngenexe% echo Note: Could not find ngen.exe. 

set REGEXE32BIT=reg.exe
if not "%OSARCH%"=="x86" set REGEXE32BIT=%WINDIR%\syswow64\reg.exe

::See https://stackoverflow.com/a/17113667/111575 on 2^>NUL for suppressing the error "ERROR: The system was unable to find the specified registry key or value." from reg.exe, this fixes #3619
                                FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\NETFXSDK\4.6.2\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL')  DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\NETFXSDK\4.6.1\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL')  DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\NETFXSDK\4.6\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL')  DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows\v8.1A\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL') DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows\v8.0A\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL') DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows\v7.1\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL')  DO SET WINSDKNETFXTOOLS_x86=%%B
if "%WINSDKNETFXTOOLS_x86%"=="" FOR /F "tokens=2* delims=	 " %%A IN ('%REGEXE32BIT% QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows\v7.0A\WinSDK-NetFx40Tools" /v InstallationFolder 2^>NUL') DO SET WINSDKNETFXTOOLS_x86=%%B

set WINSDKNETFXTOOLS_x64=%WINSDKNETFXTOOLS_x86%x64\

set SN32="%WINSDKNETFXTOOLS_x86%sn.exe"
set SN64="%WINSDKNETFXTOOLS_x64%sn.exe"

rem Disable strong-name validation for binaries that are delay-signed with the microsoft key
echo %SN32% -q -Vr *,b03f5f7f11d50a3a
%SN32% -q -Vr *,b03f5f7f11d50a3a

if /i "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
    echo %SN64% -q -Vr *,b03f5f7f11d50a3a
    %SN64% -q -Vr *,b03f5f7f11d50a3a
)


::Build

%_ngenexe% install .\.nuget\NuGet.exe 

.\.nuget\NuGet.exe restore packages.config -PackagesDirectory packages -ConfigFile NuGet.Config
@if ERRORLEVEL 1 echo Error: Nuget restore failed  && goto :failure

%_ngenexe% install packages\FSharp.Compiler.Tools.4.1.27\tools\fsc.exe

set BUILD_NET40=1
set TEST_NET40_COREUNIT_SUITE=1

%_msbuildexe% src\fsharp-proto-build.proj /p:Configuration=Proto
@if ERRORLEVEL 1 echo Error: "%_msbuildexe% src\fsharp-proto-build.proj" failed  && goto :failure

%_ngenexe% install Proto\net40\bin\fsc.exe

%_msbuildexe% %msbuildflags% build-everything.proj /p:TargetDotnetProfile=net40 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: "%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetDotnetProfile=net40 /p:Configuration=Release" failed  && goto :failure


@echo "Finished"
goto :eof

:failure
exit /b 1
