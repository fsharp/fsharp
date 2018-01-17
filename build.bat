@echo off

:: Check prerequisites
set _msbuildexe="%ProgramFiles(x86)%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\14.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles(x86)%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% set _msbuildexe="%ProgramFiles%\MSBuild\12.0\Bin\MSBuild.exe"
if not exist %_msbuildexe% echo Error: Could not find MSBuild.exe.  Please see http://www.microsoft.com/en-us/download/details.aspx?id=40760. && goto :eof

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

.\.nuget\NuGet.exe restore packages.config -PackagesDirectory packages -ConfigFile .nuget\nuget.config
@if ERRORLEVEL 1 echo Error: Nuget restore failed  && goto :failure

%_ngenexe% install packages\FSharp.Compiler.Tools.4.1.27\tools\fsc.exe

set BUILD_NET40=1
set TEST_NET40_COREUNIT_SUITE=1

%_msbuildexe% src\fsharp-proto-build.proj /p:Configuration=Proto
@if ERRORLEVEL 1 echo Error: "%_msbuildexe% src\fsharp-proto-build.proj" failed  && goto :failure

%_ngenexe% install Proto\net40\bin\fsc-proto.exe

%_msbuildexe% %msbuildflags% build-everything.proj /p:TargetDotnetProfile=net40 /p:Configuration=Release
@if ERRORLEVEL 1 echo Error: "%_msbuildexe% %msbuildflags% src\fsharp-library-build.proj /p:TargetDotnetProfile=net40 /p:Configuration=Release" failed  && goto :failure


@echo "Finished"
goto :eof

:failure
exit /b 1
