@if "%_echo%"=="" echo off

set _SCRIPT_DRIVE=%~d0
set _SCRIPT_PATH=%~p0
set _SCRIPT_ROOT=%_SCRIPT_DRIVE%%_SCRIPT_PATH%

set PATH=%ProgramFiles(x86)%\Microsoft SDKs\Windows\v8.1A\bin\NETFX 4.5.1 Tools;%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\Common7\IDE;%_SCRIPT_ROOT%\..\lib\debug;%SystemRoot%\Microsoft.NET\Framework\v4.0.30319;%PATH%

set FSHARP_HOME=%_SCRIPT_ROOT%

set TARGETFSHARP=VS2010

color 6
title "%_SCRIPT_ROOT% Test Environment"
