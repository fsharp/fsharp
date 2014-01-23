rem @if "%_echo%"=="" echo off

setlocal
call %~d0%~p0\..\..\..\config.bat

if EXIST provider-base.dll del provider-base.dll
if ERRORLEVEL 1 goto :Error

if EXIST provider.dll del provider.dll
if ERRORLEVEL 1 goto :Error

%CSC% /out:provider-base.dll /target:library CSharpBaseClasses.cs
if ERRORLEVEL 1 goto :Error

"%FSC%" /target:library /out:provider.dll /debug+ /r:provider-base.dll /optimize- ProvidedTypes-head.fsi ProvidedTypes-head.fs FieldAccess.fs
if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% /debug+ /r:provider.dll /r:provider-base.dll /optimize- test.fs
if ERRORLEVEL 1 goto Error

%PEVERIFY% test.exe
if ERRORLEVEL 1 goto Error

:Ok
echo. > build.ok
endlocal
exit /b 0

:Error
call %SCRIPT_ROOT%\ChompErr.bat  %ERRORLEVEL% %~f0
endlocal
exit /b %ERRORLEVEL%


