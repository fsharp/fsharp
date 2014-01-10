@if "%_echo%"=="" echo off

setlocal 
dir build.ok > NUL ) || (
  @echo 'build.ok' not found.
  goto :ERROR
)

call %~d0%~p0..\..\..\config.bat


REM **************************

  %CLIX% .\collections.exe
  if ERRORLEVEL 1 goto Error

REM **************************

%CLIX% .\events.exe
if ERRORLEVEL 1 goto Error


REM **************************

%CLIX% .\indexers.exe
if ERRORLEVEL 1 goto Error


REM **************************

%CLIX% .\fields.exe
if ERRORLEVEL 1 goto Error

REM **************************


  %CLIX% .\byrefs.exe
  if ERRORLEVEL 1 goto Error

REM **************************

%CLIX% .\methods.exe
if ERRORLEVEL 1 goto Error

REM **************************

%CLIX% .\properties.exe
if ERRORLEVEL 1 goto Error

REM **************************

  %CLIX% .\classes.exe
  if ERRORLEVEL 1 goto Error

REM **************************

  %CLIX% .\ProtectedMethodOpt.exe 
  if ERRORLEVEL 1 goto Error

:Ok
echo Ran fsharp %~f0 ok.
endlocal
exit /b 0

:Skip
echo Skipped %~f0
endlocal
exit /b 0


:Error
call %SCRIPT_ROOT%\ChompErr.bat %ERRORLEVEL% %~f0
endlocal
exit /b %ERRORLEVEL%
