@if "%_echo%"=="" echo off
setlocal
set _SCRIPT_FILE=%~f0

@"C:\PROGRA~1\Mono-2.8\bin\mono.exe" %MONO_OPTIONS% %_SCRIPT_FILE:-mono.bat=.exe% %*
endlocal
