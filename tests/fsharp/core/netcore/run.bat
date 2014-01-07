@if "%_echo%"=="" echo off


setlocal

call %~d0%~p0..\..\..\config.bat

set CONTROL_FAILURES_LOG=%~dp0\control_failures.log

.\ConsoleApplication1\bin\Debug\PortableTestEntry.exe
endlocal
exit /b %ERRORLEVEL%

:Skip
echo Skipped %~f0
endlocal
exit /b 0


