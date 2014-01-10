@if "%_echo%"=="" echo off

IF EXIST test.exe (
   echo Running test.exe to warm up SQL
   test.exe > nul 2> nul
)

call %~d0%~p0..\..\..\single-test-run.bat

exit /b %ERRORLEVEL%


