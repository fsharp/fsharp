@if "%_echo%"=="" echo off

setlocal
REM Configure the sample, i.e. where to find the F# compiler and C# compiler.

call %~d0%~p0..\..\..\config.bat

"%FSC%" --out:provider.dll -a provider.fs
if errorlevel 1 goto :Error

"%FSC%" --out:providerDesigner.dll -a providerDesigner.fsx

call %~d0%~p0..\..\single-test-build.bat
goto :Ok

:Ok
echo. > build.ok
endlocal
exit /b 0

:Error
call %SCRIPT_ROOT%\ChompErr.bat  %ERRORLEVEL% %~f0
endlocal
exit /b %ERRORLEVEL%
