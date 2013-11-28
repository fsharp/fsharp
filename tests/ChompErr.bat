@if "%_echo%"=="" echo off
if /I %1 EQU 0 	( goto Skip )

if not defined _UNATTENDEDLOG ( goto Exit ) 

echo ERRORLEVEL=%1: in %2 >> %_UNATTENDEDLOG%
echo current directory is %CD%  >> %_UNATTENDEDLOG%
:LOG_ERRORS
if "%~3"=="" (
echo. >> %_UNATTENDEDLOG%
exit /b 0
) 
echo "%~3" >> %_UNATTENDEDLOG%
shift /3
goto LOG_ERRORS 


:Skip
exit /b 0

:Exit
echo ERRORLEVEL= %1 : in %2
echo current directory is %CD%
:ECHO_ERRORS
if "%~3"=="" (
echo 
exit /b 1
)
echo %~3
shift /3
goto ECHO_ERRORS 
