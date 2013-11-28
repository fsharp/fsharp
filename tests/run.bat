@if "%_echo%"=="" echo off

setlocal
set _ScriptHome=%~dp0%

if exist tdirs (
 for /f %%i in (tdirs) do ( 
  if exist "%%i" (
	pushd %%i
        echo **************************************************
        cd
        echo **************************************************
        call %_ScriptHome%\run.bat
	if ERRORLEVEL 1 goto Exit
	popd
  )
 )
)

if NOT exist tdirs (
   if exist run.bat (
        call .\run.bat
	if ERRORLEVEL 1 goto Exit
   ) 
)

echo Ran ok.
:Exit
endlocal

exit /b %ERRORLEVEL%

