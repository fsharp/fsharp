@if "%_echo%"=="" echo off
 
setlocal
set _ScriptHome=%~dp0%

if exist tdirs (
 for /f %%i in (tdirs) do ( 
  if exist %%i (
	pushd %%i
        echo **************************************************
        cd
        echo **************************************************
        call %_ScriptHome%\build.bat
	if ERRORLEVEL 1 goto Error
	popd
  )
 )
)

if NOT exist tdirs (
   if exist build.bat (
        call .\build.bat
	if ERRORLEVEL 1 goto Error
   ) 
)

echo Built fsharp ok.
:Ok
endlocal
exit /b 0

:Error
endlocal
exit /b %ERRORLEVEL%



