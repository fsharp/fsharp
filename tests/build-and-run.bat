@if "%_echo%"=="" echo off

setlocal
set _ScriptHome=%~dp0%

if "%_UNATTENDEDLOG%"== "" set _UNATTENDEDLOG=%~dp0%\build-and-run.log
echo Start: >_UNATTENDEDLOG

if exist tdirs (
    for /f %%i in (tdirs) do ( 
        if exist "%%i" (
            pushd %%i
            echo **************************************************
            cd
            cd >> %_UNATTENDEDLOG%
            echo **************************************************
            call %_ScriptHome%\build-and-run.bat
            if ERRORLEVEL 1 goto Exit
            popd
        )
    )
)

if NOT exist tdirs (
    if exist build.bat (
        call .\build.bat
        if ERRORLEVEL 1 exit /b 1
    )
    
    if exist run.bat (
        call .\run.bat
        if ERRORLEVEL 1 exit /b 1
    ) 

    if NOT exist build.bat (
        if NOT exist run.bat ( 
            echo FAILURE: build.bat and run.bat not found.  Check %CD%\..\tdirs
            call .\build.bat > NUL 2>&1
            if ERRORLEVEL 1 goto Error
        )
    )
)

:Exit
endlocal

exit /b %ERRORLEVEL%

:Error
call %_ScriptHome%\ChompErr.bat %ERRORLEVEL% %~f0
endlocal
exit /b %ERRORLEVEL%