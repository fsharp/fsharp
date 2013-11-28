@if "%_echo%"=="" echo off

setlocal

REM Configure the sample, i.e. where to find the F# compiler and C# compiler.
if EXIST build.ok DEL /f /q build.ok

call %~d0%~p0..\..\..\config.bat

if NOT "%FSC:NOTAVAIL=X%" == "%FSC%" ( 
  ECHO Skipping test for FSI.EXE
  goto Skip
)


rem fsc.exe building


    "%FSC%" %fsc_flags% -o:test.exe -g test.fsx
    @if ERRORLEVEL 1 goto Error

    "%PEVERIFY%" test.exe 
    @if ERRORLEVEL 1 goto Error

    "%FSC%" %fsc_flags% --optimize -o:test--optimize.exe -g test.fsx
    @if ERRORLEVEL 1 goto Error

    "%PEVERIFY%" test--optimize.exe 
    @if ERRORLEVEL 1 goto Error


    "%FSC%" %fsc_flags% -a -o:testlib.dll -r:System.Data.Entity.dll -r:FSharp.Data.TypeProviders.dll -g test-part1.fs
    REM NOTE!!! We expect an error here, because entity framework types can't be used across assembly boundaries
    @if %ERRORLEVEL% EQU 0 goto Error

    ECHO good, got an error.

    REM Disabled because cross-assembly entity framework doesn't yet work, 
	REM "%FSC%" %fsc_flags% -r:testlib.dll -o:testapp.exe -r:System.Data.Entity.dll -g test-part2.fs 
    REM @if ERRORLEVEL 1 goto Error

    REM "%PEVERIFY%" testapp.exe 
    REM @if ERRORLEVEL 1 goto Error


    "%FSC%" %fsc_flags% -o:testtwoparts.exe -r:System.Data.Entity.dll --define:INTERNAL -r:FSharp.Data.TypeProviders.dll -g test-part1.fs test-part2.fs 
    @if ERRORLEVEL 1 goto Error

    "%PEVERIFY%" testtwoparts.exe 
    @if ERRORLEVEL 1 goto Error


    "%FSC%" %fsc_flags% -o:testtwoparts-not-internal.exe -r:System.Data.Entity.dll -r:FSharp.Data.TypeProviders.dll -g test-part1.fs test-part2.fs 
    REM NOTE!!! We expect an error here, because entity framework types can't be used across assembly boundaries
    @if %ERRORLEVEL% EQU 0 goto Error

    ECHO good, got an error.

:Ok
echo Built fsharp %~f0 ok.
echo. > build.ok
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

