@if "%_echo%"=="" echo off

setlocal
REM Configure the sample, i.e. where to find the F# compiler and C# compiler.
if EXIST build.ok DEL /f /q build.ok

call %~d0%~p0..\..\..\config.bat

if NOT "%FSC:NOTAVAIL=X%" == "%FSC%" ( 
  REM Skipping test for FSI.EXE
  goto Skip
)

REM **************************
IF NOT EXIST "%FSCOREDLL20PATH%" goto :Skip20
  "%FSC%" -o:test-2.0.exe -r:"%FSCOREDLL20PATH%" -r:2.0\mscorlib.dll --noframework --define:FX_AT_LEAST_2_0 test.fsx
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" test-2.0.exe
  @if ERRORLEVEL 1 goto Error

  "%FSC%" -o:test-2.0b.exe -r:"%FSCOREDLL20PATH%" -r:2.0\mScorlib.DlL --noframework --define:FX_AT_LEAST_2_0 test.fsx
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" test-2.0b.exe
  @if ERRORLEVEL 1 goto Error

  "%FSC%" -o:test-3.5.exe -r:"%FSCOREDLL20PATH%" -r:3.5\mscorlib.dll --noframework --define:FX_AT_LEAST_2_0 --define:FX_AT_LEAST_3_5 test.fsx
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" test-3.5.exe
  @if ERRORLEVEL 1 goto Error

:Skip20

IF NOT EXIST "%FSHARP_HOME%\Retail\Silverlight\2.0\bin\FSharp.Core.dll" goto :SkipSL
 "%FSC%" -o:test-Silverlight-2.0.exe --noframework -r:Silverlight2.0.30523.8\mscorlib.dll -r:%FSHARP_HOME%\Retail\Silverlight\2.0\bin\FSharp.Core.dll --define:SILVERLIGHT_AT_LEAST_2_0 test.fsx
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" test-Silverlight-2.0.exe
  @if ERRORLEVEL 1 goto Error

:SkipSL


:Ok
echo Built fsharp %~n0 ok.
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

