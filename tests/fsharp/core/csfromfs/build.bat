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

REM only a valid test if generics supported

  "%FSC%" %fsc_flags% -o:collections.exe -g collections.fs
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" collections.exe
  @if ERRORLEVEL 1 goto Error


REM **************************

REM only a valid test if generics supported

  %CSC% /nologo /target:library /out:classes-lib.dll classes.cs 
  @if ERRORLEVEL 1 goto Error

  "%FSC%" %fsc_flags% -r:classes-lib.dll -o:classes.exe -g classes.fs
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" classes.exe
  @if ERRORLEVEL 1 goto Error




REM **************************

REM only a valid test if generics supported
  %CSC% /nologo /target:library /out:byrefs-lib.dll byrefs.cs 
  @if ERRORLEVEL 1 goto Error

  "%FSC%" %fsc_flags% -r:byrefs-lib.dll -o:byrefs.exe -g byrefs.fs
  @if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" byrefs.exe
  @if ERRORLEVEL 1 goto Error

REM **************************

%CSC% /nologo /target:library /out:methods-lib.dll methods.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:methods-lib.dll -o:methods.exe -g methods.fs
@if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" methods.exe
  @if ERRORLEVEL 1 goto Error


REM **************************

%CSC% /nologo /target:library /out:events-lib.dll events.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% --progress -r:events-lib.dll -o:events.exe -g events.fs
@if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" events.exe
  @if ERRORLEVEL 1 goto Error


REM **************************

%CSC% /nologo /target:library /out:indexers-lib.dll indexers.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:indexers-lib.dll -o:indexers.exe -g indexers.fs
@if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" indexers.exe
  @if ERRORLEVEL 1 goto Error


REM **************************

%CSC% /nologo /target:library /out:fields-lib.dll fields.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:fields-lib.dll -o:fields.exe -g fields.fs
@if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" fields.exe 
  @if ERRORLEVEL 1 goto Error

REM **************************

%CSC% /nologo /target:library /out:properties-lib.dll properties.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:properties-lib.dll -o:properties.exe -g properties.fs
@if ERRORLEVEL 1 goto Error


  "%PEVERIFY%" properties.exe 
  @if ERRORLEVEL 1 goto Error
REM **************************

%CSC% /nologo /target:library /out:nested-types-cslib.dll nested-types.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:nested-types-cslib.dll -o:nested-types.exe -g nested-types.fs
@if ERRORLEVEL 1 goto Error

"%PEVERIFY%" nested-types.exe
@if ERRORLEVEL 1 goto Error

set old_fsc_flags=%fsc_flags%

set fsc_flags=%fsc_flags% -r:nested-types-cslib.dll
call ..\..\single-neg-test.bat nested-types-error
set fsc_flags=%old_fsc_flags%

@if ERRORLEVEL 1 goto Error

  
REM **************************
REM Dev11 bug 47156

%CSC% /nologo /target:library /out:ProtectedMethodOpt_lib.dll ProtectedMethodOpt.cs 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:ProtectedMethodOpt_lib.dll -o:ProtectedMethodOpt.exe --optimize+ ProtectedMethodOpt.fs
@if ERRORLEVEL 1 goto Error


  "%PEVERIFY%" ProtectedMethodOpt.exe 
  @if ERRORLEVEL 1 goto Error

REM **************************
REM Excel interop

REM only a valid test if generics supported

REM Don't try to compile without Office installed, will fail verification on Dev10

set OfficeExists=0
@ECHO Probing for well-known Office/Excel locations. It is ok to see errors like "The system was unable to find the specified registry key..."
REG>NUL >2&1 QUERY HKLM\SOFTWARE\Microsoft\Office\11.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
REG>NUL QUERY HKLM\SOFTWARE\Microsoft\Office\12.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
REG>NUL QUERY HKLM\SOFTWARE\Microsoft\Office\14.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
REG>NUL QUERY HKLM\SOFTWARE\Wow6432Node\Microsoft\Office\11.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
REG>NUL QUERY HKLM\SOFTWARE\Wow6432Node\Microsoft\Office\12.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
REG>NUL QUERY HKLM\SOFTWARE\Wow6432Node\Microsoft\Office\14.0\Excel\InstallRoot
IF "%ERRORLEVEL%"=="0" set OfficeExists=1&&goto :EndCheckForOffice
:EndCheckForOffice

IF %OfficeExists%==1 (
  "%FSC%" %fsc_flags% -r:Excel.dll -o:optional.exe -g optional.fs
  @if ERRORLEVEL 1 goto Error
   "%PEVERIFY%" optional.exe
  @if ERRORLEVEL 1 goto Error
)


REM **************************
REM Multi-module case

%CSC% /nologo /target:module /out:events-lib.netmodule events.cs 
@if ERRORLEVEL 1 goto Error
%CSC% /nologo /target:module /out:properties-lib.netmodule properties.cs 
@if ERRORLEVEL 1 goto Error
%CSC% /nologo /target:module /out:fields-lib.netmodule fields.cs 
@if ERRORLEVEL 1 goto Error
%CSC% /nologo /target:module /out:indexers-lib.netmodule indexers.cs 
@if ERRORLEVEL 1 goto Error
"%ALINK%" /target:library /out:multi-module-lib.dll events-lib.netmodule properties-lib.netmodule fields-lib.netmodule indexers-lib.netmodule 
@if ERRORLEVEL 1 goto Error

"%FSC%" %fsc_flags% -r:multi-module-lib.dll -o:multi-module.exe -g properties.fs fields.fs indexers.fs events.fs 
@if ERRORLEVEL 1 goto Error

  "%PEVERIFY%" multi-module.exe
  @if ERRORLEVEL 1 goto Error


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

