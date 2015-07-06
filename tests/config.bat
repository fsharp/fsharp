@if "%_echo%"=="" echo off

set _SCRIPT_DRIVE=%~d0
set _SCRIPT_PATH=%~p0
set SCRIPT_ROOT=%_SCRIPT_DRIVE%%_SCRIPT_PATH%

if not defined FSHARP_HOME set FSHARP_HOME=%SCRIPT_ROOT%..\..

for /f %%i in ("%FSHARP_HOME%") do set FSHARP_HOME=%%~fi

REM Do we know where fsc.exe is?
IF DEFINED FSCBinPath goto :FSCBinPathFound
FOR /F "delims=" %%i IN ('where fsc.exe') DO IF NOT DEFINED FSCBinPath SET FSCBinPath=%%~dpi
:FSCBinPathFound

SET CLIFLAVOUR=cli\4.5

if not defined FSCBinPath set FSCBinPath=%FSHARP_HOME%\Retail\%CLIFLAVOUR%\bin

if not exist "%FSCBinPath%\fsc.exe" echo %FSCBinPath%\fsc.exe not found. Assume that this is a lab QA run machine, with product installed.
if not exist "%FSCBinPath%\fsc.exe" call :SetFSCBinPath45
 
if not exist "%FSCBinPath%\fsc.exe" echo %FSCBinPath%\fsc.exe still not found. Assume that user has added it to path somewhere

REM strip extra file separators and make short-name in case the the location is "Program Files" (how i hate spaces in file names!)
if defined FSCBinPath for /f "delims=" %%l in ("%FSCBinPath%") do set FSCBinPath=%%~fsl

REM add %FSCBinPath% to path only if not already there. Otherwise, the path keeps growing.
echo %path%; | find /i "%FSCBinPath%;" > NUL
if ERRORLEVEL 1    set PATH=%PATH%;%FSCBinPath%

if "%FSDIFF%"=="" set FSDIFF=%SCRIPT_ROOT%fsharpqa\testenv\bin\diff.exe
if not exist "%FSDIFF%" echo FSDIFF not found at expected path of %fsdiff% && exit /b 1

rem check if we're already configured, if not use the configuration from the last line of the config file
if "%fsc%"=="" ( 
    set csc_flags=/nologo
    set fsiroot=fsi
)

if not defined ALINK  set ALINK=al.exe
if not defined CSC    set CSC=csc.exe %csc_flags%

REM SDK Dependencires.
if not defined ILDASM   set ILDASM=ildasm.exe
if not defined GACUTIL   set GACUTIL=gacutil.exe
if not defined PEVERIFY set PEVERIFY=peverify.exe
if not defined RESGEN   set RESGEN=resgen.exe

if "%fsiroot%" == "" ( set fsiroot=fsi)

REM == Test strategy: if we are on a 32bit OS => use fsi.exe
REM ==                if we are on a 64bit OS => use fsiAnyCPU.exe
REM == This way we get coverage of both binaries without having to
REM == double the test matrix. Note that our nightly automation
REM == always cover x86 and x64... so we won't miss much. There
REM == is an implicit assumption that the CLR will do it's job
REM == to make an FSIAnyCPU.exe behave as FSI.exe on a 32bit OS.
REM == On 64 bit machines ensure that we run the 64 bit versions of tests too.
SET OSARCH=%PROCESSOR_ARCHITECTURE%
IF NOT "%PROCESSOR_ARCHITEW6432%"=="" SET OSARCH=%PROCESSOR_ARCHITEW6432%
IF "%fsiroot%"=="fsi" IF NOT "%OSARCH%"=="x86" (
    SET fsiroot=fsiAnyCPU
    set FSC_BASIC_64=FSC_BASIC_64
)

REM ---------------------------------------------------------------
REM If we set a "--cli-version" flag anywhere in the flags then assume its v1.x
REM and generate a config file, so we end up running the test on the right version
REM of the CLR.  Also modify the CORSDK used.
REM
REM Use CLR 1.1 at a minimum since 1.0 is not installed on most of my machines

REM otherwise assume v2.0
REM TODO: we need to update this to be v2.0 or v3.5 and nothing else.

set fsc_flags=%fsc_flags% 

set CLR_SUPPORTS_GENERICS=true
set ILDASM=%ILDASM%
set GACUTIL=%GACUTIL%
set CLR_SUPPORTS_WINFORMS=true
set CLR_SUPPORTS_SYSTEM_WEB=true

REM ==
REM == F# v1.0 targets NetFx3.5 (i.e. NDP2.0)
REM == It is ok to hardcode the location, since this is not going to
REM == change ever. Well, if/when we target a different runtime we'll have
REM == to come and update this, but for now we MUST make sure we use the 2.0 stuff.
REM ==
REM == If we run on a 64bit machine (from a 64bit command prompt!), we use the 64bit
REM == CLR, but tweaking 'Framework' to 'Framework64'.
REM ==
set CORDIR=%SystemRoot%\Microsoft.NET\Framework\v2.0.50727\

set CORDIR40=
FOR /D %%i IN (%windir%\Microsoft.NET\Framework\v4.0.?????) do set CORDIR40=%%i
IF NOT "%CORDIR40%"=="" set CORDIR=%CORDIR40%

REM == Use the same runtime as our architecture
REM == ASSUMPTION: This could be a good or bad thing.
IF /I NOT "%PROCESSOR_ARCHITECTURE%"=="x86" set CORDIR=%CORDIR:Framework=Framework64%

REM ==
REM == Find out path to NDP SDK (on a standard F# v1.0 run, this should be one of:
REM == - NDP2.0 SDK (NetFx2.0 SDK)
REM == - WinSDK 6.0A (VS2008)
REM == - WinSDK 6.1 (Vista WinSDK)
REM == - WinSDK 7.0A (Dev10)
REM == - WinSDK 8.0A (Dev11)
REM == - WinSDK 8.1A (Dev12)
REM == ==> we need to peverify against NET 2.0 (F# VS2008) or 4.0/4.5/4.5.1 (F# Dev12)
REM ==
REM == Try Windows SDK 6.x or 7.x
@reg>NUL 2>&1 QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows" /v CurrentInstallFolder
IF ERRORLEVEL 1 goto :TryNDPSDK20

IF /I "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
    FOR /F "tokens=2*" %%A IN ('reg QUERY "HKLM\Software\Wow6432Node\Microsoft\Microsoft SDKs\Windows\v8.1A\WinSDK-NetFx40Tools" /v InstallationFolder') DO SET CORSDK=%%B
) ELSE (
    FOR /F "tokens=2*" %%A IN ('reg QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows\v8.1A\WinSDK-NetFx40Tools" /v InstallationFolder') DO SET CORSDK=%%B
)

IF "%CORSDK%"=="" FOR /F "tokens=2*" %%A IN ('reg QUERY "HKLM\Software\Microsoft\Microsoft SDKs\Windows" /v CurrentInstallFolder') DO SET CORSDK=%%BBin
IF NOT "%CORDIR40%"=="" IF EXIST "%CORSDK%\NETFX 4.0 Tools" set CORSDK=%CORSDK%\NETFX 4.0 Tools

REM == Fix up CORSDK for 64bit platforms...
IF /I "%PROCESSOR_ARCHITECTURE%"=="AMD64" SET CORSDK=%CORSDK%\x64
IF /I "%PROCESSOR_ARCHITECTURE%"=="IA64"  SET CORSDK=%CORSDK%\IA64
goto :DoneCORSDK

REM == Try NDP2.0 SDK
:TryNDPSDK20
@reg>NUL 2>&1 QUERY "HKLM\Software\Microsoft\.NETFramework" /v sdkInstallRootv2.0
IF NOT ERRORLEVEL 0 @echo NDPSDK Not Found!&&goto :TryNDPSDK20
FOR /F "tokens=2*" %%A IN ('reg QUERY "HKLM\Software\Microsoft\.NETFramework" /v sdkInstallRootv2.0') DO SET CORSDK=%%BBin
goto :DoneCORSDK

:DoneCORSDK

REM add powerpack to flags only if not already there. Otherwise, the variable can keep growing.
echo %fsc_flags% | find /i "powerpack"
if ERRORLEVEL 1 set fsc_flags=%fsc_flags% -r:System.Core.dll --nowarn:20

if not defined fsi_flags set fsi_flags=%fsc_flags:--define:COMPILED=% --define:INTERACTIVE --maxerrors:1 --abortonerror

echo %fsc_flags%; | find "--define:COMPILED" > NUL || (
    set fsc_flags=%fsc_flags% --define:COMPILED
)

if NOT "%fsc_flags:generate-config-file=X%"=="%fsc_flags%" ( 
    if NOT "%fsc_flags:clr-root=X%"=="%fsc_flags%" ( 
        set fsc_flags=%fsc_flags% --clr-root:%CORDIR%
    )
)

if "%CORDIR%"=="unknown" set CORDIR=

REM use short names in the path so you don't have to deal with the space in things like "Program Files"
for /f "delims=" %%I in ("%CORSDK%") do set CORSDK=%%~dfsI%
for /f "delims=" %%I in ("%CORDIR%") do set CORDIR=%%~dfsI%

set NGEN=

REM ==
REM == Set path to C# compiler. If we are NOT on NetFx4.0, try we prefer C# 3.5 to C# 2.0 
REM == This is because we have tests that reference System.Core.dll from C# code!
REM == (e.g. fsharp\core\fsfromcs)
REM ==
                        IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\csc.exe"                                          SET CSC="%CORDIR%\csc.exe" %csc_flags%
IF     "%CORDIR40%"=="" IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\..\V3.5\csc.exe"                                  SET CSC="%CORDIR%\..\v3.5\csc.exe" %csc_flags%

IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\ngen.exe"            SET NGEN=%CORDIR%\ngen.exe
IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\al.exe"              SET ALINK=%CORDIR%\al.exe

REM ==
REM == The logic here is: pick the latest msbuild
REM == If we are testing against NDP4.0, then don't try msbuild 3.5
REM ==
IF NOT "%CORSDK%"=="" IF EXIST "%CORSDK%\ildasm.exe"          SET ILDASM=%CORSDK%\ildasm.exe
IF NOT "%CORSDK%"=="" IF EXIST "%CORSDK%\gacutil.exe"         SET GACUTIL=%CORSDK%\gacutil.exe
IF NOT "%CORSDK%"=="" IF EXIST "%CORSDK%\peverify.exe"        SET PEVERIFY=%CORSDK%\peverify.exe
IF NOT "%CORSDK%"=="" IF EXIST "%CORSDK%\resgen.exe"          SET RESGEN=%CORSDK%\resgen.exe
IF NOT "%CORSDK%"=="" IF EXIST "%CORSDK%\al.exe"              SET ALINK=%CORSDK%\al.exe

IF NOT DEFINED FSC                                            SET FSC=fsc.exe
IF NOT DEFINED FSI                                            SET FSI=%fsiroot%.exe

IF DEFINED FSCBinPath IF EXIST "%FSCBinPath%\fsc.exe"   SET FSC=%FSCBinPath%\fsc.exe
IF DEFINED FSCBinPath IF EXIST "%FSCBinPath%\%fsiroot%.exe"   SET FSI=%FSCBinPath%\%fsiroot%.exe

REM == In Dev11 (layout setup), FSharp.Core.dll is not sitting next to fsc.exe
REM == so we provide an alternative location to look for it. Automation will check
REM == this value (which may or may not be defined) and decide to use it.
set FSCOREDLLPATH=
set FSCOREDLL20PATH=
call :GetFSCOREDLLPaths

IF EXIST "%FSCBinPath%\FSharp.Core.dll" set FSCOREDLLPATH=%FSCBinPath%
set FSCOREDLLPATH=%FSCOREDLLPATH%\FSharp.Core.dll

set FSCOREDLL20PATH=%FSCOREDLL20PATH%\FSharp.Core.dll
IF EXIST "%FSCBinPath%net20\FSharp.Core.dll" set FSCOREDLL20PATH=%FSCBinPath%net20\FSharp.Core.dll
IF EXIST "%FSCBinPath%Runtime\2.0\FSharp.Core.dll" set FSCOREDLL20PATH=%FSCBinPath%Runtime\2.0\FSharp.Core.dll

REM == Set standard flags for invoking powershell scripts
IF NOT DEFINED PSH_FLAGS SET PSH_FLAGS=-nologo -noprofile -executionpolicy bypass

if DEFINED _UNATTENDEDLOG exit /b 0

rem first see if we have got msbuild installed
if exist "%X86_PROGRAMFILES%\MSBuild\14.0\Bin\MSBuild.exe" SET MSBuildToolsPath=%X86_PROGRAMFILES%\MSBuild\14.0\Bin\
if not "%MSBuildToolsPath%" == "" goto done_MsBuildToolsPath

                        IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\msbuild.exe"         SET MSBuildToolsPath=%CORDIR%
IF     "%CORDIR40%"=="" IF NOT "%CORDIR%"=="" IF EXIST "%CORDIR%\..\V3.5\msbuild.exe" SET MSBuildToolsPath="%CORDIR%\..\V3.5\"

IF NOT "%CORDIR%"=="" FOR /f %%j IN ("%MSBuildToolsPath%") do SET MSBuildToolsPath=%%~fj
:done_MsBuildToolsPath

echo ---------------------------------------------------------------
echo Executables
echo.
echo ALINK               =%ALINK%
echo CORDIR              =%CORDIR%
echo CORSDK              =%CORSDK%
echo CSC                 =%CSC%
echo csc_flags           =%csc_flags%
echo FSC                 =%FSC%
echo fsc_flags           =%fsc_flags%
echo FSCBinPath          =%FSCBinPath%
echo FSCOREDLL20PATH     =%FSCOREDLL20PATH%
echo FSCOREDLLPATH       =%FSCOREDLLPATH%
echo FSCOREDLLPORTABLEPATH =%FSCOREDLLPORTABLEPATH%
echo FSCOREDLLNETCOREPATH=%FSCOREDLLNETCOREPATH%
echo FSCOREDLLNETCORE78PATH=%FSCOREDLLNETCORE78PATH%
echo FSCOREDLLNETCORE259PATH=%FSCOREDLLNETCORE259PATH%
echo FSDATATPPATH        =%FSDATATPPATH%
echo FSDIFF              =%FSDIFF%
echo FSI                 =%FSI%
echo fsi_flags           =%fsi_flags%
echo GACUTIL             =%GACUTIL%
echo ILDASM              =%ILDASM%
echo MSBUILDTOOLSPATH    =%MSBuildToolsPath%
echo NGEN                =%ngen%
echo PEVERIFY            =%PEVERIFY%
echo RESGEN              =%RESGEN%
echo ---------------------------------------------------------------

exit /b 0

REM ===
REM === Find path to FSC/FSI looking up the registry
REM === Will set the FSCBinPath env variable.
REM === This if for Dev11+/NDP4.5
REM === Works on both XP and Vista and hopefully everything else
REM === Works on 32bit and 64 bit, no matter what cmd prompt it is invoked from
REM === 
:SetFSCBinPath45

reg>NUL 2>&1 query "HKLM\SOFTWARE\Microsoft\FSharp\3.1\Runtime\v4.0" /ve
IF ERRORLEVEL 1 goto :Try64bit
FOR /F "tokens=1-2*" %%a IN ('reg query "HKLM\SOFTWARE\Microsoft\FSharp\4.0\Runtime\v4.0" /ve') DO set FSCBinPath=%%c
IF EXIST "%FSCBinPath%" goto :EOF
FOR /F "tokens=1-3*" %%a IN ('reg query "HKLM\SOFTWARE\Microsoft\FSharp\4.0\Runtime\v4.0" /ve') DO set FSCBinPath=%%d
goto :EOF

:Try64bit
FOR /F "tokens=1-2*" %%a IN ('reg query "HKLM\SOFTWARE\Wow6432Node\Microsoft\FSharp\4.0\Runtime\v4.0" /ve') DO set FSCBinPath=%%c
IF EXIST "%FSCBinPath%" goto :EOF
FOR /F "tokens=1-3*" %%a IN ('reg query "HKLM\SOFTWARE\Wow6432Node\Microsoft\FSharp\4.0\Runtime\v4.0" /ve') DO set FSCBinPath=%%d

goto :EOF

REM ===
REM === Find path to FSharp.Core.dll (in Dev11, this is under Reference Assemblies\Microsoft\FSharp\3.0\Runtime\v4.0 and v2.0)
REM ===                               in Dev12, this is under Reference Assemblies\Microsoft\FSharp\{.NETFramework|.NETCore|.NETPortable}\...
REM ===
:GetFSCOREDLLPaths

REM == Find out OS architecture, no matter what cmd prompt
SET OSARCH=%PROCESSOR_ARCHITECTURE%
IF NOT "%PROCESSOR_ARCHITEW6432%"=="" SET OSARCH=%PROCESSOR_ARCHITEW6432%

REM == Find out path to native 'Program Files 32bit', no matter what
REM == architecture we are running on and no matter what command
REM == prompt we came from.
IF /I "%OSARCH%"=="x86"   set X86_PROGRAMFILES=%ProgramFiles%
IF /I "%OSARCH%"=="IA64"  set X86_PROGRAMFILES=%ProgramFiles(x86)%
IF /I "%OSARCH%"=="AMD64" set X86_PROGRAMFILES=%ProgramFiles(x86)%

REM == Default VS install locations
set FSCOREDLLPATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0
set FSCOREDLL20PATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETFramework\v2.0\2.3.0.0
set FSCOREDLLPORTABLEPATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETPortable\3.47.4.0
set FSCOREDLLNETCOREPATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETCore\3.7.4.0
set FSCOREDLLNETCORE78PATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETCore\3.78.4.0
set FSCOREDLLNETCORE259PATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETCore\3.259.4.0
set FSDATATPPATH=%X86_PROGRAMFILES%\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\Type Providers

REM == Check if using open build instead
IF EXIST "%FSCBinPath%\FSharp.Core.dll" set FSCOREDLLPATH=%FSCBinPath%
IF EXIST "%FSCBinPath%\..\..\net20\bin\FSharp.Core.dll" set FSCOREDLL20PATH=%FSCBinPath%\..\..\net20\bin
IF EXIST "%FSCBinPath%\..\..\portable47\bin\FSharp.Core.dll" set FSCOREDLLPORTABLEPATH=%FSCBinPath%\..\..\portable47\bin
IF EXIST "%FSCBinPath%\..\..\portable7\bin\FSharp.Core.dll" set FSCOREDLLNETCOREPATH=%FSCBinPath%\..\..\portable7\bin
IF EXIST "%FSCBinPath%\..\..\portable78\bin\FSharp.Core.dll" set FSCOREDLLNETCORE78PATH=%FSCBinPath%\..\..\portable78\bin
IF EXIST "%FSCBinPath%\..\..\portable259\bin\FSharp.Core.dll" set FSCOREDLLNETCORE259PATH=%FSCBinPath%\..\..\portable259\bin
IF EXIST "%FSCBinPath%\FSharp.Data.TypeProviders.dll" set FSDATATPPATH=%FSCBinPath%

set FSCOREDLLPATH=%FSCOREDLLPATH%\FSharp.Core.dll
set FSCOREDLL20PATH=%FSCOREDLL20PATH%\FSharp.Core.dll
set FSCOREDLLPORTABLEPATH=%FSCOREDLLPORTABLEPATH%\FSharp.Core.dll
set FSCOREDLLNETCOREPATH=%FSCOREDLLNETCOREPATH%\FSharp.Core.dll
set FSCOREDLLNETCORE78PATH=%FSCOREDLLNETCORE78PATH%\FSharp.Core.dll
set FSCOREDLLNETCORE259PATH=%FSCOREDLLNETCORE259PATH%\FSharp.Core.dll
set FSDATATPPATH=%FSDATATPPATH%\FSharp.Data.TypeProviders.dll
