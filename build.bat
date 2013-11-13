@echo off

::Env
if %PROCESSOR_ARCHITECTURE%==x86 (
         set MSBUILD="%SystemRoot%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
) else ( set MSBUILD="%SystemRoot%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
)

::Clean
del /F /S /Q lib\proto
del /F /S /Q lib\release

::If there is newer F# (3.1) installed in GAC then MSBuild will fail in bootstrap
:: with following error ->
::
::      FSC : error FS1223: FSharp.Core.sigdata not found alongside FSharp.Core
::
:: So I COPY it o_O
if %PROCESSOR_ARCHITECTURE%==x86 (
    if exist "%ProgramFiles%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.dll" (
        if not exist "%ProgramFiles%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.optdata" (
            COPY lib\bootstrap\4.0\FSharp.Core.optdata "%ProgramFiles%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.optdata"
        )
        if not exist "%ProgramFiles%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.sigdata" (
            COPY lib\bootstrap\4.0\FSharp.Core.sigdata "%ProgramFiles%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.sigdata"
        )
    ) 
) else (
    if exist "%ProgramFiles(x86)%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.dll" (
        if not exist "%ProgramFiles(x86)%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.optdata" (
            COPY lib\bootstrap\4.0\FSharp.Core.optdata "%ProgramFiles(x86)%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.optdata"
        )
        if not exist "%ProgramFiles(x86)%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.sigdata" (
            COPY lib\bootstrap\4.0\FSharp.Core.sigdata "%ProgramFiles(x86)%\FSharp-2.0.0.0\v4.0\bin\FSharp.Core.sigdata"
        )
    )
)

::Build
pushd .
cd .\src
set ABS_PATH=%CD%
%MSBUILD% %ABS_PATH%/fsharp-proto-build.proj
%MSBUILD% %ABS_PATH%/fsharp-library-build.proj /p:TargetFramework=net40 /p:Configuration=Release
%MSBUILD% %ABS_PATH%/fsharp-compiler-build.proj /p:TargetFramework=net40 /p:Configuration=Release
popd
