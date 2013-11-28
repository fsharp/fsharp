@setlocal
@if "%FSHARP_HOME%"=="" ( set FSHARP_HOME=c:\program files\fsharp-1.1.10.4)
@if "%FSC%"=="" ( set FSC=%FSCBinPath%\fsc.exe)
@if "%FSYACC%"=="" ( set FSYACC=%FSCBinPath%\fsyacc.exe)
@if "%FSLEX%"=="" ( set FSLEX=%FSCBinPath%\fslex.exe)
"%FSLEX%" --light-off -o lex.fs lex.fsl
@if ERRORLEVEL 1 goto Exit
"%FSYACC%" --light-off -o pars.fs pars.fsy
@if ERRORLEVEL 1 goto Exit
"%FSC%" --standalone -g -r:Microsoft.GLEE.dll -r:Microsoft.GLEE.Drawing.dll -r:Microsoft.GLEE.GraphViewerGDI.dll -r:Microsoft.GLEE.IGraphViewer.dll -r:Microsoft.GLEE.Splines.dll -r:AsmL.Tools.Algos.SimplexMethod.dll -r:AsmL.Tools.Algos.SimplexMethodOpt.Tableu.dll -o:checkmri.exe utils.fs ast.fs pars.fs lex.fs states.fs analysis.fs test.fs checkmri.fs
@if ERRORLEVEL 1 goto Exit
:Exit
@endlocal
@exit /b %ERRORLEVEL%
