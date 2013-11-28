@if "%_echo%"=="" echo off

setlocal
if EXIST build.ok DEL /f /q build.ok
call %~d0%~p0..\..\..\config.bat

IF EXIST ..\..\samples\dont.use.standalone (
  "%FSC%" -g -r:AsmL.Tools.Algos.SA.dll -r:Microsoft.GLEE.GraphHelper.dll -r:Microsoft.GLEE.dll -r:Microsoft.GLEE.Drawing.dll -r:Microsoft.GLEE.GraphViewerGDI.dll -r:Microsoft.GLEE.IGraphViewer.dll -r:Microsoft.GLEE.Splines.dll -r:AsmL.Tools.Algos.SimplexMethod.dll -r:AsmL.Tools.Algos.SimplexMethodOpt.Tableu.dll -o:checkmri.exe prim-lexing.fs prim-parsing.fs lexing.fs parsing.fs HashMultiMap.fs HashSet.fs TaggedHash.fs hashtbl.fs utils.fs ast.fs pars.fs lex.fs states.fs analysis.fs test.fs checkmri.fs --nowarn:62
) ELSE (
  "%FSC%" --standalone -g -r:AsmL.Tools.Algos.SA.dll -r:Microsoft.GLEE.GraphHelper.dll -r:Microsoft.GLEE.dll -r:Microsoft.GLEE.Drawing.dll -r:Microsoft.GLEE.GraphViewerGDI.dll -r:Microsoft.GLEE.IGraphViewer.dll -r:Microsoft.GLEE.Splines.dll -r:AsmL.Tools.Algos.SimplexMethod.dll -r:AsmL.Tools.Algos.SimplexMethodOpt.Tableu.dll -o:checkmri.exe prim-lexing.fs prim-parsing.fs lexing.fs parsing.fs HashMultiMap.fs HashSet.fs TaggedHash.fs hashtbl.fs utils.fs ast.fs pars.fs lex.fs states.fs analysis.fs test.fs checkmri.fs --nowarn:62
)
if ERRORLEVEL 1 goto Error  
  
"%PEVERIFY%" checkmri.exe
if ERRORLEVEL 1 goto Error  


:Ok
echo Built fsharp %~f0 ok.
echo. > build.ok
endlocal
exit /b 0


:Error
call %SCRIPT_ROOT%\ChompErr.bat %ERRORLEVEL% %~f0
endlocal
exit /b %ERRORLEVEL%
