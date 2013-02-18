@if "%_echo%"=="" echo off
setlocal

set WIX=dependencies\wix\3.7
set CANDLE=%WIX%\candle.exe
set LIGHT=%WIX%\light.exe

%CANDLE%  -ext WixNetFxExtension src\setup\fsharp-side-by-side.wxs
%LIGHT%  -out fsharp-3.0-side-by-side.msi -ext WixUIExtension -ext WixNetFxExtension -ext WixVsExtension fsharp-side-by-side.wixobj 

REM %CANDLE%  -ext WixNetFxExtension src\setup\fsharp-standard.wxs
REM %LIGHT%  -out fsharp-3.0-standard.msi -ext WixUIExtension -ext WixNetFxExtension -ext WixVsExtension fsharp-standard.wixobj 

endlocal
