@if "%_echo%"=="" echo off
setlocal

set COR=%windir%\Microsoft.NET\Framework\v4.0.30319
set WIX=dependencies\wix\3.7
set CANDLE=%WIX%\candle.exe
set LIGHT=%WIX%\light.exe

pushd src
%COR%\msbuild fsharp-proto-build.proj
%COR%\ngen install lib\bootstrap\4.0\fsc.exe
%COR%\msbuild fsharp-library-build.proj /p:Configuration=Release &&^
%COR%\msbuild fsharp-compiler-build.proj /p:Configuration=Release &&^
%COR%\msbuild fsharp-library-build.proj /p:TargetFramework=net20  /p:Configuration=Release &&^
%COR%\msbuild fsharp-library-build.proj /p:TargetFramework=mono21 /p:Configuration=Release &&^
%COR%\msbuild fsharp-library-build.proj /p:TargetFramework=portable-net45+sl5+win8 /p:Configuration=Release &&^
%COR%\msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
popd 
%CANDLE%  -ext WixNetFxExtension src\setup\fsharp-side-by-side.wxs
%LIGHT%  -out fsharp-3.0-side-by-side.msi -ext WixUIExtension -ext WixNetFxExtension -ext WixVsExtension fsharp-side-by-side.wixobj 

endlocal
