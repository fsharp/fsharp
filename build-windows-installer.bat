@if "%_echo%"=="" echo off
setlocal

set WIX=dependencies\wix\3.7
set CANDLE=%WIX%\candle.exe
set LIGHT=%WIX%\light.exe

pushd src
msbuild fsharp-proto-build.proj
ngen install lib\bootstrap\4.0\fsc.exe
msbuild fsharp-library-build.proj /p:Configuration=Release &&^
msbuild fsharp-compiler-build.proj /p:Configuration=Release &&^
msbuild fsharp-library-build.proj /p:TargetFramework=net20  /p:Configuration=Release &&^
msbuild fsharp-library-build.proj /p:TargetFramework=mono21 /p:Configuration=Release &&^
msbuild fsharp-library-build.proj /p:TargetFramework=portable-net4+sl4+wp71+win8 /p:Configuration=Release &&^
msbuild fsharp-library-build.proj /p:TargetFramework=sl5 /p:Configuration=Release
popd 
%CANDLE%  -ext WixNetFxExtension src\setup\fsharp-side-by-side.wxs
%LIGHT%  -out fsharp-3.0-side-by-side.msi -ext WixUIExtension -ext WixNetFxExtension -ext WixVsExtension fsharp-side-by-side.wixobj 

endlocal
