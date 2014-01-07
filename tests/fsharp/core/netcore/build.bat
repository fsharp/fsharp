@if "%_echo%"=="" echo off

msbuild netcore.sln /p:Configuration=Debug


exit /b %ERRORLEVEL%
