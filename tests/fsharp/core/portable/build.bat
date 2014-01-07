@if "%_echo%"=="" echo off

msbuild portablelibrary1.sln /p:Configuration=Debug


exit /b %ERRORLEVEL%
