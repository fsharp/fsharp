@echo off
if not exist packages\FAKE\tools\Fake.exe ( 
  nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion 
)
if not exist packages\SourceLink.Fake\tools\Fake.fsx ( 
  nuget.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
)
packages\FAKE\tools\FAKE.exe sourcelink.fsx %*