rem migrate the FSharp.Core.Microsoft.Signed to FSharp.Core
rem https://github.com/fsharp/fsharp/issues/352

.nuget\NuGet.exe install FSharp.Core.4.3.0.0.Microsoft.Signed -version 3.0.0.0 -out ..\packages
.nuget\NuGet.exe install FSharp.Core.4.3.0.0.Microsoft.Signed -version 3.0.0.1 -out ..\packages
.nuget\NuGet.exe install FSharp.Core.Microsoft.Signed -version 3.1.1.0 -out ..\packages
.nuget\NuGet.exe install FSharp.Core.Microsoft.Signed -version 3.1.1.1 -out ..\packages

.nuget\NuGet.exe pack FSharp.Core.3.0.0.0.nuspec
.nuget\NuGet.exe pack FSharp.Core.3.0.0.1.nuspec
.nuget\NuGet.exe pack FSharp.Core.3.1.1.0.nuspec
.nuget\NuGet.exe pack FSharp.Core.3.1.1.1.nuspec