
# the version under development, update after a release
$version = '3.1.2.3'

# append the AppVeyor build number as the pre-release version
if ($env:appveyor){
    $version = $version + '-b' + [int]::Parse($env:appveyor_build_number).ToString('000')
}

$nuget = (gi .\FSharp.Core.Nuget\.nuget\NuGet.exe).FullName

function pack($nuspec){
    pushd([IO.Path]::GetDirectoryName($nuspec))
    & $nuget pack $nuspec -Version $version -NoDefaultExcludes
    popd
}

pack(gi .\FSharp.Core.Nuget\FSharp.Core.3.1.nuspec)
pack(gi .\FSharp.Compiler.Tools.Nuget\FSharp.Compiler.Tools.3.1.nuspec)
