
# the version under development, update after a release
$version = '4.0.0.1'

# append the AppVeyor build number as the pre-release version
if ($env:appveyor){
    $version = $version + '-b' + [int]::Parse($env:appveyor_build_number).ToString('000')
} else {
    $version = $version + '-b001'
}

$nuget = (gi .\FSharp.Core.Nuget\.nuget\NuGet.exe).FullName

function pack($nuspec){
    $dir = [IO.Path]::GetDirectoryName($nuspec)
    rm "$dir\*.nupkg"
    pushd $dir
    & $nuget pack $nuspec -Version $version -NoDefaultExcludes
    popd
}

pack(gi .\FSharp.Core.Nuget\FSharp.Core.4.0.nuspec)
pack(gi .\FSharp.Compiler.Tools.Nuget\FSharp.Compiler.Tools.4.0.nuspec)
