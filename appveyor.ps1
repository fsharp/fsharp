
# the version under development, update after a release
$version = '4.0.1.4'

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse($tag, [ref]$v)
}

# append the AppVeyor build number as the pre-release version
if ($env:appveyor){
    $version = $version + '-b' + [int]::Parse($env:appveyor_build_number).ToString('000')
    if ($env:appveyor_repo_tag -eq 'true' -and (isVersionTag($env:appveyor_repo_tag_name))){
        $version = $env:appveyor_repo_tag_name
    }
    Update-AppveyorBuild -Version $version
} else {
    $version = $version + '-b001'
}

$nuget = (gi .\.nuget\NuGet.exe).FullName

function pack($nuspec){
    $dir = [IO.Path]::GetDirectoryName($nuspec)
    rm "$dir\*.nupkg"
    pushd $dir
    & $nuget pack $nuspec -Version $version -NoDefaultExcludes
    popd
}

pack(gi .\FSharp.Core.Nuget\FSharp.Core.nuspec)
pack(gi .\FSharp.Compiler.Tools.Nuget\FSharp.Compiler.Tools.nuspec)

# Merge the latest known .NET Core FSharp.Core nuget package with the one we build here
& packages\NupkgMerge.1.0.0.1\tools\NupkgMerge.exe -p FSharp.Core.Nuget\FSharp.Core.$version.nupkg -s packages\Microsoft.FSharp.Core.netcore.1.0.0-alpha-160629\Microsoft.FSharp.Core.netcore.1.0.0-alpha-160629.nupkg -o lib\release\FSharp.Core.$version.nupkg
& copy FSharp.Compiler.Tools.Nuget\*.nupkg lib\release

