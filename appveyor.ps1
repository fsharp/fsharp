
# the version under development, update after a release
$version = '4.0.1.19'

function isVersionTag($tag){
    $v = New-Object Version
    [Version]::TryParse(($tag).Replace('-alpha','').Replace('-beta',''), [ref]$v)
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
pushd dotnet-tools
& dotnet restore
& dotnet mergenupkg --source ..\FSharp.Core.Nuget\FSharp.Core.$version.nupkg --other ..\packages\Microsoft.FSharp.Core.netcore.1.0.0-alpha-160831\Microsoft.FSharp.Core.netcore.1.0.0-alpha-160831.nupkg --framework netstandard1.6
popd
& copy FSharp.Core.Nuget\*.nupkg lib\release
& copy FSharp.Compiler.Tools.Nuget\*.nupkg lib\release

