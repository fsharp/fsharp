This is the F# compiler, core library and core tools (open source edition). It uses the Apache 2.0 license.

The `master` branch is for the latest version of F# (currently F# 3.0).

To bootstrap the compiler, binaries built from an earlier version of this project are used.


## Requirements

Requires mono 2.9 or higher.  Prefer Mono 3.0.

On OSX, requires automake 2.69. To install from [homebrew](http://mxcl.github.com/homebrew):
```
brew install automake
```


## Building

To build and install on non-MacOS systems:
```
./autogen.sh
make
sudo make install
```

On MacOS (OSX), use a prefix to your version of Mono:
```
./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/Current/
make
sudo make install
```

To build the FSharp.Core.dll for [Mono for Android](http://xamarin.com/monoforandroid), use:
```
make do-2-1
```

You can also build using xbuild:
```
cd src
xbuild fsharp-build.proj
```
However the binaries produced are NOT yet usable because they are not correcy strong-name signed. Further, building using
xbuild does not create a Mono-ready distribution (see src/fsharp/targets.make).

## Development notes

We have a CI build set up with the JetBrains/Teamcity server as part of the F# community projects there:

http://teamcity.codebetter.com/project.html?projectId=project61&tab=projectOverview

@forki controls access. Right now this builds both a Mono 'make' install  and a Windows 'cd src; msbuild fsharp-build.proj' build.  No binaries are saved from  the build, it is just for sanity checking.

Uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

F# compiler sources dropped by Microsoft are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).
