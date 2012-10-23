This is the F# compiler, core library and core tools (open source edition). It uses the Apache 2.0 license.

The `master` branch is for the latest version of F# (currently F# 3.0).

To bootstrap the compiler, binaries built from an earlier version of this project are used.


## Requirements

Requires mono 2.9 or higher.

On OSX, requires automake 2.69. To install from [homebrew](http://mxcl.github.com/homebrew):
```
brew install automake
```


## Building

To build, run:
```
./autogen.sh
make
make install
```

On OSX, to replace the installation of F# that comes with Mono you may need to use a prefix:
```
./autogen.sh --prefix=/Library/Frameworks/Mono.framework/Versions/2.10.9/
```

To build the FSharp.Core.dll for [Mono for Android](http://xamarin.com/monoforandroid), use:
```
make do-2-1
```


## Notes

Uses bootstrapping libraries, tools and F# compiler. The `lib/bootstrap/X.0` directories contain mono-built libraries, compiler and tools that can be used to bootstrap a build. You can also supply your own via the `--with-bootstrap` option.

F# original powerpack sources are available from [fsharppowerpack.codeplex.com](http://fsharppowerpack.codeplex.com).
