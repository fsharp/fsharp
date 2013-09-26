#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([fsharp], [0.1], [avidigal@novell.com])

# Checks for programs.
AC_PROG_MAKE_SET

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

# On OSX use Mono's private copy of pkg-config if it exists, see https://github.com/fsharp/fsharp/issues/107
osx_pkg_config=/Library/Frameworks/Mono.framework/Versions/Current/bin/pkg-config
if test -e $osx_pkg_config; then
    PKG_CONFIG=$osx_pkg_config
elif test "x$PKG_CONFIG" = "xno"; then
        AC_MSG_ERROR([You need to install pkg-config])
fi

MONO_REQUIRED_VERSION=2.9
MONO_RECOMMENDED_VERSION=3.0

if ! $PKG_CONFIG --atleast-version=$MONO_REQUIRED_VERSION mono; then
	AC_MSG_ERROR("You need mono $MONO_REQUIRED_VERSION")
fi

AC_PATH_PROG(MONO_SGEN, mono-sgen, no)

if ! $PKG_CONFIG --atleast-version=$MONO_RECOMMENDED_VERSION mono; then
	AC_MSG_WARN([Mono $MONO_RECOMMENDED_VERSION or higher is recommended, for better MSBuild (xbuild) compatibility])

	# stability of Mono's SGEN GC is not so good in older versions than Mono v3.0
	MONO_SGEN=no
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_ARG_WITH([bootstrap], 
        [  --with-bootstrap=<path>], 
        [], [with_bootstrap=lib/bootstrap])
with_bootstrap=$(cd "$with_bootstrap" && pwd)
AC_SUBST(with_bootstrap)

AC_ARG_WITH([gacdir],
                    [  --with-gacdir=/path/to/gac      Specify the gac directory (ex: /usr/lib/mono/gac)],
                [],
                [with_gacdir=no]
        )

MONOPREFIX=$(cd `$PKG_CONFIG --variable=prefix mono` && pwd)
MONOLIBDIR="$MONOPREFIX"/lib
MONOGACDIR="$MONOLIBDIR"/mono
if ! test "x$with_gacdir" = "xno"; then
	MONOGACDIR=$(cd "$with_gacdir/.." && pwd)
fi

MONOGACDIR20="$MONOGACDIR"/2.0
MONOGACDIR35="$MONOGACDIR"/3.5
MONOGACDIR40="$MONOGACDIR"/4.0

if ! test -e $MONOGACDIR20/mscorlib.dll; then
	AC_ERROR(Couldn't find the mono gac directory or mscorlib.dll in the usual places. Set --with-gacdir=<path>)
fi

AC_SUBST(MONOLIBDIR)
AC_SUBST(MONOGACDIR)

AC_SUBST(MONOGACDIR20)
AC_SUBST(MONOGACDIR35)
AC_SUBST(MONOGACDIR40)

if test "x$MONO_SGEN" = "xno"; then
	mono_gc_options=
else
	mono_gc_options=--gc=sgen
fi

AC_SUBST(mono_gc_options)


AC_CONFIG_FILES([
launcher
Makefile
config.make
src/fsharp/Makefile
src/fsharp/FSharp.Build-proto/Makefile
src/fsharp/FSharp.Compiler-proto/Makefile
src/fsharp/Fsc-proto/Makefile
src/fsharp/FSharp.Core/Makefile
src/fsharp/FSharp.Build/Makefile
src/fsharp/FSharp.Compiler/Makefile
src/fsharp/Fsc/Makefile
src/fsharp/FSharp.Compiler.Interactive.Settings/Makefile
src/fsharp/FSharp.Compiler.Server.Shared/Makefile
src/fsharp/FSharp.Data.TypeProviders/Makefile
src/fsharp/fsi/Makefile
src/fsharp/fsiAnyCpu/Makefile
src/fsharp/policy.2.0.FSharp.Core/Makefile
src/fsharp/policy.4.0.FSharp.Core/Makefile
src/fsharp/policy.2.3.FSharp.Core/Makefile
src/fsharp/policy.4.3.FSharp.Core/Makefile
])
AC_OUTPUT

if ! test "x$MONOPREFIX" = "x$prefix"; then
	AC_WARN([Prefix to use is not the same as mono's: $prefix
		Consider using: ./autogen.sh --prefix=$MONOPREFIX])
fi
