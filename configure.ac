#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([fsharp], [4.0], [https://github.com/fsharp/fsharp/issues])

# Checks for programs.
AC_PROG_MAKE_SET

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)

# pkg-config precedence: 1) our prefix 2) the system Mono location 3) the PATH
prefix_pkg_config="$prefix"/bin/pkg-config
osx_pkg_config=/Library/Frameworks/Mono.framework/Versions/Current/bin/pkg-config

if test -e $prefix_pkg_config; then
    PKG_CONFIG=$prefix_pkg_config
elif test -e $osx_pkg_config; then
    PKG_CONFIG=$osx_pkg_config
elif test "x$PKG_CONFIG" = "xno"; then
        AC_MSG_ERROR([You need to install pkg-config])
fi

AC_MSG_NOTICE("pkg-config: $PKG_CONFIG")
AC_MSG_NOTICE("PKG_CONFIG_LIBDIR: $PKG_CONFIG_LIBDIR")

MONO_REQUIRED_VERSION=4.0
MONO_RECOMMENDED_VERSION=4.2
MONO_RECOMMENDED_REASON="as it is newer and already considered a stable version"

if ! $PKG_CONFIG --atleast-version=$MONO_REQUIRED_VERSION mono; then
	AC_MSG_ERROR("You need mono $MONO_REQUIRED_VERSION")
fi

if ! $PKG_CONFIG --atleast-version=$MONO_RECOMMENDED_VERSION mono; then
	AC_MSG_WARN([Mono $MONO_RECOMMENDED_VERSION or higher is recommended, $MONO_RECOMMENDED_REASON])
fi

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_ARG_WITH([bootstrap], 
        [  --with-bootstrap=<path>], 
        [], [with_bootstrap=packages/FSharp.Compiler.Tools.4.0.1.3/tools])
with_bootstrap=$(mkdir -p "$with_bootstrap" && cd "$with_bootstrap" && pwd)
AC_MSG_NOTICE(Using bootstrap : $with_bootstrap)
AC_SUBST(with_bootstrap)

AC_ARG_WITH([gacdir],
                    [  --with-gacdir=/path/to/gac      Specify the gac directory (ex: /usr/lib/mono/gac)],
                [],
                [with_gacdir=no]
        )

MONOPREFIX=$(cd `$PKG_CONFIG --variable=prefix mono` && pwd)
MONOBINDIR="$MONOPREFIX"/bin
MONOLIBDIR="$MONOPREFIX"/lib

AC_PATH_PROG(XBUILD, xbuild, no)
xbuild_from_pkg_config="$MONOBINDIR"/xbuild
if test -e $xbuild_from_pkg_config; then
    XBUILD=$xbuild_from_pkg_config
elif test "x$XBUILD" == "xno"; then
    AC_MSG_ERROR([Could not find xbuild])
fi
AC_MSG_NOTICE(xbuild: $XBUILD)

MONOGACDIR="$MONOLIBDIR"/mono
if ! test "x$with_gacdir" = "xno"; then
	MONOGACDIR=$(cd "$with_gacdir/.." && pwd)
fi

MONOGACDIR20="$MONOGACDIR"/2.0
MONOGACDIR35="$MONOGACDIR"/3.5
MONOGACDIR40="$MONOGACDIR"/4.0
MONOGACDIR45="$MONOGACDIR"/4.5

if ! test -e $MONOGACDIR20/mscorlib.dll -o -e $MONOGACDIR45/mscorlib.dll; then
	AC_ERROR(Couldn't find the mono gac directory or mscorlib.dll in the usual places. Set --with-gacdir=<path>)
fi

if test -e $MONOLIBDIR/mono/xbuild-frameworks/.NETPortable/v4.0/Profile/Profile47/mscorlib.dll; then
   PCLENABLED47=yes
else
   PCLENABLED47=no
fi
AC_MSG_NOTICE(PCL Reference Assemblies for Profile 47 found: $PCLENABLED47)

AC_SUBST(PCLENABLED47)


if test -e $MONOLIBDIR/mono/xbuild-frameworks/.NETPortable/v4.0/Profile/Profile47/mscorlib.dll; then
   PCLENABLED47=yes
else
   PCLENABLED47=no
fi
AC_MSG_NOTICE(PCL Reference Assemblies for Profile 47 found: $PCLENABLED47)

AC_SUBST(PCLENABLED47)


if test -e $MONOLIBDIR/mono/xbuild-frameworks/.NETPortable/v4.5/Profile/Profile7/System.Runtime.dll; then
   PCLENABLED7=yes
else
   PCLENABLED7=no
fi
AC_MSG_NOTICE(PCL Reference Assemblies for Profile 7 found: $PCLENABLED7)

AC_SUBST(PCLENABLED7)

if test -e $MONOLIBDIR/mono/xbuild-frameworks/.NETPortable/v4.5/Profile/Profile78/System.Runtime.dll; then
   PCLENABLED78=yes
else
   PCLENABLED78=no
fi
AC_MSG_NOTICE(PCL Reference Assemblies for Profile 78 found: $PCLENABLED78)

AC_SUBST(PCLENABLED78)

if test -e $MONOLIBDIR/mono/xbuild-frameworks/.NETPortable/v4.5/Profile/Profile259/System.Runtime.dll; then
   PCLENABLED259=yes
else
   PCLENABLED259=no
fi
AC_MSG_NOTICE(PCL Reference Assemblies for Profile 259 found: $PCLENABLED259)

AC_SUBST(PCLENABLED259)

# We enable MonoTouch and MonoDroid builds if PCL components are available.
# These build using binaries from dependencies/mono/2.1, but see
# https://github.com/fsharp/fsharp/issues/391 where PCL is a requirement of
# Microsoft.Common.targets when used in this configuration
MONOTOUCHENABLED=$PCLENABLED78
MONODROIDENABLED=$PCLENABLED78
XAMARINMACENABLED=$PCLENABLED78

AC_SUBST(MONOTOUCHENABLED)
AC_SUBST(MONODROIDENABLED)
AC_SUBST(XAMARINMACENABLED)

AC_SUBST(MONOBINDIR)
AC_SUBST(MONOLIBDIR)
AC_SUBST(MONOGACDIR)

AC_SUBST(MONOGACDIR20)
AC_SUBST(MONOGACDIR35)
AC_SUBST(MONOGACDIR40)

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
src/fsharp/fsi/Makefile
src/fsharp/fsiAnyCpu/Makefile
src/fsharp/policy.2.0.FSharp.Core/Makefile
src/fsharp/policy.2.3.FSharp.Core/Makefile
src/fsharp/policy.3.3.FSharp.Core/Makefile
src/fsharp/policy.3.7.FSharp.Core/Makefile
src/fsharp/policy.3.47.FSharp.Core/Makefile
src/fsharp/policy.3.78.FSharp.Core/Makefile
src/fsharp/policy.3.259.FSharp.Core/Makefile
src/fsharp/policy.4.0.FSharp.Core/Makefile
src/fsharp/policy.4.3.FSharp.Core/Makefile
])
AC_OUTPUT

CONFIGURE_FILE=autogen.sh
if ! test -e $CONFIGURE_FILE; then
	CONFIGURE_FILE=configure
fi
CONFIGURE_COMMAND="`dirname $0`/$CONFIGURE_FILE"

if ! test "x$MONOPREFIX" = "x$prefix"; then
	AC_WARN([Prefix to use is not the same as mono's: $prefix
		Consider using: $CONFIGURE_COMMAND --prefix=$MONOPREFIX])
fi
