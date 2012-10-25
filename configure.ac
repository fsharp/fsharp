#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([fsharp], [0.1], [avidigal@novell.com])

# Checks for programs.
AC_PROG_MAKE_SET

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test "x$PKG_CONFIG" = "xno"; then
        AC_MSG_ERROR([You need to install pkg-config])
fi

AC_PATH_PROG(MONO_SGEN, mono-sgen, no)

# PKG_CHECK_MODULES([MONO], [mono >= 2.9])
# 
# PKG_CHECK_MODULES([MONO_2_12],
# 	[mono >= 2.11.5], HAVE_MONO_2_12_OR_HIGHER=yes, HAVE_MONO_2_12_OR_HIGHER=no)
# 
# if test "x$HAVE_MONO_2_12_OR_HIGHER" = "xno" ; then
# 	AC_MSG_WARN([Mono 2.11.5 or higher is recommended, for better MSBuild (xbuild) compatibility])
# fi

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

if test "x$with_gacdir" = "xno"; then
	MONODIR=`pkg-config --variable=libdir mono`/mono
	if ! test -e $MONODIR/2.0/mscorlib.dll; then
		MONODIR=`pkg-config --variable=prefix mono`/lib/mono
	fi
else
	MONODIR=$(cd "$with_gacdir/.." && pwd)
fi

if ! test -e $MONODIR/2.0/mscorlib.dll; then
	AC_ERROR(Couldn't find the mono gac directory or mscorlib.dll in the usual places. Set --with-gacdir=<path>)
fi
AC_SUBST(MONODIR)

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
src/fsharp/fsi/Makefile
src/fsharp/policy.2.0.FSharp.Core/Makefile
src/fsharp/policy.4.0.FSharp.Core/Makefile
src/fsharp/policy.2.3.FSharp.Core/Makefile
src/fsharp/policy.4.3.FSharp.Core/Makefile
])
AC_OUTPUT

if ! test -e $prefix/bin/mono; then
	AC_WARN($prefix/bin/mono not found: it is recommended to use the same install prefix where mono is installed)
fi
