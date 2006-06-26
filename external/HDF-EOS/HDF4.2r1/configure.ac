dnl Process this file with autoconf to produce configure.
dnl
dnl Copyright by the Board of Trustees of the University of Illinois.
dnl All rights reserved.
dnl
AC_REVISION($Id: configure.ac,v 1.8 2005/02/08 18:29:27 acheng Exp $)

dnl ======================================================================
dnl Initialize configure.
dnl ======================================================================
dnl AC_INIT takes the name of the package, the version number, and an
dnl email address to report bugs. AC_CONFIG_SRCDIR takes a unique file
dnl as its argument.
dnl
dnl NOTE: Don't forget to change the version number here when we do a
dnl release!!!
dnl
AC_INIT([HDF], [4.2r1], [hdfhelp@ncsa.uiuc.edu])
AC_CONFIG_SRCDIR([hdf/src/atom.c])
AC_CONFIG_AUX_DIR([bin])
AC_CONFIG_HEADER([hdf/src/h4config.h])

AM_INIT_AUTOMAKE([HDF], [4.2r1])
AM_MAINTAINER_MODE

AC_PREFIX_DEFAULT([`pwd`/NewHDF])

AC_OUTPUT_COMMANDS([
  echo "creating hdf/src/h4config.h"
  sed 's/#define /#define H4_/' < hdf/src/h4config.h |\
    sed 's/#undef /#undef H4_/' > h4config
  if test ! -f hdf/src/h4config.h; then
    /bin/mv -f h4config hdf/src/h4config.h
  elif (diff h4config hdf/src/h4config.h >/dev/null); then
    /bin/rm -f h4config
    echo "hdf/src/h4config.h is unchanged"
  else
    /bin/mv -f h4config hdf/src/h4config.h
  fi
])

dnl ======================================================================
dnl Information on the package
dnl ======================================================================

dnl Dump all shell variables values.
AC_MSG_CHECKING([shell variables initial values])
set >&AS_MESSAGE_LOG_FD
AC_MSG_RESULT([done])

AC_CANONICAL_HOST

dnl Some platforms have broken tr, basename, and/or xargs programs. Check
dnl that it actually does what it's supposed to do. Catch this early
dnl since configure relies upon tr heavily and there's no use continuing
dnl if it's broken.
dnl
AC_MSG_CHECKING([if tr works])
TR_TEST="`echo Test | tr 'a-z,' 'A-Z '`"
if test $TR_TEST != "TEST"; then
  AC_MSG_ERROR([tr program doesn't work])
else
  AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([if basename works])
BASENAME_TEST="`basename /foo/bar/baz/qux/basename_works`"
if test $BASENAME_TEST != "basename_works"; then
  AC_MSG_ERROR([basename program doesn't work])
else
  AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([if xargs works])
XARGS_TEST="`echo /foo/bar/baz/qux/xargs_works | xargs basename`"
if test $XARGS_TEST != "xargs_works"; then
  AC_MSG_ERROR([xargs program doesn't work])
else
  AC_MSG_RESULT([yes])
fi

dnl Source any special files that we need. These files normally aren't
dnl present but can be used by the maintainers to fine tune things like
dnl turning on debug or profiling flags for the compiler. The search order
dnl is:
dnl
dnl         CPU-VENDOR-OS
dnl         VENDOR-OS
dnl         CPU-OS
dnl         CPU-VENDOR
dnl         OS
dnl         VENDOR
dnl         CPU
dnl
dnl If the `OS' ends with a version number then remove it. For instance,
dnl `freebsd3.1' would become `freebsd'
dnl
case "$host_os" in
  aix4.*)       host_os_novers="aix4.x"     ;;
  aix5.*)       host_os_novers="aix5.x"     ;;
  freebsd*)     host_os_novers="freebsd"    ;;
  irix5.*)      host_os_novers="irix5.x"    ;;
  irix6.*)      host_os_novers="irix6.x"    ;;
  osf4.*)       host_os_novers="osf4.x"     ;;
  osf5.*)       host_os_novers="osf5.x"     ;;
  solaris2.*)   host_os_novers="solaris2.x" ;;
  *)            host_os_novers="$host_os"   ;;
esac

host_config="none"
for f in $host_cpu-$host_vendor-$host_os        \
         $host_cpu-$host_vendor-$host_os_novers \
         $host_vendor-$host_os                  \
         $host_vendor-$host_os_novers           \
         $host_cpu-$host_os                     \
         $host_cpu-$host_os_novers              \
         $host_cpu-$host_vendor                 \
         $host_os                               \
         $host_os_novers                        \
         $host_vendor                           \
         $host_cpu ; do
  AC_MSG_CHECKING([for config $f])
  if test -f "$srcdir/config/$f"; then
    host_config=$srcdir/config/$f
    AC_MSG_RESULT([found])
    break
  fi
  AC_MSG_RESULT([no])
done
if test "X$host_config" != "Xnone"; then
  CC_BASENAME="`echo $CC | cut -f1 -d' ' | xargs basename 2>/dev/null`"
  F77_BASENAME="`echo $F77 | cut -f1 -d' ' | xargs basename 2>/dev/null`"
  CXX_BASENAME="`echo $CXX | cut -f1 -d' ' | xargs basename 2>/dev/null`"
  . $host_config
fi

dnl ======================================================================
dnl Checks for programs
dnl ======================================================================

AC_PROG_MAKE_SET

AC_PROG_CC
AC_PROG_CPP
AC_PROG_CXX

AC_ARG_ENABLE([fortran],
              [AC_HELP_STRING([--enable-fortran],
                              [Build Fortran into library [default=yes]])],,
              enableval="yes")

case "$enableval" in
  yes)
    BUILD_FORTRAN="yes"
    AC_PROG_F77

    if test "X$F77" = "X"; then
      BUILD_FORTRAN="no"
    fi
    ;;
  no)
    BUILD_FORTRAN="no"
    ;;
esac

AM_CONDITIONAL([HDF_BUILD_FORTRAN], [test "X$BUILD_FORTRAN" = "Xyes"])

AC_PROG_INSTALL
AM_PROG_LEX
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_YACC

AC_CHECK_PROGS([AR], [ar xar], [:], [$PATH])

AC_CHECK_PROG([DIFF],     [diff],     [diff -w])
AC_CHECK_PROG([MAKEINFO], [makeinfo], [makeinfo])
AC_CHECK_PROG([NEQN],     [neqn],     [neqn])
AC_CHECK_PROG([TBL],      [tbl],      [tbl])

AC_SUBST([AR])
AC_SUBST([DIFF])

dnl ======================================================================
dnl Checks for libraries
dnl ======================================================================

dnl ----------------------------------------------------------------------
dnl Fake --with-xxx option to allow us to create a help message for the
dnl following --with-xxx options which can take either a =DIR or =INC,LIB
dnl specifier.
dnl
AC_ARG_WITH([fnord],
  [
 For the following --with-xxx options, you can specify where the header
 files and libraries are in two different ways:

    --with-xxx=INC,LIB - Specify individually the include directory and
                         library directory separated by a comma
    --with-xxx=DIR     - Specify only the directory which contains the
                         include/ and lib/ subdirectories
  ])

dnl ----------------------------------------------------------------------
dnl Is the GNU zlib present? It has a header file `zlib.h' and a library
dnl `-lz' and their locations might be specified with the `--with-zlib'
dnl command-line switch. The value is an include path and/or a library path.
dnl If the library path is specified then it must be preceded by a comma.
AC_ARG_WITH([zlib],
            [AC_HELP_STRING([--with-zlib=DIR],
                            [Use zlib library [default=yes]])],,
            [withval=yes])

case "$withval" in
  yes)
    HAVE_ZLIB="yes"
    AC_CHECK_HEADERS([zlib.h],, [unset HAVE_ZLIB])
    AC_CHECK_LIB([z], [compress2],, [unset HAVE_ZLIB])

    if test -z "$HAVE_ZLIB"; then
      AC_MSG_ERROR([couldn't find zlib library])
    fi
    ;;
  no)
    AC_MSG_ERROR([zlib library required to build HDF4])
    ;;
  *)
    HAVE_ZLIB="yes"
    case "$withval" in
      *,*)
        zlib_inc="`echo $withval | cut -f1 -d,`"
        zlib_lib="`echo $withval | cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          zlib_inc="$withval/include"
          zlib_lib="$withval/lib"
        fi
        ;;
    esac

    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$zlib_inc" = "X/usr/include"; then
      zlib_inc=""
    fi
    if test "X$zlib_lib" = "X/usr/lib"; then
      zlib_lib=""
    fi

    if test -n "$zlib_inc"; then
      CPPFLAGS="$CPPFLAGS -I$zlib_inc"
    fi

    AC_CHECK_HEADERS([zlib.h],, [unset HAVE_ZLIB])

    if test -n "$zlib_lib"; then
      LDFLAGS="$LDFLAGS -L$zlib_lib"
    fi

    AC_CHECK_LIB([z], [compress2],, [unset HAVE_ZLIB])

    if test -z "$HAVE_ZLIB"; then
      AC_MSG_ERROR([couldn't find zlib library])
    fi
    ;;
esac

dnl ----------------------------------------------------------------------
dnl Is the JPEG library present?
AC_ARG_WITH([jpeg],
            [AC_HELP_STRING([--with-jpeg=DIR],
                            [Use jpeg library [default=yes]])],,
            [withval=yes])

case "$withval" in
  yes)
    HAVE_JPEG="yes"
    AC_CHECK_HEADERS([jpeglib.h],, [unset HAVE_JPEG])
    AC_CHECK_LIB([jpeg], [jpeg_start_decompress],, [unset HAVE_JPEG])

    if test -z "$HAVE_JPEG"; then
      AC_MSG_ERROR([couldn't find jpeg library])
    fi
    ;;
  no)
    AC_MSG_ERROR([jpeg library required to build HDF4])
    ;;
  *)
    HAVE_JPEG="yes"
    case "$withval" in
      *,*)
        jpeg_inc="`echo $withval | cut -f1 -d,`"
        jpeg_lib="`echo $withval | cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          jpeg_inc="$withval/include"
          jpeg_lib="$withval/lib"
        fi
        ;;
    esac

    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$jpeg_inc" = "X/usr/include"; then
      jpeg_inc=""
    fi
    if test "X$jpeg_lib" = "X/usr/lib"; then
      jpeg_lib=""
    fi

    if test -n "$jpeg_inc"; then
      CPPFLAGS="$CPPFLAGS -I$jpeg_inc"
    fi

    AC_CHECK_HEADERS([jpeglib.h],, [unset HAVE_JPEG])

    if test -n "$jpeg_lib"; then
      LDFLAGS="$LDFLAGS -L$jpeg_lib"
    fi

    AC_CHECK_LIB([jpeg], [jpeg_start_decompress],, [unset HAVE_JPEG])

    if test -z "$HAVE_JPEG"; then
      AC_MSG_ERROR([couldn't find jpeg library])
    fi
    ;;
esac

dnl ----------------------------------------------------------------------
dnl Is the SZip library present?
AC_ARG_WITH([szlib],
            [AC_HELP_STRING([--with-szlib=DIR],
                            [Use szlib library [default=no]])],,
            [withval=no])

case "$withval" in
  yes)
    HAVE_SZIP="yes"
    AC_CHECK_HEADERS([szlib.h],, [unset HAVE_SZIP])
    AC_CHECK_LIB([sz], [SZ_BufftoBuffCompress],, [unset HAVE_SZIP])

    if test -z "$HAVE_SZIP"; then
      AC_MSG_ERROR([couldn't find szlib library])
    fi
    ;;
  no)
    AC_MSG_CHECKING([for szlib])
    AC_MSG_RESULT([suppressed])
    ;;
  *)
    HAVE_SZIP="yes"
    case "$withval" in
      *,*)
        szip_inc="`echo $withval | cut -f1 -d,`"
        szip_lib="`echo $withval | cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          szip_inc="$withval/include"
          szip_lib="$withval/lib"
        fi
        ;;
    esac

    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$szip_inc" = "X/usr/include"; then
      szip_inc=""
    fi
    if test "X$szip_lib" = "X/usr/lib"; then
      szip_lib=""
    fi

    if test -n "$szip_inc"; then
      CPPFLAGS="$CPPFLAGS -I$szip_inc"
    fi

    AC_CHECK_HEADERS([szlib.h],, [unset HAVE_SZIP])

    if test -n "$szip_lib"; then
      LDFLAGS="$LDFLAGS -L$szip_lib"
    fi

    AC_CHECK_LIB([sz], [SZ_BufftoBuffCompress],, [unset HAVE_SZIP])

    if test -z "$HAVE_SZIP"; then
      AC_MSG_ERROR([couldn't find szlib library])
    fi
    ;;
esac

dnl ----------------------------------------------------------------------
dnl Is XDR support present? The TRY_LINK info was gotten from the
dnl mfhdf/libsrc/local_nc.c file.
AC_MSG_CHECKING([for xdr library support])
AC_TRY_LINK([
#ifdef VMS
#define STDC_INCLUDES
#endif
#ifdef __ultrix
#define GCC_FIX
#endif 
#include <rpc/types.h>
#ifdef __ultrix
#undef GCC_FIX
#endif
#include <rpc/xdr.h>], [xdr_int],
            [AC_MSG_RESULT([yes]); build_xdr="no"],
            [AC_MSG_RESULT([no]); build_xdr="yes"])
AM_CONDITIONAL([HDF_BUILD_XDR], [test "X$build_xdr" = "Xyes"])

dnl For Solaris and Fuji systems, add the -nsl for XDR support
case "$host" in
  *-solaris*|*-hp-hpux*|S370-fujitsu-sysv*)
    LIBS="$LIBS -lnsl"  ;;
  *) ;;
esac

dnl ======================================================================
dnl Checks for header files
dnl ======================================================================

dnl Always put the /usr/include/rpc directory first in the search list.
CPPFLAGS="-I/usr/include/rpc $CPPFLAGS"

AC_HEADER_STDC

dnl ======================================================================
dnl Checks for types
dnl ======================================================================

dnl ======================================================================
dnl Checks for structures
dnl ======================================================================

dnl ======================================================================
dnl Checks for compiler characteristics
dnl ======================================================================

AC_PROG_CC_C_O
AC_PROG_CXXCPP

if test "X$BUILD_FORTRAN" = "Xyes"; then
  AC_PROG_F77_C_O
fi

dnl This is a bit of a hack. The AC_CHECK_SIZEOF macro is supposed to
dnl #define a value in a header file. However, we don't use a generated
dnl header file. So I check the value left over from autoconf's test  is
dnl >= 8.
AC_CHECK_SIZEOF([int*])

if test $ac_cv_sizeof_intp -ge 8; then
  CPPFLAGS="$CPPFLAGS -DBIG_LONGS"

  dnl Define super secret special flag for AIX machines in 64-bit mode.
  case "${host}" in
    *-ibm-aix*) CPPFLAGS="$CPPFLAGS -DAIX5L64" ;;
    *)          ;;
  esac
fi

AC_C_BIGENDIAN([], [CPPFLAGS="$CPPFLAGS -DSWAP"])

dnl Are we building this in debug or production mode? (Remove the -g flag
dnl in production mode.)
AC_MSG_CHECKING(for build mode)
AC_ARG_ENABLE(production,
              [AC_HELP_STRING([--enable-production],
                              [Determines how to run the compiler.])])

case "X-$enable_production" in
  X-|X-yes)
    AC_MSG_RESULT([production])

    dnl Remove the "-g" flag from compile line if it's in there.
    CFLAGS_temp=""
    if test -n "$CFLAGS"; then
      for d in $CFLAGS ; do
        if test "X$d" != "X-g"; then
          CFLAGS_temp="$CFLAGS_temp $d"
        fi
      done
      CFLAGS=$CFLAGS_temp
    fi

    FFLAGS_temp=""
    if test -n "$FFLAGS"; then
      for d in $FFLAGS ; do
        if test "X$d" != "X-g"; then
          FFLAGS_temp="$FFLAGS_temp $d"
        fi
      done
      FFLAGS=$FFLAGS_temp
    fi

    CXXFLAGS_temp=""
    if test -n "$CXXFLAGS"; then
      for d in $CXXFLAGS ; do
        if test "X$d" != "X-g"; then
          CXXFLAGS_temp="$CXXFLAGS_temp $d"
        fi
      done
      CXXFLAGS=$CXXFLAGS_temp
    fi

    CFLAGS="$CFLAGS $PROD_CFLAGS"
    CXXFLAGS="$CXXFLAGS $PROD_CXXFLAGS"
    FFLAGS="$FFLAGS $PROD_FFLAGS"
    CPPFLAGS="$CPPFLAGS $PROD_CPPFLAGS"
    ;;
  X-no)
    AC_MSG_RESULT([development])
    CFLAGS="$CFLAGS $DEBUG_CFLAGS"
    FFLAGS="$FFLAGS $DEBUG_FFLAGS"
    CXXFLAGS="$CXXFLAGS $DEBUG_CXXFLAGS"
    CPPFLAGS="$CPPFLAGS $DEBUG_CPPFLAGS"
    ;;
  *)
    AC_MSG_RESULT([user-defined])
    ;;
esac

dnl ======================================================================
dnl Checks for library functions
dnl ======================================================================

AC_MSG_CHECKING([for math library support])
AC_TRY_LINK([#include <math.h>], [sqrt(37.927)],
            [AC_MSG_RESULT([yes])],
            [AC_MSG_RESULT([no]); LIBS="$LIBS -lm"])

dnl ======================================================================
dnl Checks for system services
dnl ======================================================================

dnl Copy NetCDF header files.
dnl
dnl FIXME: This is code stolen^Wborrowed from the old HDF4 configure.
dnl These header files should probably be generated by autoconf instead
dnl of being copied depending upon the host.
case "$host" in
  alpha-*-*)            BAR="alpha"       ;;
  alphaev*-*-*)         BAR="alpha"       ;;
  mips*-dec-ultrix*)    BAR="dec"         ;;
  mips*-sgi-irix6.*)    BAR="irix6"       ;;
  mips*-sgi-irix6_32*)  BAR="irix32"      ;;
  mips*-sgi-irix5*)     BAR="irix5"       ;;
  mips*-sgi-irix4*)     BAR="irix4"       ;;
  *-linux*)             BAR="linux"       ;;
  *-freebsd*)           BAR="fbsd"        ;;
  ia64-*-*)             BAR="ia64"        ;;
  *-ibm-aix*)           BAR="aix"         ;;
  i386-*-solaris2*)     BAR="solarisx86"  ;;
  sparc64-*-solaris2*)  BAR="solaris64"   ;;
  *-*-solaris2*)        BAR="solaris"     ;;
  *-sun-*)              BAR="sun"         ;;
  *-hp-hpux*)           BAR="hpux"        ;;
  *-convex-bsd*)        BAR="convex"      ;;
  *-cray-unicos*)       BAR="unicos"      ;;
  t3*-cray-craympp)     BAR="t3e"         ;;
  S370-fujitsu-sysv*)   BAR="fujivp"      ;;
  *-mac-*)              BAR="mac"         ;;
  *-apple*)             BAR="apple"       ;;
  *)                    echo "*** unknown host $host!"; exit 1 ;;
esac

src_files="mfhdf/libsrc/config/netcdf-FOO.h:mfhdf/libsrc/netcdf.h
           mfhdf/fortran/config/jackets-FOO.c:mfhdf/fortran/jackets.c
           mfhdf/fortran/config/netcdf-FOO.inc:mfhdf/fortran/netcdf.inc
           mfhdf/fortran/config/ftest-FOO.f:mfhdf/fortran/ftest.f"
src_files="`echo $src_files | sed -e s/FOO/${BAR}/g`"

for config_file in $src_files; do
  src_file="${srcdir}/`echo $config_file | sed -e s/:.*$//`"
  target_file="`echo $config_file | sed -e s/^[[^:]]*://g`"

  if test ! -r ${src_file}; then
    echo '***' "${progname}: cannot copy file \"${target_file}\"," 1>&2                  
    echo '***' "since the file \"${src_file}\" does not exist." 1>&2
    exit 1
  fi

  ## this sed command emulates the dirname command
  dstdir=`echo "$target_file" | sed -e 's,[[^/]]*$,,;s,/$,,;s,^$,.,'`

  ## Create directory for the target_file if it's not there.
  if test ! -d $dstdir; then
    ${srcdir}/bin/mkinstalldirs $dstdir
  fi

  echo "Copying \"${src_file}\" to \"${target_file}\" ."
  cp ${src_file} ${target_file}

  if test ! -r ${target_file}; then
    echo '***' "${progname}: cannot copy file \"${target_file}\"," 1>&2                  
    exit 1
  fi
done

AC_CONFIG_FILES([Makefile
                 hdf/Makefile
                 hdf/src/Makefile
                 hdf/test/Makefile
                 hdf/util/Makefile
                 hdf/util/h4cc
                 hdf/util/h4fc
                 hdf/util/h4redeploy
                 hdf/util/testutil.sh
                 man/Makefile
                 mfhdf/Makefile
                 mfhdf/dumper/Makefile
                 mfhdf/dumper/testhdp.sh
                 mfhdf/fortran/Makefile
                 mfhdf/hdfimport/Makefile
                 mfhdf/hdfimport/testutil.sh
                 mfhdf/hdiff/Makefile
                 mfhdf/hrepack/Makefile
                 mfhdf/libsrc/Makefile
                 mfhdf/ncdump/Makefile
                 mfhdf/ncgen/Makefile
                 mfhdf/nctest/Makefile
			 mfhdf/port/Makefile])

dnl If we're building the XDR library, then generate its Makefile.
if test "X$build_xdr" = "Xyes"; then
  AC_CONFIG_FILES([mfhdf/xdr/Makefile])
fi

AC_OUTPUT

chmod 755 hdf/util/h4cc hdf/util/h4redeploy

if test "X$BUILD_FORTRAN" = "Xyes"; then
  chmod 755 hdf/util/h4fc
fi
