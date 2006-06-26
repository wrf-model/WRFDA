define(diversion_number, divnum)dnl
divert(-1)


dnl Define a substitution for both `@...@' and C preprocessor forms.
dnl
define(UC_REPLACE, [dnl
$1=$2
AC_SUBST($1)dnl
AC_DEFINE($1,$2)
])


dnl Set the value of a variable.  Use the environment if possible; otherwise
dnl set it to a default value.  Call the substitute routine.
dnl
define([UC_DEFAULT], [dnl
$1=${$1-"$2"}
AC_SUBST([$1])
])


dnl Initialize this Unidata autoconf(1)-support module.
dnl
define([UC_INIT], [dnl
AC_INIT($1) dnl
UC_CUSTOMIZE dnl
UC_PORT dnl
])


dnl Set up for customizing the makefile in the port/ subdirectory.
dnl
define([UC_PORT], [dnl
TRANSFORMEES='port/Makefile port/master.mk'
POST_PROCESSEES='port/Makefile'
UC_DEFAULT(CPPFLAGS, -DNDEBUG) dnl
UC_DEFAULT(CFLAGS, -O) dnl
UC_OS dnl
case "${OS}" in
  aix*)  UC_ENSURE(CPPFLAGS, -D_ALL_SOURCE);;
  hpux*) UC_ENSURE(CPPFLAGS, -D_HPUX_SOURCE);;
esac
UC_DEFAULT(LIBOBJS, ) dnl
UC_DEFAULT(PORT_HEADERS, ) dnl
UC_DEFAULT(PORT_MANIFEST, )
UC_DEFAULT(PORT_SUBDIRS, )
UC_PROG_CC dnl
UC_PROG_AR dnl
AC_PROG_RANLIB dnl
])


dnl Terminate this Unidata autoconf(1)-support module.
dnl
define([UC_TERM], [dnl
UC_CHECK_MISSING dnl
UC_POSTPROCESS_MAKEFILES($1) dnl
])


dnl Finish with everything (both GNU and Unidata autoconf(1) support).
dnl
define([UC_FINISH], [dnl
AC_OUTPUT($1 ${TRANSFORMEES-})dnl
UC_TERM($1 ${POST_PROCESSEES-})dnl
])


dnl Add a member to the list of makefiles to be post-processed.
dnl
define([UC_POST_PROCESS], [dnl
UC_ENSURE(POST_PROCESSEES, $1)dnl
])


dnl Add a member to the list of files to be transformed.
dnl
define([UC_CREATE], [dnl
UC_ENSURE(TRANSFORMEES, $1)dnl
])


dnl Check for functioning `const' keyword
dnl
define([UC_CONST], [dnl
AC_COMPILE_CHECK([working const], , [/* Ultrix mips cc rejects this.  */
typedef int charset[2]; const charset x;
], dnl
, dnl
[AC_DEFINE(UD_NO_CONST)])dnl
])


dnl Check for functioning `signed' keyword
dnl
define([UC_SIGNED],
[AC_COMPILE_CHECK([working signed], ,
changequote(,)dnl
signed char x;
changequote([,])dnl
, dnl
, dnl
[AC_DEFINE(UD_NO_SIGNED)])dnl
])


dnl Check for function prototypes
dnl
define([UC_PROTOTYPES],
[AC_COMPILE_CHECK([function prototypes], ,
extern int foo(int bar);
, dnl
, dnl
[AC_DEFINE(UD_NO_PROTOTYPES)])dnl
])


dnl Convert argument to uppercase.
dnl
define([UC_UPPERCASE],[translit($1,abcdefghijklmnopqrstuvwxyz,ABCDEFGHIJKLMNOPQRSTUVWXYZ)])


dnl Return the C macro name version of the argument.
dnl
define([UC_C_MACRONAME], [UC_UPPERCASE([translit($1,/.<>,__)])])


dnl Obtain the pathname of a system-supplied header file.  The value of the
dnl associated shell variable is empty if the header-file could not be found.
dnl
define([UC_SYSTEM_HEADER], [dnl
AC_REQUIRE([UC_PROG_CPP])dnl
echo "#include <$1.h>" > conftestpath.c
dnl
dnl We add additional `/'s to the header file name to preclude compiler 
dnl warnings about the non-portability of `#include "/usr/include/..."'.
dnl
path=`$CPP conftestpath.c 2> /dev/null |
    sed -n 's/^#.* 1 "\(.*$1\.h\)".*/\1/p' | 
    head -1`
if test -z "$path"; then
    path=/dev/null
else
    path=//$path
fi
rm -f conftestpath.c
ifelse($1, limits, , [ifelse($1, float, , [dnl
UC_CREATE(port/$1.h)dnl
UC_REPLACE(UD_SYSTEM_[]UC_C_MACRONAME(ifelse($2,,$1,$2))_H,\"$path\")dnl
])])dnl
])


dnl Define macros for variadic function support
dnl
define([UC_VARIADIC_FUNCTIONS],[dnl
AC_REQUIRE([UC_PROG_CPP])dnl
UC_ENSURE(PORT_MANIFEST, stdarg.h.in)dnl
AC_COMPILE_CHECK([variadic function support], [#include <stdarg.h>]
int foo(int bar, ...) {
    va_list     alist;
    va_start(alist, bar);
    bar = (int)va_arg(alist, int);
    va_end(alist);
    return bar;
}, , [dnl
UC_REPLACE(UD_NO_STDARG,0)dnl
UC_SYSTEM_HEADER(stdarg)], [dnl
UC_REPLACE(UD_NO_STDARG,1)dnl
UC_SYSTEM_HEADER(varargs, stdarg)])dnl
UC_ENSURE(PORT_HEADERS, stdarg.h)dnl
AC_PROVIDE([$0])dnl
])


dnl Define macro for string generation
dnl
define([UC_MAKESTRING], [dnl
AC_COMPILE_CHECK([stringization], dnl
[# define MAKESTRING(x)	#x],
char *cp = MAKESTRING(foo);
, dnl
, dnl
[AC_DEFINE(UD_NO_STRINGIZATION)])dnl
])


dnl Define macro for token pasting.
dnl
define([UC_GLUE], [dnl
ifdef([AC_PROVIDE_$0], , [
AC_COMPILE_CHECK([token pasting], [#define GLUE(a,b) a ## b],
char *GLUE(c,p) = "foo";
, dnl
, dnl
[AC_DEFINE(UD_NO_TOKEN_PASTING)])
AC_PROVIDE([$0])dnl
])])


dnl Define pointer-to-void macro.
dnl
define([UC_VOIDP],
[AC_COMPILE_CHECK([void*], ,
extern void *foo();
, dnl
, dnl
[AC_DEFINE(UD_NO_VOIDSTAR)])])


dnl CFORTRAN support:
dnl
define([UC_CFORTRAN], [dnl
ifdef([AC_PROVIDE_$0], , [
echo "checking for cfortran.h"
UC_ENSURE(PORT_MANIFEST, cfortran_h)dnl
UC_ENSURE(PORT_HEADERS, cfortran.h)dnl
AC_REQUIRE([UC_GLUE])dnl
case "$DEFS" in
  *UD_NO_TOKEN_PASTING*) PORT_CFORTRAN=reiser;;
  *) PORT_CFORTRAN=stdc;;
esac
AC_SUBST(PORT_CFORTRAN)dnl
AC_PROVIDE([$0])dnl
])])


dnl Check for standard, udposix(3) stuff.
dnl
define([UC_UDPOSIX], [dnl
ifdef([AC_PROVIDE_$0], , [
AC_REQUIRE([UC_CONST])dnl
AC_REQUIRE([UC_SIGNED])dnl
AC_REQUIRE([UC_PROTOTYPES])dnl
AC_REQUIRE([UC_VARIADIC_FUNCTIONS])dnl
AC_REQUIRE([UC_MAKESTRING])dnl
AC_REQUIRE([UC_GLUE])dnl
AC_REQUIRE([UC_VOIDP])dnl
UC_ENSURE(PORT_MANIFEST, udposix.h.in)dnl
UC_ENSURE(PORT_HEADERS, udposix.h)dnl
UC_CREATE(port/udposix.h)dnl
AC_PROVIDE([$0])dnl
])])


dnl Check for a function.
dnl
define([UC_FUNC], [dnl
AC_REPLACE_FUNCS($2)dnl
case "$LIBOBJS" in
    *$2.o*) UC_REPLACE([UD_NO_[]UC_UPPERCASE($2)_DECL],1);;
    *) UC_REPLACE([UD_NO_[]UC_UPPERCASE($2)_DECL],0);;
esac
])


dnl Check for a type definition.
dnl
define([UC_TYPEDEF], [dnl
AC_COMPILE_CHECK("typedef $2 in $1", [#include $1
typedef void $2;], , [dnl
UC_REPLACE(UD_NO_[]UC_C_MACRONAME($1[]_$2),1)dnl
], [dnl
UC_REPLACE(UD_NO_[]UC_C_MACRONAME($1[]_$2),0)dnl
])dnl
])


dnl Check for a structure definition.
dnl
define([UC_STRUCT], [dnl
AC_COMPILE_CHECK(structure $2, [#include $1
struct $2 {char *foo;};], , [dnl
UC_REPLACE(UD_NO_[]UC_UPPERCASE($2)[]_STRUCT,1)dnl
], [dnl
UC_REPLACE(UD_NO_[]UC_UPPERCASE($2)[]_STRUCT,0)dnl
])dnl
])


dnl Ensure a macro definition.
dnl
define([UC_MACRO], [dnl
AC_COMPILE_CHECK(macro $2, [#include $1
#ifdef $2
  error
#endif], , 
[UC_REPLACE(UD_NO_[]UC_UPPERCASE($2)_MACRO,1)],
[UC_REPLACE(UD_NO_[]UC_UPPERCASE($2)_MACRO,0)])dnl
])


dnl Check for POSIX threads.
dnl
define([UC_PTHREADS], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
AC_BEFORE([$0], [UC_UDPOSIX_ERRNO])dnl
AC_BEFORE([$0], [UC_UDPOSIX_LIMITS])dnl
AC_BEFORE([$0], [UC_UDPOSIX_SETJMP])dnl
AC_BEFORE([$0], [UC_UDPOSIX_SIGNAL])dnl
AC_BEFORE([$0], [UC_UDPOSIX_UNISTD])dnl
echo checking for POSIX threads
UC_TEST_DIR(CPP_PTHREADS,
    /usr/include /usr/gnu/include /usr/local/include /usr/local/gnu/include,
    pthread.h,
    [POSIX threads [[include]]-directory],
    [-I/usr/local/[[include]]])dnl
case "$CPP_PTHREADS" in
    -I*) ;;
    /usr/include)  CPP_PTHREADS=;;
    *) CPP_PTHREADS=-I$CPP_PTHREADS;;
esac
DEFS="$DEFS $CPP_PTHREADS"
UC_TEST_LIB(LD_PTHREADS,
    /lib /usr/lib /usr/gnu /usr/local/lib /usr/local/gnu/lib,
    pthreads,
    POSIX threads,
    -L/usr/local/lib -lpthreads)dnl
AC_PROVIDE([$0])dnl
])])


dnl Ensure a POSIX <limits.h>.
dnl
define([UC_UDPOSIX_LIMITS], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
AC_REQUIRE([UC_PROG_CC])dnl
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, config.c)dnl
AC_HEADER_CHECK(UC_SYSTEM_HEADER(limits), , dnl
[UC_ENSURE(PORT_HEADERS, limits.h)])dnl
])])


dnl Ensure a POSIX <float.h>.
dnl
define([UC_UDPOSIX_FLOAT], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
AC_REQUIRE([UC_PROG_CC])dnl
AC_REQUIRE([UC_UDPOSIX])dnl
echo "checking for conforming <float.h>"
UC_ENSURE(PORT_MANIFEST, config.c)dnl
AC_TEST_CPP([#include <float.h>
#define DBL_DIG foobar], dnl
[UC_ENSURE(PORT_HEADERS, float.h)])dnl
])])


dnl Ensure a POSIX <stdarg.h>.
dnl
define([UC_UDPOSIX_STDARG], [dnl
AC_REQUIRE([UC_VARIADIC_FUNCTIONS])dnl
])


dnl Ensure a POSIX <stddef.h>.
dnl
define([UC_UDPOSIX_STDDEF], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, stddef.h.in)dnl
UC_ENSURE(PORT_HEADERS, stddef.h)dnl
UC_SYSTEM_HEADER(stddef)dnl
UC_MACRO(<stddef.h>, offsetof, (type\, member), dnl
    ((size_t)\&((type*)0)->member))dnl
UC_TYPEDEF(<stddef.h>, size_t)dnl
AC_PROVIDE([$0])dnl
])])


dnl Ensure a POSIX <stdlib.h>.
dnl
define([UC_UDPOSIX_STDLIB], [dnl
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, stdlib.h.in atexit.c)dnl
UC_ENSURE(PORT_HEADERS, stdlib.h)dnl
UC_SYSTEM_HEADER(stdlib)dnl
dnl UC_TYPEDEF(<stdlib.h>, div_t, struct div { int quot; int rem; })dnl
dnl UC_TYPEDEF(<stdlib.h>, ldiv_t, struct ldiv { long quot; long rem; })dnl
UC_TYPEDEF(<stdlib.h>, size_t)dnl
UC_FUNC(<stdlib.h>, atexit, int atexit, (void (*fcn)(void)))dnl
UC_FUNC(<stdlib.h>, getenv)dnl
AC_HAVE_FUNCS(on_exit)dnl
AC_PROVIDE([$0])dnl
])


dnl Ensure a POSIX <string.h>.
dnl
define([UC_UDPOSIX_STRING], [dnl
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, strerror.c string.h strstr.c string.h.in)dnl
UC_ENSURE(PORT_HEADERS, string.h)dnl
UC_SYSTEM_HEADER(string)dnl
UC_TYPEDEF(<string.h>, size_t)dnl
UC_FUNC(<string.h>, strerror, char *strerror, (int errno))dnl
UC_FUNC(<string.h>, strstr, char *strstr, 
    (const char *cs\, const char *ct))dnl
AC_HAVE_FUNCS(bcopy [[index]] rindex)dnl
AC_PROVIDE([$0])dnl
])


dnl Ensure a POSIX <time.h>.
dnl
define([UC_UDPOSIX_TIME], [dnl
ifdef([AC_PROVIDE_$0], , [
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, difftime.c strftime.c time.h.in)dnl
UC_ENSURE(PORT_HEADERS, time.h)dnl
UC_SYSTEM_HEADER(time)dnl
UC_TYPEDEF(<time.h>, time_t)dnl
UC_TYPEDEF(<time.h>, size_t)dnl
UC_FUNC(<time.h>, difftime)dnl
UC_FUNC(<time.h>, strftime)dnl
AC_PROVIDE([$0])dnl
])])


dnl Ensure a POSIX <signal.h>.
dnl
define([UC_UDPOSIX_SIGNAL], [dnl
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, signal.h.in sigaddset.c \
    sigdelset.c sigemptyset.c sigfillset.c sigismember.c sigpending.c \
    sigprocmask.c sigsuspend.c)dnl
UC_ENSURE(PORT_HEADERS, signal.h)dnl
UC_SYSTEM_HEADER(signal)dnl
UC_TYPEDEF(<signal.h>, sigset_t)dnl
UC_TYPEDEF(<signal.h>, sig_atomic_t)dnl
UC_STRUCT(<signal.h>, sigaction)dnl
UC_FUNC(<signal.h>, sigemptyset, int sigemptyset, (sigset_t *set))dnl
UC_FUNC(<signal.h>, sigfillset, int sigfillset, (sigset_t *set))dnl
UC_FUNC(<signal.h>, sigaddset, int sigaddset, (sigset_t *set\, int signo))dnl
UC_FUNC(<signal.h>, sigdelset, int sigdelset, (sigset_t *set\, int signo))dnl
UC_FUNC(<signal.h>, sigismember, int sigismember, (const sigset_t *set\, int signo))dnl
UC_FUNC(<signal.h>, sigaction, int sigaction, (int sig\, const struct sigaction *act\, struct sigaction *oact))dnl
UC_FUNC(<signal.h>, sigprocmask, int sigprocmask, (int how\, const sigset_t *set\, sigset_t *oset))dnl
UC_FUNC(<signal.h>, sigpending, int sigpending, (sigset_t *set))dnl
UC_FUNC(<signal.h>, sigsuspend, int sigsuspend, (const sigset_t *set))dnl
AC_HAVE_FUNCS(sigvec sigblock sigpause sigsetmask sigstack bsdsigp)dnl
AC_PROVIDE([$0])dnl
])


dnl Ensure a POSIX <unistd.h>.
dnl
define([UC_UDPOSIX_UNISTD], [dnl
ifdef([AC_PROVIDE_$0], , [
AC_REQUIRE([UC_UDPOSIX])dnl
UC_ENSURE(PORT_MANIFEST, unistd.h.in)dnl
UC_ENSURE(PORT_HEADERS, unistd.h)dnl
UC_SYSTEM_HEADER(unistd)dnl
UC_TYPEDEF(<unistd.h>, size_t)dnl
UC_FUNC(<unistd.h>, getlogin)dnl
AC_PROVIDE([$0])dnl
])])


dnl Ensure a <select.h>.
dnl
define([UC_SELECT], [dnl
UC_ENSURE(PORT_MANIFEST, select.h)dnl
UC_SYSTEM_HEADER(sys/select)dnl
])


dnl Check for C compiler.  This macro replaces the AC_PROG_CC macro because 
dnl that macro prefers the GNU C compiler.  Note that `c89' isn't checked for;
dnl this is because that compiler hides things like NBBY.
dnl
define([UC_PROG_CC], [dnl
ifdef([AC_PROVIDE_$0], , [
AC_BEFORE([$0], [UC_PROG_CPP])dnl
AC_PROGRAM_CHECK(CC, cc, cc, )dnl
if test -z "$CC"; then
  UC_NEED_VALUE(CC, [C compiler], /bin/cc)dnl
fi
dnl Find out if we are using GNU C, under whatever name.
cat <<EOF > conftest.c
#ifdef __GNUC__
  yes
#endif
EOF
${CC-cc} -E conftest.c > conftest.out 2>&1
if egrep yes conftest.out >/dev/null 2>&1; then
  GCC=1 # For later tests.
  CC="$CC -O"
fi
rm -f conftest*
AC_PROVIDE([$0])dnl
])])


dnl Check for which(1)
dnl
define([UC_PROG_WHICH], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
UC_ENSURE(PORT_MANIFEST, which)dnl
AC_PROGRAM_CHECK(WHICH, which, which, [UC_ABSPATH(port)]/which)dnl
AC_PROVIDE([$0])dnl
])])


dnl Check for cpp(1).  This macro replaces the AC_PROG_CPP macro because:
dnl	1. That macro, for some reason, sets the value of the shell 
dnl	   variable `CPP' to `${CC-cc} -E' rather than to the cpp(1)
dnl	   program and such a value has caused trouble in shell command
dnl	   lines;
dnl	2. The documentation explicitly states that the AC_PROG_CPP macro 
dnl	   should be called after the AC_PROG_CC macro, so there's no reason 
dnl	   for the above value that I can see; and
dnl	3. We need to discover when ${CPP} doesn't work (e.g. when it's 
dnl	   defined as `acc -E' under older versions of SunOS).
dnl
define([UC_PROG_CPP], [dnl
ifdef([AC_PROVIDE_$0], , [dnl
AC_REQUIRE([UC_PROG_WHICH])dnl
AC_REQUIRE([UC_PROG_CC])dnl
AC_PROG_CPP[]dnl
CPP=`eval echo $CPP`
echo "#include <stdlib.h>" > conftest.c
if test `$CPP conftest.c 2> /dev/null | wc -l` = 0; then
  if test "$CPP" = cpp; then
    echo 1>&2 "$[]0: C preprocessor, \`$CPP', doesn't work"
    UC_NEED_VALUE(CPP, [C preprocessor], /lib/cpp)dnl
  else
    echo 1>&2 "$[]0: C preprocessor, \`$CPP', doesn't work; setting to \`cpp'"
    CPP=cpp
    if test `$(WHICH) ${CPP} 2>&1 | wc -w` != 1; then
      echo 1>&2 "$[]0: C preprocessor, \`$CPP', doesn't exist"
      UC_NEED_VALUE(CPP, [C preprocessor], /lib/cpp)dnl
    fi
  fi
fi
rm -f conftest.c
AC_PROVIDE([$0])dnl
])])


dnl Check for FORTRAN compiler.
dnl
define([UC_PROG_FC], [dnl
ifdef([AC_PROVIDE_$0], , [ dnl
AC_REQUIRE([UC_OS])dnl
case "$OS" in
  hpux*) AC_PROGRAMS_CHECK(FC, fort77 fortc f77);;
  dgux*) AC_PROGRAMS_CHECK(FC, ghf77 f77);;
  *)     AC_PROGRAMS_CHECK(FC, f77 cf77 fc);;
esac
if test -z "$FC"; then
  UC_ENSURE(FC, NONE)
  echo "configure: Was unable to find a Fortran compiler on this machine."
fi
AC_PROVIDE([$0])dnl
])])


dnl Check for FORTRAN library.
dnl
define([UC_LIB_F77], [dnl
ifdef([AC_PROVIDE_$0], , [ dnl
AC_REQUIRE([UC_PROG_FC])dnl
AC_REQUIRE([UC_PROG_WHICH])dnl
echo checking for FORTRAN library
case `$(WHICH) "$FC"` in
  *lang*) LD_F77='-lF77 -lM77';;
  *)	  LD_F77='-lF77';;
esac
AC_SUBST(LD_F77)dnl
AC_PROVIDE([$0])dnl
])
])


dnl Check for library utility, ar(1).
dnl
define([UC_PROG_AR], [dnl
AC_PROGRAM_CHECK(AR, ar, ar, )dnl
if test -z "$AR"; then
  UC_NEED_VALUE(AR, [library utility], /bin/ar)dnl
fi
AC_PROVIDE([$0])dnl
])


dnl Check for troff(1).
dnl
define([UC_PROG_TROFF], [dnl
AC_PROGRAM_CHECK(TROFF, troff, ptroff, troff)dnl
if test -z "$TROFF"; then
  UC_NEED_VALUE(TROFF, [troff(1)-like utility], /bin/troff)dnl
fi
AC_PROVIDE([$0])dnl
])


dnl Check for fortc(1)
dnl
define([UC_PROG_FORTC], [dnl
AC_REQUIRE([UC_OS])dnl
AC_REQUIRE([UC_UDPOSIX_STDDEF])dnl
UC_ENSURE(PORT_SUBDIRS, fortc)dnl
UC_ENSURE(PORT_MANIFEST, fortc.h fortc.fc udalloc.h)dnl
UC_CREATE(port/fortc/Makefile)dnl
UC_POST_PROCESS(port/fortc/Makefile)dnl
dir=`pwd`/port/fortc
FORTC="$dir/fortc"
AC_SUBST(FORTC)dnl
NEED_FORTC=yes
AC_SUBST(NEED_FORTC)dnl
])


dnl Check for neqn(1).
dnl
define([UC_PROG_NEQN], [dnl
AC_PROGRAM_CHECK(NEQN, neqn, neqn, cat)dnl
test "$NEQN" = cat && 
  echo 1>&2 "$[]0: Can't find program \`neqn'; setting to \`cat'"
])


dnl Check for tbl(1).
dnl
define([UC_PROG_TBL], [dnl
AC_PROGRAM_CHECK(TBL, tbl, tbl, cat)dnl
test "$TBL" = cat && 
  echo 1>&2 "$[]0: Can't find program \`tbl'; setting to \`cat'"
])


dnl Check for makeinfo(1).
dnl
define([UC_PROG_MAKEINFO], [dnl
AC_PROGRAM_CHECK(MAKEINFO, makeinfo, makeinfo)dnl
])


dnl Determine the operating system.
dnl
define([UC_OS], [dnl
ifdef([AC_PROVIDE_$0], , [
if test -z "$OS"; then
echo checking for type of operating system
cat << \CAT_EOF > conftest.c
#ifdef __osf__
OS_osf
#endif
#ifdef _AIX
OS_aix
#endif
#ifdef __DGUX__
OS_dgux
#endif
#ifdef hpux
OS_hpux
#endif
#ifdef NeXT
OS_nextos
#endif
#ifdef sgi
OS_irix
#endif
#ifdef sun
OS_sunos
#endif
#ifdef ultrix
OS_ultrix
#endif
#ifdef _UNICOS
OS_unicos
#endif
#ifdef __convex__
OS_convex
#endif
#ifdef masscomp
OS_rtu
#endif
CAT_EOF
OS=`cc -E conftest.c | sed -n '/^OS_/ {
  s///p
  q
}'`
rm conftest.c
if test -z "$OS"; then
  UC_NEED_VALUE(OS, [operating system], sunos)dnl
fi
fi
AC_SUBST(OS)dnl
AC_PROVIDE([$0])dnl
])])


dnl Determine the machine type.
dnl
define([UC_MACHINE], [dnl
ifdef([AC_PROVIDE_$0], , [
if test -z "$MACHINE"; then
echo checking for type of machine
MACHINE=`uname -m | tr A-Z a-z`
if test -z "$MACHINE"; then
  UC_NEED_VALUE(MACHINE, [machine hardware type], sun4c)dnl
fi
fi
AC_SUBST(MACHINE)dnl
AC_PROVIDE([$0])dnl
])])


dnl Check for ncdump(1)
dnl
ncdump=${NCDUMP-}
NCDUMP=
AC_PROGRAMS_CHECK(NCDUMP, $ncdump ncdump UC_ABSPATH($exec_prefix)/ncdump)dnl
if test -z "$NCDUMP"; then
  UC_NEED_VALUE(NCDUMP, [netCDF lister], /usr/local/unidata/bin/ncdump)dnl
fi
])


dnl Check for ncgen(1)
dnl
ncgen=${NCGEN-}
NCGEN=
AC_PROGRAMS_CHECK(NCGEN, $ncgen ncgen UC_ABSPATH($exec_prefix)/ncgen)dnl
if test -z "$NCGEN"; then
  UC_NEED_VALUE(NCGEN, [netCDF generator], /usr/local/unidata/bin/ncgen)dnl
fi
])


dnl Test a script.
dnl
define([UC_TEST_SCRIPT],
[cat << EOF > conftest.sh
[$1]
EOF
chmod +x conftest.sh
if ./conftest.sh 2> /dev/null; then
  ifelse([$2], , :, [$2])
ifelse([$3], , , [else
  $3
])dnl
fi
rm -f conftest.sh
])dnl


dnl Filter a file through cpp(1).
dnl
define([UC_FILTER_CPP], [dnl
AC_REQUIRE([UC_PROG_CPP])dnl
echo processing $1 with the C preprocessor to produce $2
ifdef([AC_CONFIG_NAME],
UC_TEST_SCRIPT([dnl
echo "$DEFS" > conftest.c
echo "dnl line 1 $1" >> conftest.c
cat $1 >> conftest.c
$CPP conftest.c | \
    awk '/^$/ {if (set) next; set=1} {print} !/^$/ {set=0}' > $2
rm -f conftest.c]), dnl
[UC_TEST_SCRIPT(
[$CPP "$DEFS" $1 | \
    awk '/^$/ {if (set) next; set=1} {print} !/^$/ {set=0}' > $2])])])


dnl Convert a pathname to an absolute one at autoconf(1) execution time.
dnl
define([UC_ABSPATH_M4], [dnl
syscmd([case "$1" in 
  /*) echo $1; exit;;
   *) path=`pwd`/$1
      tail=
      while test -n "$path"; do
        (cd $path && echo `pwd`$rest) 2> /dev/null && exit
        base=/`basename "$path"`
        tail=/$base$tail
        path=`echo "$path" | sed "s/\/$base//"`
      done;;
esac > conftest.syscmd 2>&1
])dnl
include(conftest.syscmd)dnl
])


dnl Convert a pathname to an absolute one at ./configure execution time.
dnl
define([UC_ABSPATH], [`dnl
case "$1" in 
  /*[)] echo $1; exit;;
   *[)] path=\`pwd\`/$1
        tail=
        while test -n "$path"; do
          (cd $path && echo \`pwd\`$rest) 2> /dev/null && exit
          base=/\`basename "$path"\`
          tail=/$base$tail
          path=\`echo "$path" | sed "s/\/$base//"\`
        done;;
esac
`])


dnl Set a value for the installation prefix.
dnl
define([UC_PREFIX], 
[AC_BEFORE([$0],[UC_PROG_FORTC])dnl
AC_BEFORE([$0],[UC_LIB_NETCDF])AC_BEFORE([$0],[UC_CPP_NETCDF])dnl
AC_BEFORE([$0],[UC_LIB_NCOPERS])AC_BEFORE([$0],[UC_CPP_NCOPERS])dnl
AC_BEFORE([$0],[UC_LIB_UDPORT])dnl
echo setting the installation prefix
prefix=UC_ABSPATH(${prefix-$1})
test -z "$exec_prefix" && exec_prefix=$prefix/bin
AC_PROVIDE([$0])dnl
])


dnl Check for a directory containing a file.
dnl
define([UC_TEST_DIR], [dnl
  if test -z "$$1"; then
    for dir in $2; do
      if test -r $dir/$3; then
        $1=$dir
        break;
      fi
    done
    if test -z "$$1"; then
      UC_NEED_VALUE($1, $4, $5)dnl
    fi
  fi
AC_SUBST($1)dnl
])


dnl Check for X11 header-file directory.
dnl
define([UC_CPP_X11], [dnl
echo checking for X11 header-files
UC_TEST_DIR(CPP_X11, ${OPENWINHOME-/usr/openwin}/[[include]] \
    /usr/[[include]] /usr/local/[[include]], X11/Xlib.h,
    X11 [[[include]]]-directory, -I/usr/openwin/[[[include]]])dnl
CPP_X11=`case ${CPP_X11} in -I*) echo ${CPP_X11};; *) echo -I${CPP_X11-};; esac`
AC_PROVIDE([$0])dnl
])


dnl Check for McIDAS library.
dnl
define([UC_LIB_MCIDAS], [dnl
echo checking for MCIDAS library
UC_TEST_LIB(LD_MCIDAS, /home/mcidas/lib /home/mcidasd/lib, mcidas, McIDAS, dnl
  -L/home/mcidas/lib -lmcidas)dnl
AC_PROVIDE([$0])dnl
])


dnl Check for X11 library.
dnl
define([UC_LIB_X11], [dnl
echo checking for X11 library
UC_TEST_LIB(LD_X11, ${OPENWINHOME-/usr/openwin}/lib /usr/lib dnl
  /usr/lib/X11 /usr/local/lib /usr/local/lib/X11, X11, X11, dnl
  -L/usr/lib/X11 -lX11)dnl
AC_PROVIDE([$0])dnl
])


dnl Check for X11 implementation (header file and library).
dnl
define([UC_X11], [AC_REQUIRE([UC_CPP_X11])AC_REQUIRE([UC_LIB_X11])])


dnl Check for netCDF header-file directory.
dnl
define([UC_CPP_NETCDF], [dnl
echo checking for netCDF header-file
UC_TEST_DIR(CPP_NETCDF, UC_ABSPATH($prefix/[[[include]]]), netcdf.h,
    [netCDF [[include]]-directory], [-I/usr/local/unidata/[[include]]])dnl
CPP_NETCDF=`case ${CPP_NETCDF} in -I*) echo ${CPP_NETCDF};; *) echo -I${CPP_NETCDF-};; esac`
AC_PROVIDE([$0])dnl
])


dnl Check for netCDF library.
dnl
define([UC_LIB_NETCDF], [dnl
echo checking for netCDF library
UC_TEST_LIB(LD_NETCDF, UC_ABSPATH($prefix/lib), netcdf,
  netCDF, -L/usr/local/unidata/lib -lnetcdf)dnl
AC_PROVIDE([$0])dnl
])


dnl Check for netCDF implementation (header file and library).
dnl
define([UC_NETCDF], [AC_REQUIRE([UC_CPP_NETCDF])AC_REQUIRE([UC_LIB_NETCDF])])


dnl Check for netCDF operators library.
dnl
define([UC_LIB_NCOPERS], [dnl
echo checking for netCDF operators library
UC_TEST_LIB(LD_NCOPERS, UC_ABSPATH($prefix/lib), ncopers,
  netCDF-operators, [-L/usr/local/unidata/lib -lncopers])dnl
AC_PROVIDE([$0])dnl
])


dnl Check for LDM header-file directory.
dnl
define([UC_CPP_LDM], [dnl
echo checking for LDM header-file
UC_TEST_DIR(CPP_LDM, UC_ABSPATH($prefix/[[[include]]]) dnl
    UC_ABSPATH($prefix/../[[[include]]]) dnl
    UC_ABSPATH($prefix/../ldm/[[[include]]]), ldm.h,
    [LDM [[include]]-directory], [-I/usr/local/unidata/[[include]]])dnl
CPP_LDM=`case ${CPP_LDM} in -I*) echo ${CPP_LDM};; *) echo -I${CPP_LDM-};; esac`
AC_PROVIDE([$0])dnl
])


dnl Check for LDM library.
dnl
define([UC_LIB_LDM], [dnl
echo checking for LDM library
UC_TEST_LIB(LD_LDM, UC_ABSPATH($prefix/lib) dnl
  UC_ABSPATH($prefix/../lib) UC_ABSPATH($prefix/../ldm/lib), ldm,
  LDM, -L/usr/local/unidata/lib -lldm)dnl
AC_PROVIDE([$0])dnl
])


dnl Check for LDM implementation (header file and library).
dnl
define([UC_LDM], [AC_REQUIRE([UC_CPP_LDM])AC_REQUIRE([UC_LIB_LDM])])


dnl Check for udres(3) library.
dnl
define([UC_LIB_UDRES], [dnl
echo 'checking for udres library'
UC_TEST_LIB(LD_UDRES, UC_ABSPATH($prefix/lib), udape,
  udres, -L/usr/local/unidata/lib -ludape)dnl
AC_PROVIDE([$0])dnl
])


dnl Set installation programs.  This differs from the standard
dnl autoconf(1) macro by making installed data files group writable.
dnl
define([UC_PROG_INSTALL], [dnl
AC_PROG_INSTALL[]dnl
INSTALL_DATA="`echo "${INSTALL_DATA}" | sed 's/644/664/'`"
AC_PROVIDE([$0])dnl
])


dnl Check for a library.  It would have been nice to see if a compile-link-
dnl execute sequence would have worked (via AC_TEST_PROGRAM) but, with dynamic
dnl libraries under SunOS, the link and execution fail due to unresolved 
dnl references.  Ergo, we just check for the regular `.a' file.
dnl
define([UC_TEST_LIB], [dnl
if test -z "$$1"; then
  for dir in $2; do
    if test -r $dir/lib$3.a; then
      $1="-L$dir -l$3"
      break
    fi
  done
  if test -z "$$1"; then
      UC_NEED_VALUE($1, [$4 library], $5)dnl
  fi
fi
AC_SUBST($1)dnl
])


dnl Check for missing definitions.
dnl
define([UC_CHECK_MISSING], [dnl
if test -s conf.missing; then
  cat << CAT_EOF

$[]0: The following variables need values.  They may be set in
the environment or in the file CUSTOMIZE.  Variables referring to
executable programs needn't be set if the relevant directory is added to
PATH.  In any case, ./configure should probably be rerun.  See file INSTALL
for details.

CAT_EOF
  awk -F: 'BEGIN {printf "%-13s%-27s%s\n", "VARIABLE", "MEANING", "EXAMPLE";
	          printf "%-13s%-27s%s\n", "--------", "-------", "-------"}
	         {printf "%-13s%-27s%s\n", $[]1, $[]2, $[]3}' conf.missing
  rm conf.missing
  exit 1
fi
rm -f conf.missing
])


dnl Post process makefiles.
dnl
define([UC_POSTPROCESS_MAKEFILES], [dnl
AC_REQUIRE([UC_PROG_CC])dnl
define([dn],divnum)divert(-1)undivert[]divert(dn)undefine([dn])dnl
dnl Post process any makefiles.
dnl
dnl Create a script to accomplish the post processing.
dnl
cat << EOF_CONFTEST_SH > conftest.sh
cat << EOF_CONFTEST_C > conftest.c
#include <stdio.h>
main()
{
    return readsub((char*)NULL) ? 0 : 1;
}
readsub(inpath)
    char	*inpath;
{
    char	buf[[2048]], path[[1024]];
    FILE	*fp	= inpath == NULL
				? stdin
				: fopen(inpath, "r");
    if (fp == NULL) {
	(void) perror(inpath);
	return 0;
    }
    buf[[sizeof(buf)-1]]	= 0;
    while (fgets(buf, sizeof(buf), fp) != NULL) {
      if (sscanf(buf, "[include]%*[[] \\t[]]%s", path) == 1) {
          if (!readsub(path))
		return 0;
	} else {
	    (void) fputs(buf, stdout);
	}
    }
    return 1;
}
EOF_CONFTEST_C
if $CC -o conftest conftest.c; then
    conftest=\`pwd\`/conftest
    set $1
    for file do
      echo post processing makefile \\\`\$file\\'
      sd=\`pwd\`/\`echo \$file | sed 's,[[^/]]*\$,,'\`
      base=\`basename \$file\`
      (cd \$sd; \$conftest < \$base > conftest.mk && mv conftest.mk \$base)
    done
fi
rm conftest conftest.c
EOF_CONFTEST_SH
dnl
dnl Append the above script to the output-script file, config.status, so that 
dnl invoking that file will also do the post processing.  Note that the 
dnl output-script file will be invoked by ./configure before the post-
dnl processing code is appended.
dnl
cat conftest.sh >> config.status
dnl
dnl If appropriate, do the postprocessing now because the previous step 
dnl couldn't.
dnl
test -n "$no_create" || sh conftest.sh
rm conftest.sh
])


dnl Get shell-variable override values for local customizations.
dnl
define([UC_CUSTOMIZE], [dnl
AC_BEFORE([$0], [UC_DEFAULT])dnl
if [[ -r CUSTOMIZE ]]; then
  echo reading configuration customizations
  . ./CUSTOMIZE
fi
])


dnl Set the root of the FTP distribution directory.
dnl
define([UC_FTPDIR], [dnl
FTPDIR=${FTPDIR-/home/ftp}/$1
AC_SUBST(FTPDIR)dnl
])


dnl Set package name.
dnl
define([UC_PACKAGE], [dnl
echo checking for package name
PACKAGE=${PACKAGE-`basename \`pwd\``}
AC_SUBST(PACKAGE)dnl
])


dnl Set version identifiers.
dnl
define([UC_VERSION], [dnl
echo checking for package version
if test -z "$VERSION"; then
  if test -r VERSION; then \
    VERSION="`cat VERSION`"
  else
    VERSION=
  fi
fi
AC_SUBST(VERSION)dnl
if test -z "$MAJOR_NO"; then
  if test -n "$VERSION"; then \
    MAJOR_NO=`echo $VERSION |
      sed -n '/^\([[0-9]][[0-9]]*\)\.[[0-9]][[0-9]]*.*/s//\1/p;q'`
  else
    MAJOR_NO=
  fi
fi
AC_SUBST(MAJOR_NO)dnl
if test -z "$MINOR_NO"; then
  if test -n "$VERSION"; then \
    MINOR_NO=`echo $VERSION |
      sed -n '/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/s//\1/p;q'`
  else
    MINOR_NO=
  fi
fi
AC_SUBST(MINOR_NO)dnl
])


dnl Handle a missing value.
dnl
define([UC_NEED_VALUE], [dnl
echo "$1:$2:$3" >> conf.missing
])


dnl Ensure that a variable contains a given string and that it's substituted.
dnl
define([UC_ENSURE], [dnl
ifelse($2, , [dnl
  $1=${$1-}
], [dnl
  for arg in $2; do
    case "$$1" in
      *$arg*[)] ;;
      *[)]      $1="${$1-} $arg";;
    esac
  done
])dnl
AC_SUBST($1)dnl
]dnl
)


divert(diversion_number)dnl
