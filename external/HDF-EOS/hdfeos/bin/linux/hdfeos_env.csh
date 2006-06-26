# 
# Clear all conditional flags
unset sgi_mode
unset hdfeos_f90_comp
unset hdfeos_nag_flag
 
# set the HDFEOS home directory and HDF variables
# HDFEOS installation done on Mon Jun 26 15:48:35 MDT 2006 
# 
 
setenv HDFEOS_HOME /standalone/users/bray/temp/mike/trunk/external/HDF-EOS/hdfeos	# the HDFEOS home directory
setenv HDFLIB /standalone/users/bray/temp/mike/trunk/external/HDF-EOS/HDF4.2r1/NewHDF/lib 		# the HDF lib directory
setenv HDFINC /standalone/users/bray/temp/mike/trunk/external/HDF-EOS/HDF4.2r1/NewHDF/include 		# the HDF include directory
set sgi_mode=32 		# SGI for standard mode
set opt_flag='-O'		# set compiler optimization level
 
#-----------------------------------------------------------------------------
# file:	
# 	hdfeos_env.csh
#
# description:
# 	This file defines environment variables used by HDFEOS
# 	This version is for use under the C shell (csh).
#
# usage:
# 	This file should be called from your .cshrc file with the line:
#
# 	    source <HDFEOS-home-dir>/bin/hdfeos_env.csh
#	
# 	where <HDFEOS-home-dir> is the full path of the HDFEOS home directory.
#
# author:
# 	Mike Sucher / A.R.C.
#
# notes:
# 	1) This file is compatible with the following platforms:
# 	   Sun, SGI, HP-9000, IBM RS-600 and DEC Alpha.
# 	   It automatically figures out which platform you are on,
# 	   and sets environment variables accordingly.
# 	2) This file defines a variable called hdfeos_path which contains
# 	   all the directories likely to be needed on a given machine
# 	   type, including the HDFEOS_ (and HDF) bin directories.  Users 
# 	   may choose to set their path variable with:
# 	   
# 	           set path=(<user-path-additions> $hdfeos_path )
#
# 	   where <user-path-additions> is an optional list of other
# 	   directories added to the search path.
#
# history:
#	17-Jun-1996 MES  Initial version
# 
#-----------------------------------------------------------------------------

if (! $?sgi_mode ) then		# by default, SGI mode is standard 32-bit
    set sgi_mode=32
endif
if (! $?hdfeos_formal ) then	# by default, PGS formal directories disabled
    set hdfeos_formal=0
endif
if (! $?hdfeos_daac ) then 	# by default, DAAC toolkit version disabled
    set hdfeos_daac=0
endif
if (! $?hdfeos_f90_comp ) then 	# by default, no FORTRAN-90 compiler
    set hdfeos_f90_comp=""
endif
if (! $?hdfeos_nag_flag ) then 	# by default, not using NAG FORTRAN-90
    set hdfeos_nag_flag=0
endif
if (! $?use_flavor ) then 	# by default, not using "flavor"
    set use_flavor=0
endif
if (! $?opt_flag ) then 	# by default, optimizing code
    set opt_flag=-O
endif

set user_path = ( $path )	# save user path

# set path to a base subset of directories, allowing startup on unknown host
# note: once the host has been determined the path is appropriately customized

set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/ucb /usr/bin/X11)


# get operating system type, login name
# special cases: SCO and Cray  - uname works differently,

setenv MACHINE `uname -m | awk '{print $1}'`	# needed on Cray & SCO

switch ( "$MACHINE" )

    case "i386":        			# SCO box
	setenv OSTYPE sco386
        breaksw

    case "CRAY":    				# CRAY
	setenv OSTYPE UNICOS
        breaksw

    default:					# everybody else
	setenv OSTYPE `uname`
        breaksw

endsw

set user=`id | cut -d\( -f2 | cut -d\) -f1`
if ($?LOGNAME == 0) setenv LOGNAME $user	# make sure $LOGNAME is defined
setenv USER $LOGNAME				# make sure $USER is defined


# set machine-dependent environment variables:
# 	HOST  the host name of this machine
# 	BRAND used by other achitecture-specific code
# 	path  the execution search path exported to PATH

switch ( "$OSTYPE" )

    case "AIX": 
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/ucb /usr/bin/X11 /usr/ccs/bin /usr/sbin)
	setenv HOST `hostname`
	setenv BRAND ibm
        breaksw

    case "HP-UX": 
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/bin/X11)
	setenv HOST `hostname`
	setenv BRAND hp 
        breaksw

    case "IRIX":  
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/bsd /usr/bin/X11 /usr/sbin)
	setenv HOST `hostname`
        if ("$sgi_mode" == 32) then
            setenv BRAND sgi
        else if ("$sgi_mode" == 64) then
            setenv BRAND sgi64
        else if ("$sgi_mode" == n32) then
            setenv BRAND sgi32
        else if ("$sgi_mode" == 65) then
            setenv BRAND irix65
        else
            setenv BRAND sgi
        endif
        breaksw

    case "IRIX64":  
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/bsd /usr/bin/X11 /usr/sbin)
	setenv HOST `hostname`
	if ("$sgi_mode" == 32) then
	    setenv BRAND sgi
        else if ("$sgi_mode" == 64) then
	    setenv BRAND sgi64
        else if ("$sgi_mode" == n32) then
	    setenv BRAND sgi32
        else if ("$sgi_mode" == 65) then
            setenv BRAND irix65
        else
	    setenv BRAND sgi
        endif
        breaksw

    case "Linux": 
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/bin/X11)
	setenv HOST `hostname -s`
	setenv BRAND linux
        breaksw

    case "Darwin":
        set path=(/bin /sbin /usr/bin /usr/sbin)
        setenv HOST `hostname -s`
        setenv BRAND macintosh
        breaksw

    case "OSF1":  
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/ucb /usr/bin/X11 /usr/ccs/bin /usr/sbin)
	setenv HOST `hostname -s`
	setenv BRAND dec 
        breaksw

    case "sco386": 
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/etc /usr/bin/X11)
	setenv HOST `hostname -s`
	setenv BRAND sco 
        breaksw

    case "SunOS": 
	# distinguish between SunOS 5.x and 4.x versions
	if (`uname -r | awk -F. '{print $1}'` == "5") then 
           if ( `uname -r | awk -F. '{print $2}'` == "8" ) then
            setenv BRAND sun5.8                 # release V5.x SunOS
           else if ( `uname -r | awk -F. '{print $2}'` == "9" ) then
            setenv BRAND sun5.9                 # release V5.x SunOS
           else if ( `uname -r | awk -F. '{print $2}'` == "10" ) then
            setenv BRAND sun5.10                 # release V5.x SunOS
           else
	    setenv BRAND sun5			# release V5.x SunOS
           endif
	    set path=(/usr/local/bin /opt/SUNWspro/bin /bin /usr/bin /etc /usr/etc /usr/ucb /usr/openwin/bin /usr/openwin/demo /usr/ccs/bin /usr/sbin)
	else                                
	    setenv BRAND sun4			# release V4.x SunOS
	    set path=(/usr/local/bin /usr/local/lang /usr/lang /bin /usr/bin /etc /usr/etc /usr/ucb /usr/openwin/bin /usr/openwin/demo)
	endif
	setenv HOST `hostname`
        breaksw

    case "UNICOS": 
	set path=(/usr/local/bin /bin /usr/bin /etc /usr/ucb /usr/bin/X11)
	setenv HOST `hostname`
	setenv BRAND cray 
        breaksw

    default:
	echo "Operating system: $OSTYPE not supported"
	echo "This release of HDFEOS supports: "
	echo "   Sun, SGI HP-9000 IBM-6000 DEC-Alpha and Cray/Unicos "
        breaksw

endsw




# set machine-dependent compilers and compilation switches:
#
#

setenv NSL_FLAG "" 			# this is nil on all but Sun platforms
setenv NSL_LIB "" 			# this is nil on all but Sun platforms

switch ($BRAND)

    case cray:
	setenv CC cc 			# C compiler
	setenv CFLAGS "$opt_flag" 		# default C flags (optimize, ansi)
	setenv C_CFH "-DCRAYFortran"    # C/cfortran.h called from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 cf77			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main 
	setenv HDFSYS UNICOS		# system type as defined by HDF
	setenv MACHINE CRAY		# system type as defined by HDFEOS
	breaksw

    case dec:
	setenv CC cc 			# C compiler
	setenv CFLAGS "$opt_flag -std"		# default C flags (optimize, ansi)
	setenv C_CFH "-DDECFortran"	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH -Dmain=MAIN__"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 f77 			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH "    # calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS DEC_ALPHA		# system type as defined by HDF
	setenv MACHINE DEC		# system type as defined by HDFEOS
	breaksw

    case hp:
	setenv CC c89 			# C compiler
	setenv CFLAGS "$opt_flag -Ae" 		# default C flags (optimize, ansi)
	setenv C_CFH "" 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main 
	setenv F77 fort77		# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS HP9000		# system type as defined by HDF
	setenv MACHINE HP		# system type as defined by HDFEOS
	breaksw

    case ibm:
	setenv CC cc 			# C compiler
	setenv CFLAGS "$opt_flag -qlanglvl=ansi" # default C flags (optimize, ansi)
	setenv C_CFH "" 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main  FORTAN
	setenv F77 xlf 			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH "" 		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS IBM6000		# system type as defined by HDF
	setenv MACHINE IBM		# system type as defined by HDFEOS
	breaksw

    case linux:
	setenv CC gcc 			# C compiler
	setenv CFLAGS "$opt_flag -ansi" # default C flags (optimize, ansi)
	setenv C_CFH "-Df2cFortran"	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 "g77"		# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS LINUX		# system type as defined by HDF
	setenv MACHINE LINUX		# system type as defined by HDFEOS
	breaksw

    case macintosh:
        setenv CC gcc                   # C compiler
        setenv CFLAGS "$opt_flag -ansi" # default C flags (optimize, ansi)
        setenv C_CFH "-Df2cFortran"     # C w/ cfortran.h callable from FORTRAN
        setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
        setenv C_F77_CFH "$C_CFH"       # calling FORTRAN
        setenv C_F77_LIB ""             # FORTRAN lib called by C main
        setenv F77 "g77"                # FORTRAN compiler
        setenv F77FLAGS ""              # common FORTRAN flags
        setenv F77_CFH ""               # FORTRAN callable from C w/ cfortran.h
        setenv F77_C_CFH "$F77_CFH"     # calling C w/ cfortran.h
        setenv CFH_F77 "$F77_C_CFH"     # old version of F77_C_CFH
        setenv F77_C_LIB ""             # C lib called by FORTRAN main
        setenv HDFSYS MACINTOSH         # system type as defined by HDF
        setenv MACHINE MACINTOSH        # system type as defined by HDFEOS
        breaksw

    case sco:
	setenv CC cc 			# C compiler
	setenv CFLAGS "$opt_flag -posix"	# default C flags (optimize, ansi)
	setenv C_CFH "-Df2cFortran"	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 ""			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS SCO		# system type as defined by HDF
	setenv MACHINE SCO		# system type as defined by HDFEOS
	breaksw

    case sgi:
	if ($OSTYPE == "IRIX64") then
		setenv CC "cc -32"	# C compiler (32 bit)
		setenv F77 "f77 -32"	# FORTRAN compiler (32 bit)
	else
                setenv CC cc		# C compiler
                setenv F77 f77		# FORTRAN compiler
	endif
	setenv CFLAGS "$opt_flag -xansi -D_POSIX_SOURCE"	# default C flags (optimize, ansi)
	setenv C_CFH ""	 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB "-lI77 -lU77 -lF77" # FORTRAN lib called by C main
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS IRIS4		# system type as defined by HDF
	setenv MACHINE SGI		# system type as defined by HDFEOS
	breaksw

    case sgi32:
	setenv CC "cc -n32"		# C compiler (new-style 32 bit)
	setenv F77 "f77 -n32"		# FORTRAN compiler (new-style 32 bit)
	setenv CFLAGS "$opt_flag -xansi -D_POSIX_SOURCE"	# default C flags (optimize, ansi)
	setenv C_CFH ""	 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB "-lI77 -lU77 -lF77" # FORTRAN lib called by C main
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS IRIS4		# system type as defined by HDF
	setenv MACHINE SGI		# system type as defined by HDFEOS
	breaksw

    case irix65:
	setenv CC "cc -n32"		# C compiler (new-style 32 bit)
	setenv F77 "f77 -n32"		# FORTRAN compiler (new-style 32 bit)
	setenv CFLAGS "$opt_flag -xansi -D_POSIX_SOURCE"	# default C flags (optimize, ansi)
	setenv C_CFH ""	 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB "-lI77 -lU77 -lF77" # FORTRAN lib called by C main
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS IRIS4		# system type as defined by HDF
	setenv MACHINE SGI		# system type as defined by HDFEOS
	breaksw

    case sgi64:
	set cpu_type=`hinv | fgrep CPU | head -1 | cut -d' ' -f3 | cut -b2`
	if ("$cpu_type" == "4") then
	    setenv CC "cc -64 -mips3"	# C compiler (R4?00 chip)
	    setenv F77 "f77 -64 -mips3"	# FORTRAN compiler (R4?00 chip)
        else
            setenv CC "cc -64"      	# C compiler
            setenv F77 "f77 -64"    	# FORTRAN compiler
        endif
	setenv CFLAGS "$opt_flag -xansi -D_POSIX_SOURCE"	# default C flags (optimize, ansi)
	setenv C_CFH ""	 		# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB "-lI77 -lU77 -lF77" # FORTRAN lib called by C main
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH"	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB ""		# C lib called by FORTRAN main
	setenv HDFSYS IRIS4		# system type as defined by HDF
	setenv MACHINE SGI		# system type as defined by HDFEOS
	breaksw

    case sun4:
	setenv CC acc			# C compiler
	setenv CFLAGS "$opt_flag -Xa" 		# default C flags (optimize, ansi)
	setenv C_CFH "-DsunFortran"	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 f77 			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH" 	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB "-lm" 		# C lib called by FORTRAN main
	setenv HDFSYS SUN		# system type as defined by HDF
	setenv MACHINE SUN4		# system type as defined by HDFEOS
	setenv NSL_FLAG "-lnsl"		# this is nil on all but Sun platforms
	setenv NSL_LIB "/usr/lib/libnsl.a" # this is nil on all but Sun platforms
	breaksw

    case sun5:
	setenv CC cc			# C compiler
	setenv CFLAGS "$opt_flag -Xa" 		# default C flags (optimize, ansi)
	setenv C_CFH "-DsunFortran"	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 f77 			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH" 	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB "-lm" 		# C lib called by FORTRAN main
	setenv HDFSYS SUN		# system type as defined by HDF
	setenv MACHINE SUN5		# system type as defined by HDFEOS
	setenv NSL_FLAG "-lnsl"		# this is nil on all but Sun platforms
	setenv NSL_LIB "/usr/lib/libnsl.a" # this is nil on all but Sun platform
	breaksw

    case sun5.8:
        setenv CC cc                    # C compiler
        setenv CFLAGS "$opt_flag -Xa"           # default C flags (optimize, ansi)
        setenv C_CFH "-DsunFortran"     # C w/ cfortran.h callable from FORTRAN
        setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
        setenv C_F77_CFH "$C_CFH"       # calling FORTRAN
        setenv C_F77_LIB ""             # FORTRAN lib called by C main
        setenv F77 f77                  # FORTRAN compiler
        setenv F77FLAGS ""              # common FORTRAN flags
        setenv F77_CFH ""               # FORTRAN callable from C w/ cfortran.h
        setenv F77_C_CFH "$F77_CFH"     # calling C w/ cfortran.h
        setenv CFH_F77 "$F77_C_CFH"     # old version of F77_C_CFH
        setenv F77_C_LIB "-lm"          # C lib called by FORTRAN main
        setenv HDFSYS SUN               # system type as defined by HDF
        setenv MACHINE SUN8             # system type as defined by HDFEOS
        setenv NSL_FLAG "-lnsl"         # this is nil on all but Sun platforms
        setenv NSL_LIB "/usr/lib/libnsl.a" # this is nil on all but Sun platform
        breaksw
 
    case sun5.9:
        setenv CC cc                    # C compiler
        setenv CFLAGS "$opt_flag -Xa"           # default C flags (optimize, ansi)
        setenv C_CFH "-DsunFortran"     # C w/ cfortran.h callable from FORTRAN
        setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
        setenv C_F77_CFH "$C_CFH"       # calling FORTRAN
        setenv C_F77_LIB ""             # FORTRAN lib called by C main
        setenv F77 f77                  # FORTRAN compiler
        setenv F77FLAGS ""              # common FORTRAN flags
        setenv F77_CFH ""               # FORTRAN callable from C w/ cfortran.h
        setenv F77_C_CFH "$F77_CFH"     # calling C w/ cfortran.h
        setenv CFH_F77 "$F77_C_CFH"     # old version of F77_C_CFH
        setenv F77_C_LIB "-lm"          # C lib called by FORTRAN main
        setenv HDFSYS SUN               # system type as defined by HDF
        setenv MACHINE SUN9             # system type as defined by HDFEOS
        setenv NSL_FLAG "-lnsl"         # this is nil on all but Sun platforms
        setenv NSL_LIB "/usr/lib/libnsl.a" # this is nil on all but Sun platform
        breaksw
 
    case sun5.10:
        setenv CC cc                    # C compiler
        setenv CFLAGS "$opt_flag -Xa"           # default C flags (optimize, ansi)
        setenv C_CFH "-DsunFortran"     # C w/ cfortran.h callable from FORTRAN
        setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
        setenv C_F77_CFH "$C_CFH"       # calling FORTRAN
        setenv C_F77_LIB ""             # FORTRAN lib called by C main
        setenv F77 f77                  # FORTRAN compiler
        setenv F77FLAGS ""              # common FORTRAN flags
        setenv F77_CFH ""               # FORTRAN callable from C w/ cfortran.h
        setenv F77_C_CFH "$F77_CFH"     # calling C w/ cfortran.h
        setenv CFH_F77 "$F77_C_CFH"     # old version of F77_C_CFH
        setenv F77_C_LIB "-lm"          # C lib called by FORTRAN main
        setenv HDFSYS SUN               # system type as defined by HDF
        setenv MACHINE SUN10            # system type as defined by HDFEOS
        setenv NSL_FLAG "-lnsl"         # this is nil on all but Sun platforms
        setenv NSL_LIB "/usr/lib/libnsl.a" # this is nil on all but Sun platform
        breaksw
 
    default:
	setenv CC cc			# C compiler
	setenv CFLAGS "$opt_flag" 	# default C flags (optimize)
	setenv C_CFH ""	        	# C w/ cfortran.h callable from FORTRAN
	setenv CFHFLAGS "$CFLAGS $C_CFH" # CFLAGS + C_CFH
	setenv C_F77_CFH "$C_CFH"	# calling FORTRAN
	setenv C_F77_LIB ""		# FORTRAN lib called by C main
	setenv F77 f77 			# FORTRAN compiler
	setenv F77FLAGS "" 		# common FORTRAN flags
	setenv F77_CFH ""		# FORTRAN callable from C w/ cfortran.h
	setenv F77_C_CFH "$F77_CFH" 	# calling C w/ cfortran.h
	setenv CFH_F77 "$F77_C_CFH"	# old version of F77_C_CFH
	setenv F77_C_LIB "-lm" 		# C lib called by FORTRAN main
	setenv HDFSYS unknown		# system type as defined by HDF
	setenv MACHINE unknown		# system type as defined by HDFEOS
	breaksw
endsw


# 
# set up environment to handle FORTRAN-90 compiler
#

if ("$hdfeos_f90_comp" != "") then		# using FORTRAN-90

    setenv F77 "$hdfeos_f90_comp"

    if ("$hdfeos_nag_flag" == "1") then	# using NAG f90
        setenv C_CFH "$C_CFH -DNAGf90F"
        setenv CFHFLAGS "$CFLAGS $C_CFH"
    endif

endif


# copy the machine-specific path to variable hdfeos_path

set hdfeos_path = ($path)

# set HDFEOS-related environment variables
# these may be referred to in makefiles and on compiler command lines

if ( $?HDFEOS_HOME ) then

# set up base set of HDFEOS_ Toolkit directory variables.

    setenv HDFEOS_INC 	$HDFEOS_HOME/include		# include (header) files
    setenv HDFEOS_BIN 	${HDFEOS_HOME}/bin/$BRAND	# exectuable files
    setenv HDFEOS_LIB 	${HDFEOS_HOME}/lib/$BRAND	# library files
    setenv HDFEOS_OBJ 	${HDFEOS_HOME}/obj/$BRAND	# object files
    setenv HDFEOS_SRC 	${HDFEOS_HOME}/src 		# source files

    if ( $use_flavor == 1 && "$opt_flag" == "-g" ) then

    	setenv HDFEOS_LIB ${HDFEOS_LIB}_debug
    	setenv HDFEOS_OBJ ${HDFEOS_OBJ}_debug
    	setenv HDFEOS_BIN ${HDFEOS_BIN}_debug

        set hdflib=`echo $HDFLIB | sed "s/${BRAND}/${BRAND}_debug/"`
        if ( -d $hdflib ) then
            setenv HDFLIB $hdflib
        endif
        unset hdflib

        set hdfinc=`echo $HDFINC | sed "s/${BRAND}/${BRAND}_debug/"`
        if ( -d $hdfinc ) then
            setenv HDFINC $hdfinc
        endif
        unset hdfinc

    endif

# update path variables

    set path = ($path $HDFEOS_BIN)		# add HDFEOS_BIN to path
    set hdfeos_path = ($hdfeos_path $HDFEOS_BIN) # add HDFEOS_BIN to hdfeos path
    set user_path = ($user_path $HDFEOS_BIN)	# add HDFEOS_BIN to user path


else

    echo "You must first set the environment variable HDFEOS_HOME"

endif


# set HDF-related environment variables
# these may be referred to in makefiles and on compiler command lines
# use the command 'sethdf <hdf-home-directory> to override the default




#
# restore augmented user path
#
set path = ( $user_path )


# done

