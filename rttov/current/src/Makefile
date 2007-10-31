#
# 
# Script to compile all RTTOV-8.7 library and tests programs
# You can either specifically give options in command line:
# make FC=frt FFLAGS='-Ad -Am -O3 -M .'
# or 
# remove the comment '#' from the definitions you want to use
# on your machine below. 
# If this makefile is executed with options, like example above
# they will be passed along to the other make files 
# You can run make like "make basic" to just compile
# the part of the code for clear air RTTOV or 
# "make all" to compile all the code, options are:
#
#    basic (default): classical RTTOV code
#    cld: for cloud cases with input cloud profile
#    scat: for scattering case
#    all:  to make the code for all cases
#
# P. Brunel 12 March 2004  
#
# Select options below by removing # 
#

# Compiler
#SUN and HP compilers
#FC = f90
#FC77 = f90

#Fujitsu compiler
#FC = f95
#FC77=f77

#IBM compiler
#FC = f90
#FC77 = f77

#Intel compiler
#FC = ifc
#FC77 = ifc

#Met Office HP compiler
#FC = /opt/fortran90/bin/f90
#FC77 = f77

#NEC compiler (additionally uncomment section in Makefile_lib)
#FC = sxf90
#FC77 = sxf90

#Compiler options
#HP compiler HP
#FFLAGS = -O2 +check=all -I. +cpp=yes

#VPP compiler Fujitsu
#FFLAGS= -Am -O3 -M . 

#SUN compiler Fujitsu
#FFLAGS= -Am -O2 -M . 
#FFLAGS= -Am -M . -H aesu -O2
#FFLAGS=-Am -O1 -M . 

#SUN compiler SUN
#FFLAGS= -O3 -M. 

#NAG compiler
#FC= f95
#FC77= f95 -dusty
#FFLAGS= -gline -C=all -nan

#SGI compiler
#FFLAGS= -trapuv -g 

#IBM compiler
#FFLAGS= -g -qmaxmem=8192 -qstrict \
#         -qdpc=e -qsuffix=cpp=F90 -qfree=F90 -qspillsize=860

# Intel compiler
#FFLAGS= -g -cm -w95

# Portland compiler
# FC= pgf90
# FC77= pgf77

#NEC compiler (additionally uncomment section in Makefile_lib)
#FFLAGS= -ew -sx6 -Chopt -Wf,-pvctl loopcnt=200000 -Wf,-pvctl nomsg -Wf,-O nomove,-O nomsg

# Main targets
basic: lib_basic main_basic
cld:   lib_basic lib_cld main_cld
scat:  lib_basic lib_scat main_scat
all:   lib_basic lib_cld lib_scat main_basic main_cld main_scat

# Make main programs
main_basic: 
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=rttov_ascii2bin_coef" 
#	mv rttov_ascii2bin_coef.out ../scripts
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=test_2_coef"
#	mv test_2_coef.out ../scripts
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=test_coef"
#	mv test_coef.out ../scripts
	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=example_fwd"
	mv example_fwd.out ../scripts
	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=tstrad"
	mv tstrad.out ../scripts
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=tstrad_sx6"
#	mv tstrad_sx6.out ../scripts
	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=tstrad_rttov7"
	mv tstrad_rttov7.out ../scripts
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=test_errorhandling"
#	mv test_errorhandling.out ../scripts/test_errorhandling.out
#	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=tstrad_indep"
#	mv tstrad_indep.out ../scripts

main_cld:  
	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=rttovcld_test"
	mv rttovcld_test.out ../scripts/rttovcld_test.out

main_scat:  
	@ ${MAKE} -f Makefile_main "FC=$(FC)" "FFLAGS=$(FFLAGS)" "PROG=rttovscatt_test"
	mv rttovscatt_test.out ../scripts/rttovscatt_test.out



# Make of RTTOV Library
lib_basic:
	@ ${MAKE} -f Makefile_lib  lib_basic "FC=${FC}" "FC77=$(FC77)" "FFLAGS=$(FFLAGS)"

lib_cld:
	@ ${MAKE} -f Makefile_lib  lib_cld "FC=${FC}" "FC77=$(FC77)" "FFLAGS=$(FFLAGS)"

lib_scat:
	@ ${MAKE} -f Makefile_lib  lib_scat "FC=${FC}" "FC77=$(FC77)" "FFLAGS=$(FFLAGS)"


# Clean all
clean:
	@ ${MAKE} -f Makefile_lib  clean
	@ ${MAKE} -f Makefile_main clean
#
