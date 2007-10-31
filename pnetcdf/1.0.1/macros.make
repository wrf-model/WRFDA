# $Id: macros.make.in,v 1.8 2005/04/20 22:26:13 robl Exp $

# The purpose of this file is to contain common make(1) macros.
# It should be processed by every execution of that utility.


# POSIX shell.  Shouldn't be necessary -- but is under IRIX 5.3.
SHELL		= /bin/sh


# Installation Directories:
SRCDIR		= /big/users/wrfhelp/external/netcdf/pnetcdf-1.0.1/pgi_i686
prefix		= /big/users/wrfhelp/external/netcdf/pnetcdf-1.0.1/pgi_i686
exec_prefix	= $(prefix)
INCDIR		= $(exec_prefix)/include
LIBDIR		= $(exec_prefix)/lib
BINDIR		= $(exec_prefix)/bin
MANDIR		= $(prefix)/man


# Preprocessing:
M4		= m4
M4FLAGS		= -B10000
CPP		= gcc -E
CPPFLAGS	= $(INCLUDES) $(DEFINES) 
FPP		= 
FPPFLAGS	= $(CPPFLAGS)
CXXCPPFLAGS	= $(CPPFLAGS)


# Compilation:
CC		= /big/users/wrfhelp/external/mpi/mpich2-1.0.5p4/pgi_i686/bin/mpicc
CXX		= @CXX@
FC		= /big/users/wrfhelp/external/mpi/mpich2-1.0.5p4/pgi_i686/bin/mpif77
F90		= @F90@
CFLAGS		= -g -O2 -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64
CXXFLAGS	= @CXXFLAGS@
FFLAGS		= 
F90FLAGS	= @F90FLAGS@
NETCDF.MOD	= @NETCDF_MOD@
CC_MAKEDEPEND	= false
COMPILE.c	= $(CC) -c $(CFLAGS) $(CPPFLAGS)
COMPILE.cxx	= $(CXX) -c $(CXXFLAGS) $(CXXCPPFLAGS)
COMPILE.f	= $(FC) -c $(FFLAGS)
COMPILE.F90	= $(F90) -c $(F90FLAGS)
# The following command isn't available on some systems; therefore, the
# `.F.o' rule is relatively complicated.
COMPILE.F	= $(COMPILE.f) $(FPPFLAGS)


# Linking:
MATHLIB		= -lm
FLIBS		= 
F90LIBS		= @F90LIBS@
LIBS		= 
F90LDFLAGS	= $(LDFLAGS)
LINK.c		= $(CC) -o $@ $(CFLAGS) $(LDFLAGS)
LINK.cxx	= $(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS)
LINK.F		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.f		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.F90	= $(F90) -o $@ $(F90FLAGS) $(F90LDFLAGS)


# Manual pages:
WHATIS		= whatis
# The following macro should be empty on systems that don't
# allow users to create their own manual-page indexes.
MAKEWHATIS_CMD	= 


# Misc. Utilities:
AR		= ar
ARFLAGS		= cru
RANLIB		= ranlib
TARFLAGS	= -chf


# Dummy macros: used only as placeholders to silence GNU make.  They are
# redefined, as necessary, in subdirectory makefiles.
HEADER		= dummy_header
HEADER1		= dummy_header1
HEADER2		= dummy_header2
HEADER3		= dummy_header3
LIBRARY		= dummy_library.a
MANUAL		= dummy_manual
PROGRAM		= dummy_program


# Distribution macros:
FTPDIR		= /home/ftp/pub/$(PACKAGE)
FTPBINDIR	= @FTPBINDIR@
VERSION		= dummy_version
