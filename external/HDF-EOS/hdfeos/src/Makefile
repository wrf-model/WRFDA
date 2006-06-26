# Copyright (C) 1996 Hughes and Applied Research Corporation
#
# Permission to use, modify, and distribute this software and its
# documentation for any purpose without fee is hereby granted, provided that
# the above copyright notice appear in all copies and that both that copyright
# notice and this permission notice appear in supporting documentation.
#
# Makefile
# 
# Makefile for field build of HDFEOS
#
# The following environment variables must be defined:
#
#   Flags:
#     AR           library archiving program and flags (e.g.: ar -r)
#     CC           C language compiler (e.g.: cc)
#     CFHFLAGS     C compiler flags and cfortran.h switches
#                  (e.g.: -Xa -O -DsunFortan)
#     RANLIB       ranlib program (e.g. ranlib)
#
#   Directories:
#     HDFEOS_INC   HDF-EOS include directory ($HDFEOS_HOME/include)
#     HDFEOS_LIB   HDF-EOS library directory ($HDFEOS_HOME/lib/$BRAND)
#     HDFEOS_OBJ   HDF-EOS object directory ($HDFEOS_HOME/obj/$BRAND)

# force make program to use bourne shell
SHELL=/bin/sh

# define remove utility
RM = rm -f 

AR = ar -r

# define C preprocessor symbols 
DFLAGS = -D$(HDFSYS)

# location of include files need to compile source code
IFLAGS = -I. -I$(HDFEOS_INC) -I$(HDFINC)

# object files needed to build library
LIBOFILES = \
	$(HDFEOS_OBJ)/EHapi.o \
	$(HDFEOS_OBJ)/GDapi.o \
	$(HDFEOS_OBJ)/PTapi.o \
	$(HDFEOS_OBJ)/SWapi.o

# default target, this just builds the hdfeos library
all:
	@if [ "$(BRAND)" = "sun4" ] ; then				\
		$(MAKE) $(MFLAGS) $(HDFEOS_LIB)/libhdfeos.a ranlib;	\
	else								\
		$(MAKE) $(MFLAGS) $(HDFEOS_LIB)/libhdfeos.a;		\
	fi

$(HDFEOS_LIB)/libhdfeos.a: $(LIBOFILES)
	$(AR) $@ $?

ranlib:
	$(RANLIB) $(HDFEOS_LIB)/libhdfeos.a

install: 
	@echo "no binaries to move"
	@echo "don't worry"

clean:
	$(RM) *.o $(HDFEOS_OBJ)/*.o

$(HDFEOS_OBJ)/EHapi.o : EHapi.c 
	$(CC) $(CFHFLAGS) $(DFLAGS) $(IFLAGS) -c $? -o $@

$(HDFEOS_OBJ)/SWapi.o : SWapi.c
	$(CC) $(CFHFLAGS) $(DFLAGS) $(IFLAGS) -c $? -o $@

$(HDFEOS_OBJ)/GDapi.o : GDapi.c
	$(CC) $(CFHFLAGS) $(DFLAGS) $(IFLAGS) -c $? -o $@

$(HDFEOS_OBJ)/PTapi.o : PTapi.c
	$(CC) $(CFHFLAGS) $(DFLAGS) $(IFLAGS) -c $? -o $@

.c.o:
	$(MAKE) $(MFLAGS) $(HDFEOS_OBJ)/$@
