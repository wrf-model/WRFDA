#!/bin/sh
###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#
#   DATE:      01/11/1999
#
#   PURPOSE:   This script uses the make utility to update the bacio 
#              archive libraries.
#
###############################################################

#
#     Remove make file, if it exists.  May need a new make file
#
if [ -f make.bacio ] 
then
  rm -f make.bacio
fi
#
#     Generate a make file ( make.bacio) from this HERE file.
#
cat > make.bacio << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( bacio.v1.3.o baciof.o )

\$(LIB)(bacio.v1.3.o):       bacio.v1.3.c \$(INC)
	ln -f \$(INC) clib.h
	cc -c \$(CFLAGS) bacio.v1.3.c
	ar -rv \$(AFLAGS) \$(LIB) bacio.v1.3.o
	rm clib.h

\$(LIB)(baciof.o):   baciof.f
	mpif90 -c \$(FFLAGS) baciof.f
	ar -rv \$(AFLAGS) \$(LIB) baciof.o
	rm -f baciof.o

EOF
#
#     Update 4-byte version of libbacio_4.a
#
export LIB="../lib/libbacio_4.a"
export INC="clib4.h"
export FFLAGS=" -O3"
export AFLAGS=
export CFLAGS=" -O3 "
make -f make.bacio
#
#     Update 8-byte version of libbacio_8.a
#
export LIB="../lib/libbacio_8.a"
export INC="clib8.h"
export FFLAGS=" -O3 -i8 -r8"
export AFLAGS=
export CFLAGS="-O3 "
make -f make.bacio
#
#     Update Double Precision (Size of Real 8-byte and default Integer) version 
#
export LIB="../lib/libbacio_d.a"
export INC="clib8.h"
export FFLAGS=" -O3 -r8"
export AFLAGS=
export CFLAGS=" -O3 "
make -f make.bacio

rm -f make.bacio
