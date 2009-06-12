## makefile for lightning_porcess
#! /bin/sh -v

# Define the fortran compiler and options

CC   =  mpicc  -cc=icc
CPPFLAGS        =       -D_UNDERSCORE
FC   =  mpif90
FLAGS=  -O3 -xW -assume byterecl -traceback -DWRF -check bounds 
INCLD=  -I${NETCDF}/include -I../gsi_code
LIBS =  -L${NETCDF}/lib -lnetcdf  -L../lib -lbufr_d_64

OBJS = Check_Alaska.o Check_NLDN.o \
       netCDFsub_geo.o netCDFsub_lightning.o \
       cio.o constants_module.o \
       module_debug.o misc_definitions_module.o \
       module_map_utils.o write_bufr_lght.o \
       process_Lightning.o

all: process_Lightning

process_Lightning: ${OBJS}
	${FC} -o process_Lightning.exe ${FLAGS} ${OBJS} ${LIBS} \

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.mod process_Lightning.exe
