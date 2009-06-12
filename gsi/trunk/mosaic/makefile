# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

FC   =  mpif90
FLAGS=  -O3 -xW -assume byterecl -traceback -DWRF -check bounds 
INCLD=  -I${NETCDF}/include -I../gsi_code/
LIBS =  -L${NETCDF}/lib -lnetcdf -L../lib -lbufr_d_64

OBJS = netCDFsub.o netCDFsub_geo.o write_bufr_ref.o process_NSSL_mosaic.o

all: 

process_NSSL_mosaic: ${OBJS}
	${FC} -o process_NSSL_mosaic.exe ${FLAGS} ${OBJS} ${LIBS} 

read_bufr: readbufr.o
	${FC} -o read_bufr.exe ${FLAGS} readbufr.o ${LIBS} 

write_bufr: write_bufr_ref.o write_bufr_test.o
	${FC} -o write_bufr.exe ${FLAGS} write_bufr_ref.o write_bufr_test.o  ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
