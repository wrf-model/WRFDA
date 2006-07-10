LIBROOT    =  ./libraries
HDFEOS_LIB =  $(LIBROOT)/hdfeos/lib/macintosh
HDFEOS_INC =  $(LIBROOT)/hdfeos/include
HDFINC     =  $(LIBROOT)/HDF4.2r1/include
HDFLIB     =  $(LIBROOT)/HDF4.2r1/lib
HDF5LIB    =  $(LIBROOT)/hdf5-1.6.5/lib
JPEGLIB    =  $(LIBROOT)/jpeg-6b/lib
ZLIBLIB    =  $(LIBROOT)/zlib-1.2.3/lib

CC = /usr/bin/gcc-3.3
FC = g95
FFLAGS = -ffree-form
CFLAGS =

.IGNORE:
.SUFFIXES: .c .f90 .o

.c.o:
	rm -f $@
	$(CC) $(CFLAGS) -c $*.c

.f90.o:
	rm -f $@ $*.mod
	cp $< $*.f
	$(FC) $(FFLAGS) -c $*.f
	rm -f $*.f

all: decode_airs.exe

decode_airs.exe: decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o
	$(FC) -o $@ decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o -L${HDFEOS_LIB} -L${HDFLIB} -I${HDFEOS_INC} -I${HDFINC} -lhdfeos -lGctp -lmfhdf -ldf -L${ZLIBLIB} -lz -L${JPEGLIB} -ljpeg -lm

decode_airs.o: module_read_airs.o geth_newdate.o calc_rh.o

module_read_airs.o:

geth_newdate.o:

calc_rh.o:

clean:
	rm -f decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o read_airs.mod

clobber:
	make clean; rm -f decode_airs.exe
