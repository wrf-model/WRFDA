include ../../configure.wrf

JPEGLIB    =  $(EXTERNAL)/jpeg-6b/lib
ZLIBLIB    =  $(EXTERNAL)/zlib-1.2.3/lib

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
	$(FC) -o $@ decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o \
           -L${HDFEOS_LIB} -L${HDF_LIB} -I${HDFEOS_INC} -I${HDF_INC} -lhdfeos \
           -lGctp -lmfhdf -ldf -L${ZLIBLIB} -lz -L${JPEGLIB} -ljpeg -lm

decode_airs.o: module_read_airs.o geth_newdate.o calc_rh.o

module_read_airs.o:

geth_newdate.o:

calc_rh.o:

clean:
	rm -f decode_airs.o module_read_airs.o geth_newdate.o calc_rh.o read_airs.mod

clobber:
	make clean; rm -f decode_airs.exe
