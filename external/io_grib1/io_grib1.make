# IO_GRIB1

IO_GRIB1_OBJS = io_grib1.o gribmap.o gridnav.o open_file.o grib1_routines.o trim.o
IO_GRIB1_LIBS    = -lm
IO_GRIB1_M4      = -Uinclude -Uindex -Ulen
IO_GRIB1_CPPFLAGS = -C -P
IO_GRIB1_INCLUDEDIRS  = -I. -I../external/io_grib1/grib1_util -I../external/io_grib1/MEL_grib1

wgrib :
	( cd ../external/io_grib1/WGRIB ; $(MAKE) CC="$(CC) $(CCFLAGS)" )
	$(LN) ../external/io_grib1/WGRIB/wgrib .

libio_grib1.a:	$(IO_GRIB1_OBJS)
	$(AR) cruv ../external/io_grib1/libio_grib1.a $(IO_GRIB1_OBJS)
	( cd ../external/io_grib1/MEL_grib1;  $(MAKE) CC="$(CC) $(CCFLAGS)" archive )
	( cd ../external/io_grib1/grib1_util; $(MAKE) CC="$(CC) $(CCFLAGS)" archive )
	$(RANLIB) ../external/io_grib1/libio_grib1.a

io_grib1.o:     io_grib1.F 
	$(CPP) $(IO_GRIB1_CPPFLAGS) io_grib1.F $(IO_GRIB_INCLUDEDIRS) | $(M4) $(IO_GRIB1_M4) - > io_grib1.f
	$(FC) $(FCFLAGS) -I. -c io_grib1.f

grib1_routines.o : grib1_routines.c
	$(CC) -c $(CCFLAGS) $(IO_GRIB1_INCLUDEDIRS) grib1_routines.c

test_write_grib: test_write_grib.c grib1_routines.c gridnav.c gribmap.c open_file.c trim.c
	$(CC) $(CCFLAGS) -g test_write_grib.c grib1_routines.c gridnav.c gribmap.c open_file.c trim.c \
		-L../external/io_grib1/ -lio_grib1.a -lm

test_grib1_routines: test_grib1_routines.F90 gridnav.c gribmap.c open_file.c trim.c
	$(FC) -c -g test_grib1_routines.F90
	$(CC) $(CCFLAGS) -c -g grib1_routines.c gridnav.c gribmap.c open_file.c trim.c
	$(FC) -g -o test_grib1_routines test_grib1_routines.o grib1_routines.o gridnav.o gribmap.o open_file.o trim.o \
		-L../external/io_grib1/ -lio_grib1.a -lm

# DO NOT DELETE THIS LINE -- make depend depends on it.
