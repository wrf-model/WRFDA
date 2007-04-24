# IO_GRIB

IO_GRIB1_OBJS = io_grib1.o gribmap.o grib1_routines.o trim.o
IO_GRIB1_LIBS    = -lm
IO_GRIB1_M4      = -Uinclude -Uindex -Ulen
IO_GRIB1_CPPFLAGS = -C -P
IO_GRIB1_INCLUDEDIRS  = -I. -I../external/io_grib1/grib1_util -I../external/io_grib1/MEL_grib1

IO_GRIB2_OBJS = grib2tbls_types.o read_grib2map.o io_grib2.o test_read_grib2map.o
IO_GRIB2_LIBS    = -lm
IO_GRIB2_M4      = -Uinclude -Uindex -Ulen
IO_GRIB2_CPPFLAGS = -C -P
IO_GRIB2_INCLUDEDIRS  = -I. -I../external/io_grib_share -I../external/io_grib2/bacio-1.3 \
                          -I../external/io_grib2/g2lib

IO_GRIB_SHARE_OBJS = io_grib_share.o get_region_center.o gridnav.o open_file.o
IO_GRIB_SHARE_LIBS    = -lm
IO_GRIB_SHARE_M4      = -Uinclude -Uindex -Ulen
IO_GRIB_SHARE_CPPFLAGS = -C -P
IO_GRIB_SHARE_INCLUDEDIRS  = -I. -I../external/io_grib_share


wgrib :
	( cd ../external/io_grib1/WGRIB ; $(MAKE) CC="$(CC) $(CCFLAGS)" )
	$(LN) ../external/io_grib1/WGRIB/wgrib .

$(IO_GRIB1)/libio_grib1.a:	$(IO_GRIB1_OBJS)
	$(AR) $(IO_GRIB1)/libio_grib1.a $(IO_GRIB1_OBJS)
	( cd $(IO_GRIB1)/MEL_grib1;  $(MAKE) CC="$(CC) $(CCFLAGS)" archive )
	( cd $(IO_GRIB1)/grib1_util; $(MAKE) CC="$(CC) $(CCFLAGS)" archive )
	$(RANLIB) $(IO_GRIB1)/libio_grib1.a

io_grib1.o:     io_grib1.F 
	$(CPP) $(IO_GRIB1_CPPFLAGS) io_grib1.F $(IO_GRIB_INCLUDEDIRS) | $(M4) $(IO_GRIB1_M4) - > io_grib1.f
	$(FC) $(FCFLAGS_NOWARN) -I. -c io_grib1.f

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

$(IO_GRIB_SHARE)/libio_grib_share.a: $(IO_GRIB_SHARE_OBJS)
	$(AR) $(IO_GRIB_SHARE)/libio_grib_share.a $(IO_GRIB_SHARE_OBJS)
	$(RANLIB) $(IO_GRIB_SHARE)/libio_grib_share.a

# grib2 support incomplete

libio_grib2.a:	g2lib $(IO_GRIB2_OBJS)
	$(AR) $(IO_GRIB2)/libio_grib2.a $(IO_GRIB2_OBJS)
	( cd $(IO_GRIB2)/bacio-1.3;  $(MAKE) CC="$(CC) $(CCFLAGS)" )
	$(RANLIB) $(IO_GRIB2)/libio_grib2.a

g2lib1 :
	( cd $(IO_GRIB2)/g2lib; $(MAKE) CC="$(CC) $(CCFLAGS)" )

test_read_grib2map: test_read_grib2map.F
	$(FC) -c -g test_read_grib2map.F
	$(FC) -g -o test_read_grib2map test_read_grib2map.o \
		-L../external/io_grib2/ -lio_grib2.a -lm

# DO NOT DELETE THIS LINE -- make depend depends on it.
