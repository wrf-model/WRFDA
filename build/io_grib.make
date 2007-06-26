# IO_GRIB

GRIB_SHARE_OBJS = io_grib_share.o get_region_center.o gridnav.o open_file.o

wgrib :
	( cd $(IO_GRIB1)/WGRIB ; $(MAKE) CC="$(CC) $(CCFLAGS)" )
	$(LN) $(IO_GRIB1)/WGRIB/wgrib .

$(GRIB1_LIBS):
	( cd $(IO_GRIB1) ; \
          make CC="$(CC)" CFLAGS="$(CCFLAGS)" FC="$(FFC) $(FCTYPE) $(FCDEBUG) \
            -w $(EXTRA_FCFLAGS)" RM="$(RM)" CPP="$(CPP) $(CPPFLAGS)" FIXED="$(FCFIXED)" archive)
	$(RANLIB) $(IO_GRIB1)/libio_grib1.a

$(GRIB2_LIBS) :
	( cd $(IO_GRIB2) ; \
          make CC="$(CC) -I$(GRIB2_INC)" CFLAGS="$(CCFLAGS)" FC="$(FFC) $(FCTYPE) $(FCDEBUG) \
            -w $(EXTRA_FCFLAGS)" RM="$(RM)" CPP="$(CPP) $(CPPFLAGS)" FIXED="$(FCFIXED)" archive)
	$(RANLIB) $(IO_GRIB2)/libio_grib2.a

$(GRIB_SHARE_LIBS) : 
	( cd $(IO_GRIB_SHARE); \
          make CC="$(CC)" CFLAGS="$(CCFLAGS)" FC="$(FFC) $(FCTYPE) $(FCDEBUG) \
            -w $(EXTRA_FCFLAGS)" RM="$(RM)" CPP="$(CPP) $(CPPFLAGS)" FIXED="$(FCFIXED)" archive)
	$(RANLIB) $(IO_GRIB_SHARE)/libio_grib_share.a

