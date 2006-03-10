include configure.wrf

LIBS = libwrflib.a libwrfio_nf.a libio_grib1.a libMEL_grib1.a libgrib1_util.a libesmf_time.a

include dyn_em.make
include share.make
include frame.make
include phys.make
include da_3dvar.make
include convertor.make
include tools.make
include esmf_time_f90.make
include esmf_wrapper.make
include io_int.make
include io_netcdf.make
include io_grib1.make
include ioapi_share.make
include main.make

OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_EM_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) \
  module_wrf_top.o

MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_EM_MODULES)

libwrflib.a : $(MODULES) $(OBJS)
	$(AR) ru libwrflib.a $(MODULES) $(OBJS)
	$(RANLIB) libwrflib.a

WRF_MODULES = module_wrf_top.o

wrf.exe : $(MODULES) $(WRF_LIBS) wrf.o
	$(LD) $(LDFLAGS) -o wrf.exe wrf.o $(WRF_MODULES) $(LIB)

clean :
	@ $(RM) *.exe *.o *.mod *.b *.a inc/*.inc inc/*.F registry
