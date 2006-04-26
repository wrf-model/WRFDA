include configure.wrf

WRF_LIBS = libwrflib.a libwrfio_nf.a libio_grib1.a libesmf_time.a

VAR_LIBS = ${WRF_LIBS} libwrfvar.a

include dyn_$(SOLVER).make
include share.make
include frame.make
include phys.make
include da_3dvar.make
include gen_be.make
include convertor.make
include tools.make
include esmf_time_f90.make
include esmf_wrapper.make
include io_int.make
include io_netcdf.make
include io_grib1.make
include ioapi_share.make
include main.make

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

libwrflib.a : $(WRF_MODULES) $(WRF_OBJS)
	$(AR) ru libwrflib.a $(WRF_MODULES) $(WRF_OBJS)
	$(RANLIB) libwrflib.a

clean :
	@ $(RM) *.exe *.o *.mod *.b *.a *.f inc/*.inc frame/*.F registry \
          Registry_tmp* module_state_description.F \
          '*.F90' '*.F' '*.f' '*.inc' '*.int' '*.c' '*.h' '*.make' '*.m4' \
          '*.code' '*.sed'

superclean : clean
	@ $(RM) *.F90 *.F *.f *.inc *.int *.c *.h *.m4 *.code \
          bufrlib.PRM *.sed registry gen_comms.stub inc/* \
          convert_tool kma_wave2grid wave2grid_kma mm5_wrf wrf_mm5
