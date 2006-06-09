include configure.wrf

links :
	@ $(LN) ../*/*.make .
	@ $(LN) ../*/*.f90 .
	@ $(LN) ../*/*.inc .
	@ $(LN) ../*/*.F .
	@ $(LN) ../*/*.c .
	@ $(LN) ../*/*.h .
	@ $(LN) ../*/*sh .
	@ $(LN) $(DA)/*/*.f90 .
	@ $(LN) $(DA)/*/*.inc .
	@ $(LN) $(DA)/*/*.c .
	@ $(LN) $(DA)/*/*sh .
	@ $(LN) $(DA)/*/*m4 .
	@ $(LN) $(EXTERNAL)/*/*.make .
	@ $(LN) $(EXTERNAL)/*/*.f90 .
	@ $(LN) $(EXTERNAL)/*/*.F90 .
	@ $(LN) $(EXTERNAL)/*/*.inc .
	@ $(LN) $(EXTERNAL)/*/*.F .
	@ $(LN) $(EXTERNAL)/*/*.c .
	@ $(LN) $(EXTERNAL)/*/*.h .
	@ $(LN) $(EXTERNAL)/*/*sh ..
	@ $(LN) $(INC)/version_decl .
	@ $(LN) $(FRAME)/md_calls.m4 .
	@ $(LN) $(SHARE)/*.int .
	@ $(LN) $(BUFR_NCEP_NCO)/bufrlib.param .
	@ $(LN) $(IO_NETCDF)/*.code .
	@ $(LN) $(ARCH)/*.sed .
	@ $(LN) $(CONVERTOR)/convert_tool .
	@ $(LN) $(CONVERTOR)/kma_wave2grid .
	@ $(LN) $(CONVERTOR)/wave2grid_kma .
	@ $(LN) $(ESMF_TIME_F90)/*.F90 .
	@ $(CP) -r $(RSL) RSL
	@ $(LN) $(RSL)/*.F .
	@ $(LN) $(RSL)/*.c .
	@ $(LN) $(RSL)/RSL/*.c .
	@ $(LN) $(RSL)/RSL/*.h .
	@ $(LN) $(FRAME)/collect_on_comm.c .
	@ touch links

# Manual rules specified first take precedence

include ../dyn_$(SOLVER)/dyn_$(SOLVER).make
include $(SHARE)/share.make
include $(FRAME)/frame.make
include $(PHYS)/phys.make
include $(DA)/da.make
include $(GEN_BE)/gen_be.make
include $(CONVERTOR)/convertor.make
include $(TOOLS)/tools.make
include $(ESMF_TIME_F90)/esmf_time_f90.make
include $(IO_INT)/io_int.make
include $(IO_NETCDF)/io_netcdf.make
include $(IO_GRIB1)/io_grib1.make
include $(IOAPI_SHARE)/ioapi_share.make
include $(BUFR_NCEP_NCO)/bufr_ncep_nco.make
include $(LAPACK)/lapack.make
include $(BLAS)/blas.make
include $(FFTPACK5)/fftpack5.make
include $(MAIN)/main.make

include .depend

depend .depend :
	~/makedepf90/makedepf90-2.8.6/makedepf90 *.F *.f90 > .depend

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

libwrflib.a : $(WRF_MODULES) $(WRF_OBJS)
	$(AR) libwrflib.a $(WRF_MODULES) $(WRF_OBJS)
	$(RANLIB) libwrflib.a

clean :
	@ $(RM) *.exe *.o *.mod *.b *.a *.f inc/*.inc frame/*.f90 registry \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          RSL/RSL/*.a RSL/RSL/*.o

superclean : clean
	@ $(RM) *.f90 *.F90 *.F *.inc *.int *.c *.h *.m4 *.code *.make \
          bufrlib.param *.sed registry links \
          convert_tool kma_wave2grid wave2grid_kma *.csh version_decl \
          RSL *.bsh *.csh .depend
