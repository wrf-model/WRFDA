include configure.wrf

links : collect_on_comm.c
	@ $(LN) ../*/*.make .
	@ $(LN) ../*/*.f90 .
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
	@ $(LN) $(FRAME)/collect_on_comm.c .

# Manual rules specified first take precedence

include $(DYNAMICS)/dyn_$(SOLVER).make
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
include $(RSL_LITE)/RSL_LITE.make
include $(MAIN)/main.make

depend : links module_state_description.f90 md_calls.inc \
                      inc/da_generic_boilerplate.inc
	~bray/bin/makedepf90 $(FPPFLAGS) *.F *.f90 *.F90 > depend.txt 2>/dev/null

include depend.txt

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

WRFPLUS_OBJS=$(SHARE_OBJS_WRFPLUS) $(DYN_OBJS_WRFPLUS)

WRFPLUS_MODULES=$(SHARE_MODULES_WRFPLUS) $(DYN_MODULES_WRFPLUS)

libwrflib.a : $(WRF_MODULES) \
                $(WRF_OBJS)
	$(AR) libwrflib.a $(WRF_MODULES) $(WRF_OBJS)
	$(RANLIB) libwrflib.a

libwrfplus.a : $(WRFPLUS_MODULES_WRFPLUS) $(WRFPLUS_OBJS)
	$(AR) libwrfplus.a $(WRFPLUS_MODULES) $(WRFPLUS_OBJS)
	$(RANLIB) libwrfplus.a

clean :
	@ $(RM) *.exe *.o *.mod *.b *.a *.f inc/*.inc frame/*.f90 registry \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          $(RSL)/RSL/*.a $(RSL)/RSL/*.o

superclean : clean
	@ $(RM) *.f90 *.F90 *.F *.inc *.int *.c *.h *.m4 *.code *.make \
          bufrlib.param *.sed registry \
          convert_tool kma_wave2grid wave2grid_kma *.csh version_decl \
          *.bsh *.csh
