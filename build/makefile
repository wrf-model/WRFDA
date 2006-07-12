include configure.wrf

setup : depend

makedepf90 : 
	@ ( cd $(EXTERNAL)/makedepf90/makedepf90-2.8.8; export CC=$(CC_TOOLS); ./configure; make )
	cp $(EXTERNAL)/makedepf90/makedepf90-2.8.8/makedepf90 .

links : 
	@ $(LN) $(CHEM)/* .
	@ $(LN) $(DYN_NMM)/* .
	@ $(LN) $(DYNAMICS)/* .
	@ $(LN) $(GEN_BE)/* .
	@ $(LN) $(INC)/* .
	@ $(LN) $(SHARE)/* .
	@ $(LN) $(FRAME)/* .
	@ $(LN) $(MAIN)/* .
	@ $(LN) $(PHYS)/* .
	@ $(LN) $(TOOLS)/*.make .
	@ $(LN) $(TOOLS)/*.f90 .
	@ $(LN) $(TOOLS)/*.c .
	@ $(LN) $(TOOLS)/*.h .
	@ $(LN) $(DA)/*/* .
	@ $(LN) $(IO_INT)/* .
	@ $(LN) $(IO_NETCDF)/* .
	@ $(LN) $(IO_GRIB_SHARE)/* .
	@ $(LN) $(IO_GRIB1)/* .
	@ $(LN) $(IOAPI_SHARE)/* .
	@ $(LN) $(BLAS)/* .
	@ $(LN) $(LAPACK)/* .
	@ $(LN) $(FFTPACK5)/* .
	@ $(LN) $(ESMF_TIME_F90)/* .
	@ $(LN) $(BUFR_NCEP_NCO)/* .
	@ $(LN) $(IO_NETCDF)/*.code .
	@ $(LN) $(ARCH)/*.sed .
	@ $(LN) $(CONVERTOR)/convert_tool/* .
	@ $(LN) $(CONVERTOR)/kma_wave2grid/* .
	@ $(LN) $(CONVERTOR)/wave2grid_kma/* .
	@ $(LN) $(CONVERTOR)/decode_l2_airs/* .
	@ touch links


# Manual rules specified first take precedence

include $(DYNAMICS)/dyn_$(SOLVER).make
include $(SHARE)/share.make
include $(FRAME)/frame.make
include $(PHYS)/phys.make
include $(DA)/da.make
include $(GEN_BE)/gen_be.make
include $(CONVERTOR)/convertor.make
include $(CONVERTOR)/decode_l2_airs/decode_l2_airs.make
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


depend : arch links registry makedepf90 module_state_description.f90 md_calls.inc \
                      inc/da_generic_boilerplate.inc
	./makedepf90 $(FPPFLAGS) \
           *.F *.f90 *.F90 > depend 2>/dev/null
	touch depend

include depend

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

WRFPLUS_OBJS=$(SHARE_OBJS_WRFPLUS) $(DYN_OBJS_WRFPLUS)

WRFPLUS_MODULES=$(SHARE_MODULES_WRFPLUS) $(DYN_MODULES_WRFPLUS)

libwrf.a : $(WRF_OBJS) wrf.o
	mv registry.o registry.o1
	$(AR) libwrf.a *.o
	mv registry.o1 registry.o
	$(RANLIB) libwrf.a

libwrfplus.a : $(WRF_OBJS) $(WRFPLUS_OBJS) wrf.o
	mv registry.o registry.o1
	$(AR) libwrfplus.a *.o
	mv registry.o1 registry.o
	$(RANLIB) libwrfplus.a

libwrfvar.a : $(WRF_OBJS) $(DA_OBJS) $(BUFR_OBJS) wrfvar.o
	mv registry.o registry.o1
	$(AR) libwrfvar.a *.o
	mv registry.o1 registry.o
	$(RANLIB) libwrfvar.a

clean : arch_clean
	@ $(RM) *.exe *.o *.mod *.b *.a *.f inc/*.inc frame/*.f90 registry \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          $(EXTERNAL)/*.o $(EXTERNAL)/*.a $(EXTERNAL)/*/*.o $(EXTERNAL)/*/*.a \
          inc/dm_comm_cpp_flags depend
	@ ( cd  $(EXTERNAL)/makedepf90/makedepf90-2.8.8; make clean )

superclean : clean arch_superclean
	@ $(RM) *.f90 *.F90 *.F *.inc *.int *.c *.h *.m4 *.code *.make \
          bufrlib.param *.sed registry \
          convert_tool kma_wave2grid wave2grid_kma *.csh version_decl \
          *.bsh *.csh Utl* macros_utl module_dm_warning rsl_cpp_flags \
          scl_plotting_routines configure.wrf ../configure.wrf \
          advance_cymdh.exe decode_l2_airs.README time_window.nl \
          Makefile_old da_macros_utl links arch
