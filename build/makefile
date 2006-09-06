include configure.wrf

setup : depend

makedepf90.exe : 
	@ ( cd $(EXTERNAL)/makedepf90/makedepf90-2.8.8; export CC=$(CC_TOOLS); \
          touch configure; ./configure; touch find_dep.c find_dep.h; make )
	cp $(EXTERNAL)/makedepf90/makedepf90-2.8.8/makedepf90 makedepf90.exe

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
	@ $(LN) $(TOOLS)/*.f90 .
	@ $(LN) $(TOOLS)/*.c .
	@ $(LN) $(TOOLS)/*.h .
	@ $(LN) $(DA)/*/* .
	@ $(LN) $(IO_INT)/* .
	@ $(LN) $(IO_NETCDF)/* .
	@ $(LN) $(IO_GRIB_SHARE)/* .
	@ $(LN) $(IO_GRIB1)/* .
	@ $(LN) $(IO_GRIB2)/* .
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
	@ $(LN) $(CONVERTOR)/kma_netcdf/* .
	@ $(LN) $(CONVERTOR)/decode_l2_airs/* .
	@ touch links


# Manual rules specified first take precedence

include da.make
include dyn_em.make
include share.make
include frame.make
include phys.make
include gen_be.make
include convertor.make
include decode_l2_airs.make
include tools.make
include esmf_time_f90.make
include io_int.make
include io_netcdf.make
include io_grib.make
include ioapi_share.make
include bufr_ncep_nco.make
include lapack.make
include blas.make
include fftpack5.make
include RSL_LITE.make
include main.make

real_em.o : real_em.F version_decl module_bc.o module_big_step_utilities_em.o \
              module_date_time.o module_optional_si_input.o module_bc_time_utilities.o \
              module_dm.o module_utility.o module_timing.o module_configure.o \
              module_driver_constants.o module_io_domain.o module_initialize_real.o \
              module_domain.o module_machine.o 


depend : arch links registry makedepf90.exe module_state_description.F md_calls.inc \
                      inc/da_generic_boilerplate.inc
	./makedepf90.exe $(FPPFLAGS) \
           *.F *.f90 *.F90  2>/dev/null | grep -v 'real_em.o :' > depend.txt
	

include depend.txt

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

WRFPLUS_OBJS=$(SHARE_OBJS_WRFPLUS) $(DYN_OBJS_WRFPLUS)

WRFPLUS_MODULES=$(SHARE_MODULES_WRFPLUS) $(DYN_MODULES_WRFPLUS)

libwrflib.a : $(WRF_OBJS) $(WRF_MODULES)
	$(AR) libwrflib.a $(WRF_OBJS) $(WRF_MODULES)
	$(RANLIB) libwrflib.a

libwrfplus.a : $(WRFPLUS_OBJS) $(WRFPLUS_MODULES)
	$(AR) libwrfplus.a $(WRFPLUS_OBJS) $(WRFPLUS_MODULES)
	$(RANLIB) libwrfplus.a

libwrfvar.a : $(WRFVAR_OBJS)
	$(AR) libwrfvar.a $(WRFVAR_OBJS)
	$(RANLIB) libwrfvar.a

clean : arch_clean
	@ $(RM) *.exe *.o *.mod *.b *.a *.f inc/*.inc frame/*.f90 registry \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          $(EXTERNAL)/*.o $(EXTERNAL)/*.a $(EXTERNAL)/*/*.o $(EXTERNAL)/*/*.a \
          inc/dm_comm_cpp_flags depend
	@ ( cd $(EXTERNAL); make clean )

superclean : clean arch_superclean
	@ ( cd $(EXTERNAL); make superclean )
	@ $(RM) *.f90 *.F90 *.F *.inc *.int *.c *.h *.m4 *.code \
          bufrlib.param *.sed registry \
          convert_tool kma_wave2grid wave2grid_kma *.csh version_decl \
          *.bsh *.csh Utl* macros_utl module_dm_warning rsl_cpp_flags \
          scl_plotting_routines configure.wrf ../configure.wrf \
          decode_l2_airs.README time_window.nl \
          da_macros_utl links arch
