include configure.wrfvar

all_wrfvar: serial parallel
# Do not add convertor to serial target, as it does
# not compile with pgi and g95 compilers
serial: wrfvar be da_utils
parallel : wrfvar

setup : depend

# preserve this makefile from overwriting
links : 
	@ cp makefile makefile.old
	@ $(LN) $(CHEM)/* .
	@ $(LN) $(DYN_NMM)/* .
	@ $(LN) $(DYNAMICS)/* .
	@ $(LN) $(GEN_BE)/* .
	@ $(LN) $(INC)/* .
	@ $(LN) $(SHARE)/* .
	@ $(LN) $(FRAME)/* .
	@ $(LN) $(MAIN)/* .
	@ $(LN) $(PHYS)/* .
	@ $(LN) $(TOOLS)/*.c .
	@ $(LN) $(TOOLS)/*.h .
	@ $(LN) $(DA)/*/* .
	@ $(LN) $(IO_INT)/* .
	@ mv diffwrf.F diffwrf_int.F
	@ $(LN) $(IO_NETCDF)/* .
	@ mv diffwrf.F90 diffwrf_netcdf.F
	@ $(LN) $(IO_PNETCDF)/field_routines.F90 field_routines_pnc.F90
	@ $(LN) $(IO_PNETCDF)/module_wrfsi_static.F90 module_wrfsi_static_pnc.F90 
	@ $(LN) $(IO_PNETCDF)/wrf_io.F90 wrf_io_pnc.F90 
	@ $(LN) $(IO_PNETCDF)/transpose.code transpose_pnc.code 
	@ $(LN) $(IO_PNETCDF)/ext* .
	@ $(LN) $(IOAPI_SHARE)/* .
	@ $(LN) $(ESMF_TIME_F90)/* .
	@ $(LN) $(IO_NETCDF)/*.code .
	@ $(LN) $(IO_PHDF5)/* .
	@ $(LN) $(ARCH)/*.sed .
	@ $(LN) $(CONVERTOR)/da_module_convert_tool/* .
	@ $(LN) $(CONVERTOR)/kma_wave2grid/* .
	@ $(LN) $(CONVERTOR)/wave2grid_kma/* .
	@ $(LN) $(CONVERTOR)/kma_netcdf/* .
	@ $(LN) $(CONVERTOR)/decode_l2_airs/* .
	@ mv makefile.old makefile
	@ touch links

GRIB_SHARE_LIBS = $(IO_GRIB_SHARE)/libio_grib_share.a

GRIB_SHARE_LIB  = -L$(IO_GRIB_SHARE) -lio_grib_share

WRF_OBJS=$(SHARE_OBJS) $(FRAME_OBJS) $(PHYS_OBJS) $(DYN_OBJS) \
  $(ESMF_TIME_F90_OBJS) $(IO_INT_OBJS) $(IO_NETCDF_OBJS) \
  module_wrf_top.o $(FRAME_SPECIAL_OBJS)

WRF_MODULES=$(SHARE_MODULES) $(FRAME_MODULES) $(PHYS_MODULES) $(DYN_MODULES)

WRFVAR_LIBS = $(NETCDF_LIBS) libesmf_time.a \
                 $(PNETCDF_LIBS) $(GRIB1_LIBS) $(GRIB2_LIBS) $(GRIB_SHARE_LIBS) \
                 libwrfvar.a $(MPI_LIBS)

WRF_LIBS    = libwrflib.a $(NETCDF_LIBS) libesmf_time.a \
                 $(GRIB1_LIBS) $(GRIB2_LIBS) $(GRIB_SHARE_LIBS) \
                 $(MPI_LIBS) $(PNETCDF_LIBS)

WRFVAR_LIB  = -L. $(LAPACK_LIB) -lwrfvar $(NETCDF_LIB) $(PNETCDF_LIB)  \
                 $(GRIB1_LIB) $(GRIB2_LIB) $(GRIB_SHARE_LIB) $(LOCAL_LIB) \
                 $(ESMF_TIME_LIB) \
                 $(LAPACK_LIB) $(BLAS_LIB) $(BUFR_LIB) $(FFTPACK_LIB) \
                 $(RTTOV_LIB) $(CRTM_LIB) $(MPI_LIB)

WRF_LIB     = -L. -lwrflib \
                 $(GRIB1_LIB) $(GRIB2_LIB) $(GRIB_SHARE_LIB) $(LOCAL_LIB) \
                 $(NETCDF_LIB) \
                 $(MPI_LIB) $(PNETCDF_LIB)

GEN_BE_LIB =  $(NETCDF_LIB) -L. $(LAPACK_LIB) $(BLAS_LIB) $(FFTPACK_LIB) \
              $(LOCAL_GEN_BE_LIB) $(LOCAL_LIB)


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
include io_pnetcdf.make
include io_phdf5.make
include io_grib.make
include ioapi_share.make
include main.make
include RSL.make
include RSL_LITE.make

# makedepf90 finds lots of module_initialise versions, and picks the squall_2d one
# which is wrong, try to encourage it to use the correct one by manually defining
# the test cases in main.make

IGNORE="real_$(SOLVER)|nup_$(SOLVER)|ndown_$(SOLVER)|ideal"

depend : links arch registry module_state_description.F md_calls.inc \
                      inc/da_generic_boilerplate.inc
	makedepf90 -free $(FPPFLAGS) \
           *.F *.f90 *.F90 2>/dev/null \
           | grep -vEe $(IGNORE) > depend.txt
	touch depend
	
include depend.txt

libwrflib.a : $(WRF_OBJS) $(WRF_MODULES)
	$(AR) libwrflib.a $(WRF_OBJS) $(WRF_MODULES)
	$(RANLIB) libwrflib.a

libwrfvar.a : $(WRFVAR_OBJS)
	$(AR) libwrfvar.a $(WRFVAR_OBJS)
	$(RANLIB) libwrfvar.a

clean : arch_clean
	@ rm -rf *.o *.mod *.b *.a *.f frame/*.f90 \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          ../external/*.o ../external/*.a ../external/*/*.o ../external/*/*.a \
          inc/dm_comm_cpp_flags

superclean : clean arch_superclean
	@ ( cd ../external; make superclean )
	@ rm -rf *.exe *.f90 *.F90 *.F *.inc inc/*.inc *.int *.c *.h *.m4 *.code \
          bufrlib.param *.sed registry \
          convert_tool kma_wave2grid wave2grid_kma *.csh version_decl \
          *.bsh *.csh Utl* macros_utl module_dm_warning rsl_cpp_flags \
          scl_plotting_routines configure.wrfvar ../configure.wrfvar \
          decode_l2_airs.README time_window.nl WRFIO.doc Test1.out.correct \
          da_macros_utl links arch README* TEST_RESULTS g2lib grib1_util \
          landread.c.dist WGRIB test.grb diffwrf build MEL_grib1 bacio-1.3 \
          depend depend.txt ../main/*exe ../inc/namelist_script.inc
    
