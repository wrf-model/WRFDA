# gen_be

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

include ../configure.gen_be

GEN_BE_OBJS = \
	../external/blas/da_blas.o \
	../external/lapack/da_lapack.o

be : \
	gen_be_stage0_wrf.exe \
	gen_be_stage0_gsi.exe \
	gen_be_ep1.exe \
	gen_be_ep2.exe \
	gen_be_stage1.exe \
	gen_be_vertloc.exe \
	gen_be_stage1_gsi.exe \
	gen_be_stage1_1dvar.exe \
	gen_be_stage2.exe \
	gen_be_stage2_gsi.exe \
	gen_mbe_stage2.exe \
	gen_be_stage2_1dvar.exe \
	gen_be_stage2a.exe \
	gen_be_stage3.exe \
	gen_be_stage4_global.exe \
	gen_be_stage4_regional.exe \
	gen_be_cov2d.exe \
	gen_be_cov3d.exe \
	gen_be_cov3d3d_bin3d_contrib.exe \
	gen_be_cov3d3d_contrib.exe \
	gen_be_cov2d3d_contrib.exe \
	gen_be_cov3d2d_contrib.exe \
	gen_be_diags.exe \
	gen_be_diags_read.exe \
	gen_be_hist.exe \
	gen_be_ensrf.exe \
	gen_be_etkf.exe \
	gen_be_ensmean.exe \
	da_advance_time.exe 


GEN_BE_LIB = $(GEN_BE_SRC_ROOT_DIR)/external/tools/libtools.a -L$(GEN_BE_SRC_ROOT_DIR)/external/fftpack/fftpack5 -lfftpack $(GEN_BE_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a $(LIB_EXTERNAL)

da_advance_time.exe : da_advance_time.o
	$(RM) $@
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) da_advance_time.F | sed '/integer *, *external.*iargc/d' > da_advance_time.f90 ;\
        else \
           $(CPP) $(CPPFLAGS) da_advance_time.F > da_advance_time.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) da_advance_time.f90
	$(SFC) $(LDFLAGS) -o $@ da_advance_time.o

gen_be_stage0_wrf.exe : gen_be_stage0_wrf.o
	$(RM) $@
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_stage0_wrf.F | sed '/integer *, *external.*iargc/d' > gen_be_stage0_wrf.f90 ;\
        else \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_stage0_wrf.F > gen_be_stage0_wrf.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) $(MODULE_DIRS) $(PROMOTION) $(FCSUFFIX) gen_be_stage0_wrf.f90
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o $(GEN_BE_LIB)

gen_be_stage0_gsi.exe : gen_be_stage0_gsi.o
	$(RM) $@
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_stage0_gsi.F | sed '/integer *, *external.*iargc/d' > gen_be_stage0_gsi.f90 ;\
        else \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_stage0_gsi.F > gen_be_stage0_gsi.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) $(MODULE_DIRS) $(PROMOTION) $(FCSUFFIX) gen_be_stage0_gsi.f90
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_gsi.o $(GEN_BE_LIB)

gen_be_ep1.exe     : gen_be_ep1.o  
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep1.o $(GEN_BE_LIB)

gen_be_ep2.exe     : gen_be_ep2.o 
	$(RM) $@
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_ep2.F | sed '/integer *, *external.*iargc/d' > gen_be_ep2.f90 ;\
        else \
           $(CPP) $(CPPFLAGS) $(OMPCPP) -I$(NETCDFPATH)/include gen_be_ep2.F > gen_be_ep2.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) $(MODULE_DIRS) $(PROMOTION) $(FCSUFFIX) gen_be_ep2.f90
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep2.o $(GEN_BE_LIB)

gen_be_stage1.exe : gen_be_stage1.o 
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage1_gsi.exe : gen_be_stage1_gsi.o 
	$(SFC) -o gen_be_stage1_gsi.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_gsi.o $(GEN_BE_LIB)

gen_be_stage1_1dvar.exe : gen_be_stage1_1dvar.o 
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(GEN_BE_LIB)

gen_be_stage2.exe : gen_be_stage2.o 
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage2_gsi.exe : gen_be_stage2_gsi.o
	if [ -n "$(DMPARALLEL)" ] ;   then \
	$(DM_FC) -o gen_be_stage2_gsi.exe $(LDFLAGS)  gen_be_stage2_gsi.o ;\
	else \
	$(SFC) -o gen_be_stage2_gsi.exe $(LDFLAGS)  gen_be_stage2_gsi.o ;\
	fi
	
gen_mbe_stage2.exe : gen_mbe_stage2.o
	$(SFC) -o gen_mbe_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_mbe_stage2.o $(GEN_BE_LIB)

gen_be_stage2_1dvar.exe : gen_be_stage2_1dvar.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o $(GEN_BE_LIB)

gen_be_stage2a.exe : gen_be_stage2a.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(GEN_BE_LIB) 

gen_be_stage3.exe : gen_be_stage3.o 
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(GEN_BE_LIB)

gen_be_stage4_global.exe : gen_be_stage4_global.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(GEN_BE_LIB)

gen_be_stage4_regional.exe : gen_be_stage4_regional.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o $(GEN_BE_LIB)

gen_be_cov2d.exe : gen_be_cov2d.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o $(GEN_BE_LIB)

gen_be_cov3d.exe : gen_be_cov3d.o 
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o $(GEN_BE_LIB)

gen_be_cov3d3d_bin3d_contrib.exe : gen_be_cov3d3d_bin3d_contrib.o
	$(SFC) -o gen_be_cov3d3d_bin3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d3d_bin3d_contrib.o $(GEN_BE_LIB)

gen_be_cov3d3d_contrib.exe : gen_be_cov3d3d_contrib.o
	$(SFC) -o gen_be_cov3d3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d3d_contrib.o $(GEN_BE_LIB)

gen_be_cov2d3d_contrib.exe : gen_be_cov2d3d_contrib.o
	$(SFC) -o gen_be_cov2d3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d3d_contrib.o $(GEN_BE_LIB)

gen_be_cov3d2d_contrib.exe : gen_be_cov3d2d_contrib.o
	$(SFC) -o gen_be_cov3d2d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d2d_contrib.o $(GEN_BE_LIB)

gen_be_diags.exe : gen_be_diags.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_diags_read.exe : gen_be_diags_read.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o $(GEN_BE_LIB)

gen_be_etkf.exe : gen_be_etkf.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o $(GEN_BE_LIB)

gen_be_ensrf.exe : gen_be_ensrf.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o $(GEN_BE_LIB)

gen_be_ensmean.exe : gen_be_ensmean.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o $(GEN_BE_LIB)

gen_be_hist.exe : gen_be_hist.o
	$(SFC) -o gen_be_hist.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_hist.o $(GEN_BE_LIB)

gen_be_vertloc.exe     : gen_be_vertloc.o
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_vertloc.o $(GEN_BE_LIB)

superclean:
	$(RM) *.f *.o *.mod *.exe
