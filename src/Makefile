# gen_be

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

include ../configure.gen_be


GEN_BE_OBJS = $(WRFDA_DIR)/var/build/da_etkf.o \
	$(WRFDA_DIR)/var/build/da_blas.o \
	$(WRFDA_DIR)/var/build/da_lapack.o \
	$(WRFDA_DIR)/var/build/da_gen_be.o \
	$(WRFDA_DIR)/var/build/da_control.o \
	$(WRFDA_DIR)/var/build/da_be_spectral.o \
	$(WRFDA_DIR)/var/build/module_wrf_error.o \
	$(WRFDA_DIR)/var/build/da_reporting.o \
	$(WRFDA_DIR)/var/build/da_tools_serial.o \
	$(WRFDA_DIR)/var/build/module_ffts.o 

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
	gen_be_ensmean.exe 


GEN_BE_LIBS = $(WRFDA_DIR)/external/io_netcdf/libwrfio_nf.a
GEN_BE_LIB = $(LIB_EXTERNAL) -L$(GEN_BE_SRC_ROOT_DIR)/external/fftpack/fftpack5 -lfftpack

gen_be_stage0_wrf.exe : gen_be_stage0_wrf.o  $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o $(GEN_BE_LIB)

gen_be_stage0_gsi.exe : gen_be_stage0_gsi.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_gsi.o $(GEN_BE_LIB)

gen_be_ep1.exe     : gen_be_ep1.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep1.o $(GEN_BE_LIB)

gen_be_ep2.exe     : gen_be_ep2.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep2.o $(GEN_BE_LIB)

gen_be_stage1.exe : gen_be_stage1.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage1_gsi.exe : gen_be_stage1_gsi.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage1_gsi.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_gsi.o $(GEN_BE_LIB)

gen_be_stage1_1dvar.exe : gen_be_stage1_1dvar.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(GEN_BE_LIB)

gen_be_stage2.exe : gen_be_stage2.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage2_gsi.exe : gen_be_stage2_gsi.o
	if [ -n "$(DMPARALLEL)" ] ;   then \
	$(DM_FC) -o gen_be_stage2_gsi.exe $(LDFLAGS)  gen_be_stage2_gsi.o ;\
	else \
	$(SFC) -o gen_be_stage2_gsi.exe $(LDFLAGS)  gen_be_stage2_gsi.o ;\
	fi
	
gen_mbe_stage2.exe : gen_mbe_stage2.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_mbe_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_mbe_stage2.o $(GEN_BE_LIB)

gen_be_stage2_1dvar.exe : gen_be_stage2_1dvar.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o $(GEN_BE_LIB)

gen_be_stage2a.exe : gen_be_stage2a.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(GEN_BE_LIB) 

gen_be_stage3.exe : gen_be_stage3.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(GEN_BE_LIB)

gen_be_stage4_global.exe : gen_be_stage4_global.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(GEN_BE_LIB)

gen_be_stage4_regional.exe : gen_be_stage4_regional.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o $(GEN_BE_LIB)

gen_be_cov2d.exe : gen_be_cov2d.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o $(GEN_BE_LIB)

gen_be_cov3d.exe : gen_be_cov3d.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o $(GEN_BE_LIB)

gen_be_cov3d3d_bin3d_contrib.exe : gen_be_cov3d3d_bin3d_contrib.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov3d3d_bin3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d3d_bin3d_contrib.o $(GEN_BE_LIB)

gen_be_cov3d3d_contrib.exe : gen_be_cov3d3d_contrib.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov3d3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d3d_contrib.o $(GEN_BE_LIB)

gen_be_cov2d3d_contrib.exe : gen_be_cov2d3d_contrib.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov2d3d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d3d_contrib.o $(GEN_BE_LIB)

gen_be_cov3d2d_contrib.exe : gen_be_cov3d2d_contrib.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov3d2d_contrib.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d2d_contrib.o $(GEN_BE_LIB)

gen_be_diags.exe : gen_be_diags.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_diags_read.exe : gen_be_diags_read.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o $(GEN_BE_LIB)

gen_be_etkf.exe : gen_be_etkf.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o $(GEN_BE_LIB)

gen_be_ensrf.exe : gen_be_ensrf.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o $(GEN_BE_LIB)

gen_be_ensmean.exe : gen_be_ensmean.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o $(GEN_BE_LIB)

gen_be_hist.exe : gen_be_hist.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_hist.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_hist.o $(GEN_BE_LIB)

gen_be_vertloc.exe     : gen_be_vertloc.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o $@ $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_vertloc.o $(GEN_BE_LIB)

superclean:
	$(RM) *.f *.o *.mod *.exe
