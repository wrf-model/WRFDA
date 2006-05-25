#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

gen_be : 	gen_be_stage0_wrf	\
		gen_be_stage1	\
		gen_be_stage1_1dvar	\
		gen_be_stage2	\
		gen_be_stage2_1dvar	\
		gen_be_stage2a	\
		gen_be_stage3	\
		gen_be_stage4_global	\
		gen_be_stage4_regional	\
		gen_be_cov2d		\
		gen_be_cov3d		\
		gen_be_diags		\
		gen_be_diags_read

GEN_BE_OBJS = $(DA_SRC)/da_gen_be.o $(DA_SRC)/DA_Constants.o $(DA_SRC)/be_spectral.o \
  $(DA_SRC)/LAPACK.o $(DA_SRC)/BLAS.o ../frame/module_wrf_error.o ../frame/module_driver_constants.o \
  $(DA_SRC)/da_fftpack5.o $(DA_SRC)/da_tracing.o $(DA_SRC)/da_memory.o

include ../configure.wrf

GEN_BE_LIBS = -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)
gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o ${GEN_BE_LIBS}

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o ${GEN_BE_LIBS}

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o ${GEN_BE_LIBS}

gen_be_stage2 : gen_be_stage2.o
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o ${GEN_BE_LIBS}

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o ${GEN_BE_LIBS}

gen_be_stage2a : gen_be_stage2a.o
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o ${GEN_BE_LIBS}

gen_be_stage3 : gen_be_stage3.o
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o ${GEN_BE_LIBS}

gen_be_stage4_global : gen_be_stage4_global.o
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o ${GEN_BE_LIBS}

gen_be_stage4_regional : gen_be_stage4_regional.o
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o ${GEN_BE_LIBS}

gen_be_cov2d : gen_be_cov2d.o
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o ${GEN_BE_LIBS}

gen_be_cov3d : gen_be_cov3d.o
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o ${GEN_BE_LIBS}

gen_be_diags : gen_be_diags.o
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o ${GEN_BE_LIBS}

gen_be_diags_read : gen_be_diags_read.o
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o ${GEN_BE_LIBS}

#

gen_be_stage0_wrf.o: 
	$(RM) $@
	$(FC) -c ${FCFORM} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_stage0_wrf.F

clean: 
	@ echo 'use the clean script'

# DEPENDENCIES : $(GEN_BE_OBJS) only dependencies after this line (don't remove the word DEPENDENCIES)

