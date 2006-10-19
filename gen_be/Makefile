#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

gen_be : 	gen_be_ensrf	\
		gen_be_etkf	\
		gen_be_ensmean	\
		gen_be_stage0_wrf	\
		gen_be_ep1	\
		gen_be_ep2	\
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

GEN_BE_OBJS = $(DA)/da_etkf.o $(DA)/da_gen_be.o $(DA)/da_constants.o $(DA)/da_be_spectral.o \
  ../frame/module_wrf_error.o \
  ../frame/module_driver_constants.o $(DA)/da_tracing.o $(DA)/da_memory.o

include ../configure.wrf

gen_be_ensrf : gen_be_ensrf.o
	$(LD) -o gen_be_ensrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o ${GEN_BE_LIB}

gen_be_etkf : gen_be_etkf.o
	$(LD) -o gen_be_etkf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o ${GEN_BE_LIB}

gen_be_ensmean : gen_be_ensmean.o
	$(LD) -o gen_be_ensmean.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o ${GEN_BE_LIB}

gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o ${GEN_BE_LIB}

gen_be_ep1 : gen_be_ep1.o
	$(LD) -o gen_be_ep1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ep1.o ${GEN_BE_LIB}

gen_be_ep2 : gen_be_ep2.o
	$(LD) -o gen_be_ep2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ep2.o ${GEN_BE_LIB}

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o ${GEN_BE_LIB}

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o ${GEN_BE_LIB}

gen_be_stage2 : gen_be_stage2.o
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o ${GEN_BE_LIB}

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o ${GEN_BE_LIB}

gen_be_stage2a : gen_be_stage2a.o
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o ${GEN_BE_LIB}

gen_be_stage3 : gen_be_stage3.o
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o ${GEN_BE_LIB}

gen_be_stage4_global : gen_be_stage4_global.o
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o ${GEN_BE_LIB}

gen_be_stage4_regional : gen_be_stage4_regional.o
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o ${GEN_BE_LIB}

gen_be_cov2d : gen_be_cov2d.o
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o ${GEN_BE_LIB}

gen_be_cov3d : gen_be_cov3d.o
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o ${GEN_BE_LIB}

gen_be_diags : gen_be_diags.o
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o ${GEN_BE_LIB}

gen_be_diags_read : gen_be_diags_read.o
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o ${GEN_BE_LIB}

#

gen_be_ensrf.o: 
	$(RM) $@
	$(FC) -c ${FCFREE} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_ensrf.f90

gen_be_etkf.o: 
	$(RM) $@
	$(FC) -c ${FCFREE} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_etkf.f90

gen_be_ensmean.o: 
	$(RM) $@
	$(FC) -c ${FCFREE} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_ensmean.f90

gen_be_stage0_wrf.o: 
	$(RM) $@
	$(FC) -c ${FCFREE} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_stage0_wrf.f90

gen_be_ep2.o: 
	$(RM) $@
	$(FC) -c ${FCFREE} $(FCBASE) ${FCTYPE} ${MODULE_DIRS} -I${NETCDF}/include gen_be_ep2.f90

clean: 
	@ echo 'use the clean script'

# DEPENDENCIES : $(GEN_BE_OBJS) only dependencies after this line (don't remove the word DEPENDENCIES)

