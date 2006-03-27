#

be :		arch \
                gen_be_stage0_wrf	\
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

GEN_BE_OBJS = da_gen_be.o DA_Constants.o be_spectral.o LAPACK.o BLAS.o module_wrf_error.o \
  da_fftpack5.o

gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_stage0_wrf.o -L$(NETCDF)/lib -lnetcdf

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o DA_Constants.o
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o

gen_be_stage2 : gen_be_stage2.o
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o 

gen_be_stage2a : gen_be_stage2a.o
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o 

gen_be_stage3 : gen_be_stage3.o
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o 

gen_be_stage4_global : gen_be_stage4_global.o
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o 

gen_be_stage4_regional : gen_be_stage4_regional.o
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o 

gen_be_cov2d : gen_be_cov2d.o
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o 

gen_be_cov3d : gen_be_cov3d.o
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o 

gen_be_diags : gen_be_diags.o
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o 

gen_be_diags_read : gen_be_diags_read.o
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o 

#

gen_be_stage0_wrf.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) -I${NETCDF}/include gen_be_stage0_wrf.F

gen_be_stage1.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage1.F

gen_be_stage1_1dvar.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage1_1dvar.F

gen_be_stage2.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage2.F

gen_be_stage2_1dvar.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage2_1dvar.F

gen_be_stage2a.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage2a.F

gen_be_stage3.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage3.F

gen_be_stage4_global.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage4_global.F

gen_be_stage4_regional.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_stage4_regional.F

gen_be_cov2d.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_cov2d.F

gen_be_cov3d.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_cov3d.F

gen_be_diags.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_diags.F

gen_be_diags_read.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) gen_be_diags_read.F

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

