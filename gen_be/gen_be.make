#

be :		arch                    \
                gen_be_stage0_wrf	\
		gen_be_stage1	        \
		gen_be_stage1_1dvar	\
		gen_be_stage2	        \
		gen_be_stage2_1dvar	\
		gen_be_stage2a	        \
		gen_be_stage3	        \
		gen_be_stage4_global	\
		gen_be_stage4_regional	\
		gen_be_cov2d		\
		gen_be_cov3d		\
		gen_be_diags		\
		gen_be_diags_read       \
                advance_cymdh

GEN_BE_OBJS = da_gen_be.o DA_Constants.o be_spectral.o LAPACK.o BLAS.o module_wrf_error.o \
  da_fftpack5.o da_tracing.o da_memory.o

gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_stage0_wrf.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o  $(EXTRA_LIBS)

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o DA_Constants.o
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(EXTRA_LIBS)

gen_be_stage2 : gen_be_stage2.o
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(EXTRA_LIBS)

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o  $(EXTRA_LIBS)

gen_be_stage2a : gen_be_stage2a.o
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(EXTRA_LIBS) 

gen_be_stage3 : gen_be_stage3.o
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(EXTRA_LIBS)

gen_be_stage4_global : gen_be_stage4_global.o
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(EXTRA_LIBS)

gen_be_stage4_regional : gen_be_stage4_regional.o
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o  $(EXTRA_LIBS)

gen_be_cov2d : gen_be_cov2d.o
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o  $(EXTRA_LIBS)

gen_be_cov3d : gen_be_cov3d.o
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o  $(EXTRA_LIBS)

gen_be_diags : gen_be_diags.o
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o  $(EXTRA_LIBS)

gen_be_diags_read : gen_be_diags_read.o
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o  $(EXTRA_LIBS)



gen_be_stage0_wrf.o: $(GEN_BE_OBJS)
	$(RM) $@
	$(FC) -c $(FCFLAGS) -I${NETCDF}/include gen_be_stage0_wrf.F

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

