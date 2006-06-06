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

GEN_BE_OBJS = da_gen_be.o da_constants.o da_be_spectral.o lapack.o blas.o module_wrf_error.o \
  fftpack5.o da_tracing.o da_memory.o

gen_be_stage0_wrf : gen_be_stage0_wrf.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_stage0_wrf.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage1 : gen_be_stage1.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage2 : gen_be_stage2.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage2a : gen_be_stage2a.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS) 

gen_be_stage3 : gen_be_stage3.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage4_global : gen_be_stage4_global.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_stage4_regional : gen_be_stage4_regional.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_cov2d : gen_be_cov2d.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_cov3d : gen_be_cov3d.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_diags : gen_be_diags.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

gen_be_diags_read : gen_be_diags_read.o $(GEN_BE_OBJS)
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o -L$(NETCDF)/lib -lnetcdf $(EXTRA_LIBS)

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

