#

GEN_BE_OBJS = da_etkf.o da_gen_be.o da_control.o da_be_spectral.o module_wrf_error.o \
  module_driver_constants.o da_tracing.o da_memory.o da_reporting.o da_tools1.o

be :		setup                   \
                $(GEN_BE_LIBS)          \
                $(GEN_BE_OBJS)          \
                gen_be_stage0_wrf	\
                gen_be_ep1		\
                gen_be_ep2		\
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
		gen_be_ensrf            \
		gen_be_etkf             \
		gen_be_ensmean          \
                advance_cymdh.exe

gen_be_stage0_wrf.o      : gen_be_stage0_wrf.f90

gen_be_ep1.o             : gen_be_ep1.f90

gen_be_ep2.o             : gen_be_ep2.f90

gen_be_stage1_wrf.o      : gen_be_stage1_wrf.f90

gen_be_stage1_1dvar.o    : gen_be_stage1_1dvar.f90

gen_be_stage2.o          : gen_be_stage2.f90

gen_be_stage2_1dvar.o    : gen_be_stage2_1dvar.f90

gen_be_stage2a.o         : gen_be_stage2a.f90

gen_be_stage3.o          : gen_be_stage3.f90

gen_be_stage4_global.o   : gen_be_stage4_global.f90

gen_be_stage4_regional.o : gen_be_stage4_regional.f90

gen_be_cov2d.o           : gen_be_cov2d.f90

gen_be_cov3d.o           : gen_be_cov3d.f90

gen_be_diags.o           : gen_be_diags.f90

gen_be_diags_read.o      : gen_be_diags_read.f90

gen_be_etkf.o            : gen_be_etkf.f90

gen_be_ensrf.o           : gen_be_ensrf.f90

gen_be_ensmean.o         : gen_be_ensmean.f90

gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o $(GEN_BE_LIB)

gen_be_ep1     : gen_be_ep1.o
	$(LD) -o gen_be_ep1.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep1.o $(GEN_BE_LIB)

gen_be_ep2     : gen_be_ep2.o
	$(LD) -o gen_be_ep2.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep2.o $(GEN_BE_LIB)

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o
	$(LD) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(GEN_BE_LIB)

gen_be_stage2 : gen_be_stage2.o
	$(LD) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage2_1dvar : gen_be_stage2_1dvar.o
	$(LD) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o $(GEN_BE_LIB)

gen_be_stage2a : gen_be_stage2a.o
	$(LD) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(GEN_BE_LIB) 

gen_be_stage3 : gen_be_stage3.o
	$(LD) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(GEN_BE_LIB)

gen_be_stage4_global : gen_be_stage4_global.o
	$(LD) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(GEN_BE_LIB)

gen_be_stage4_regional : gen_be_stage4_regional.o
	$(LD) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o $(GEN_BE_LIB)

gen_be_cov2d : gen_be_cov2d.o
	$(LD) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o $(GEN_BE_LIB)

gen_be_cov3d : gen_be_cov3d.o
	$(LD) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o $(GEN_BE_LIB)

gen_be_diags : gen_be_diags.o
	$(LD) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_diags_read : gen_be_diags_read.o
	$(LD) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o $(GEN_BE_LIB)

gen_be_etkf : gen_be_etkf.o
	$(LD) -o gen_be_etkf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o $(GEN_BE_LIB)

gen_be_ensrf : gen_be_ensrf.o
	$(LD) -o gen_be_ensrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o $(GEN_BE_LIB)

gen_be_ensmean : gen_be_ensmean.o
	$(LD) -o gen_be_ensmean.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o $(GEN_BE_LIB)

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

