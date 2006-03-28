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
  $(DA_SRC)/LAPACK.o $(DA_SRC)/BLAS.o ../frame/module_wrf_error.o \
  $(DA_SRC)/da_fftpack5.o

include ../configure.wrf

gen_be_stage0_wrf : gen_be_stage0_wrf.o
	$(LD) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o -L$(NETCDF)/lib -lnetcdf

gen_be_stage1 : gen_be_stage1.o
	$(LD) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o

gen_be_stage1_1dvar : gen_be_stage1_1dvar.o
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

gen_be_stage0_wrf.o: 
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) -I${NETCDF}/include gen_be_stage0_wrf.F

gen_be_stage1.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage1.F

gen_be_stage1_1dvar.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage1_1dvar.F

gen_be_stage2.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage2.F

gen_be_stage2_1dvar.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage2_1dvar.F

gen_be_stage2a.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage2a.F

gen_be_stage3.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage3.F

gen_be_stage4_global.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage4_global.F

gen_be_stage4_regional.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_stage4_regional.F

gen_be_cov2d.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_cov2d.F

gen_be_cov3d.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_cov3d.F

gen_be_diags.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_diags.F

gen_be_diags_read.o:
	$(RM) $@
	$(FC) -c $(FCFORM) $(FCTYPE) $(MODULE_DIRS) gen_be_diags_read.F

clean: 
	@ echo 'use the clean script'

# DEPENDENCIES : $(GEN_BE_OBJS) only dependencies after this line (don't remove the word DEPENDENCIES)

