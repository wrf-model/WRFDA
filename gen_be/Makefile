#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

$(SOLVER) :	$(SOLVER)_stage1	\
		$(SOLVER)_stage2	\
		$(SOLVER)_stage2a	\
		$(SOLVER)_stage3	\
		$(SOLVER)_stage4_global	\
		$(SOLVER)_stage4_regional	\
		$(SOLVER)_cov2d		\
		$(SOLVER)_cov3d		\
		$(SOLVER)_diags		\
		$(SOLVER)_diags_read

include ../configure.wrf

gen_be_stage0 : gen_be_stage0.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o gen_be_stage0.exe $(LDFLAGS) gen_be_stage0.o ../main/libwrflib.a $(LIB)

$(SOLVER)_stage1 : $(SOLVER)_stage1.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage1.exe $(LDFLAGS) $(SOLVER)_stage1.o ../main/libwrflib.a

$(SOLVER)_stage2 : $(SOLVER)_stage2.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage2.exe $(LDFLAGS) $(SOLVER)_stage2.o ../main/libwrflib.a

$(SOLVER)_stage2a : $(SOLVER)_stage2a.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage2a.exe $(LDFLAGS) $(SOLVER)_stage2a.o ../main/libwrflib.a

$(SOLVER)_stage3 : $(SOLVER)_stage3.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage3.exe $(LDFLAGS) $(SOLVER)_stage3.o ../main/libwrflib.a

$(SOLVER)_stage4_global : $(SOLVER)_stage4_global.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage4_global.exe $(LDFLAGS) $(SOLVER)_stage4_global.o ../main/libwrflib.a

$(SOLVER)_stage4_regional : $(SOLVER)_stage4_regional.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_stage4_regional.exe $(LDFLAGS) $(SOLVER)_stage4_regional.o ../main/libwrflib.a

$(SOLVER)_cov2d : $(SOLVER)_cov2d.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_cov2d.exe $(LDFLAGS) $(SOLVER)_cov2d.o ../main/libwrflib.a

$(SOLVER)_cov3d : $(SOLVER)_cov3d.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_cov3d.exe $(LDFLAGS) $(SOLVER)_cov3d.o ../main/libwrflib.a

$(SOLVER)_diags : $(SOLVER)_diags.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_diags.exe $(LDFLAGS) $(SOLVER)_diags.o ../main/libwrflib.a

$(SOLVER)_diags_read : $(SOLVER)_diags_read.o
	$(RANLIB) ../main/libwrflib.a
	$(LD) -o $(SOLVER)_diags_read.exe $(LDFLAGS) $(SOLVER)_diags_read.o ../main/libwrflib.a

#

gen_be_stage0.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage0.f

gen_be_stage1.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage1.f

gen_be_stage2.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage2.f

gen_be_stage2a.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage2a.f

gen_be_stage3.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage3.f

gen_be_stage4_global.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage4_global.f

gen_be_stage4_regional.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage4_regional.f

gen_be_cov2d.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_cov2d.f

gen_be_cov3d.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_cov3d.f

gen_be_diags.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_diags.f

gen_be_diags_read.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_diags_read.f

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

