#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

$(SOLVER) : $(SOLVER)_stage0 $(SOLVER)_stage1 $(SOLVER)_stage2 $(SOLVER)_stage3 $(SOLVER)_stage4 \
	$(SOLVER)_stage4_scale $(SOLVER)_cov2d $(SOLVER)_cov3d $(SOLVER)_diags $(SOLVER)_diags_read

include ../../configure.wrf

$(SOLVER)_stage0 : $(SOLVER)_stage0.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage0.exe $(LDFLAGS) $(SOLVER)_stage0.o ../libwrflib.a $(LIB)

$(SOLVER)_stage1 : $(SOLVER)_stage1.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage1.exe $(LDFLAGS) $(SOLVER)_stage1.o ../libwrflib.a $(LIB)

$(SOLVER)_stage2 : $(SOLVER)_stage2.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage2.exe $(LDFLAGS) $(SOLVER)_stage2.o ../libwrflib.a $(LIB)

$(SOLVER)_stage3 : $(SOLVER)_stage3.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage3.exe $(LDFLAGS) $(SOLVER)_stage3.o ../libwrflib.a $(LIB)

$(SOLVER)_stage4 : $(SOLVER)_stage4.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage4.exe $(LDFLAGS) $(SOLVER)_stage4.o ../libwrflib.a $(LIB)

$(SOLVER)_stage4_scale : $(SOLVER)_stage4_scale.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_stage4_scale.exe $(LDFLAGS) $(SOLVER)_stage4_scale.o ../libwrflib.a $(LIB)

$(SOLVER)_cov2d : $(SOLVER)_cov2d.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_cov2d.exe $(LDFLAGS) $(SOLVER)_cov2d.o ../libwrflib.a $(LIB)

$(SOLVER)_cov3d : $(SOLVER)_cov3d.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_cov3d.exe $(LDFLAGS) $(SOLVER)_cov3d.o ../libwrflib.a $(LIB)

$(SOLVER)_diags : $(SOLVER)_diags.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_diags.exe $(LDFLAGS) $(SOLVER)_diags.o ../libwrflib.a $(LIB)

$(SOLVER)_diags_read : $(SOLVER)_diags_read.o
	$(RANLIB) ../libwrflib.a
	$(LD) -o $(SOLVER)_diags_read.exe $(LDFLAGS) $(SOLVER)_diags_read.o ../libwrflib.a $(LIB)

#	$(CPP) -I. $(CPPFLAGS_3DVAR) gen_be_stage0.f > gen_be_stage0.f
#	../../da_3dvar/src/DA_Constants.o
gen_be_stage0.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage0.f

gen_be_stage1.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage1.f

gen_be_stage2.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage2.f

gen_be_stage3.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage3.f

gen_be_stage4.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage4.f

gen_be_stage4_scale.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS_3DVAR) $(MODULE_DIRS) gen_be_stage4_scale.f

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


