#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

$(SOLVER) : $(SOLVER)_stage1 $(SOLVER)_stage2 $(SOLVER)_stage3 $(SOLVER)_stage4 $(SOLVER)_cov2d $(SOLVER)_cov3d \
         $(SOLVER)_diags $(SOLVER)_diags_read

$(SOLVER)_stage1 : $(SOLVER)_stage1.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_stage1.exe $(LDFLAGS) $(SOLVER)_stage1.o ../lib_3dvar.a

$(SOLVER)_stage2 : $(SOLVER)_stage2.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_stage2.exe $(LDFLAGS) $(SOLVER)_stage2.o ../lib_3dvar.a

$(SOLVER)_stage3 : $(SOLVER)_stage3.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_stage3.exe $(LDFLAGS) $(SOLVER)_stage3.o ../lib_3dvar.a

$(SOLVER)_stage4 : $(SOLVER)_stage4.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_stage4.exe $(LDFLAGS) $(SOLVER)_stage4.o ../lib_3dvar.a

$(SOLVER)_cov2d : $(SOLVER)_cov2d.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_cov2d.exe $(LDFLAGS) $(SOLVER)_cov2d.o ../lib_3dvar.a

$(SOLVER)_cov3d : $(SOLVER)_cov3d.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_cov3d.exe $(LDFLAGS) $(SOLVER)_cov3d.o ../lib_3dvar.a

$(SOLVER)_diags : $(SOLVER)_diags.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_diags.exe $(LDFLAGS) $(SOLVER)_diags.o ../lib_3dvar.a

$(SOLVER)_diags_read : $(SOLVER)_diags_read.o
	$(RANLIB) ../lib_3dvar.a
	$(FC) -o $(SOLVER)_diags_read.exe $(LDFLAGS) $(SOLVER)_diags_read.o ../lib_3dvar.a

include ../../configure.3dvar

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)


