#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES = 

OBJS    =

LIBPATHS = 

include ../configure.wrf

da_3dvar : da_3dvar.o
	$(RANLIB) libwrflib.a
	$(LD) -o da_3dvar.exe $(LDFLAGS) da_3dvar.o libwrflib.a $(LIB)

$(SOLVER) : $(SOLVER).o
	$(RANLIB) libwrflib.a
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(LIB)

$(SOLVER)_wrf : wrf.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o libwrflib.a $(LIB)

$(SOLVER)_ideal : module_initialize ideal.o
	$(RANLIB) libwrflib.a
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

$(SOLVER)_real : module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(RANLIB) libwrflib.a
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

$(SOLVER) :     $(SOLVER)_stage0 \
                $(SOLVER)_stage1 \
                $(SOLVER)_stage2 \
                $(SOLVER)_stage3 \
                $(SOLVER)_stage4 \
                $(SOLVER)_stage4_scale \
                $(SOLVER)_cov2d \
                $(SOLVER)_cov3d \
                $(SOLVER)_diags \
                $(SOLVER)_diags_read

$(SOLVER)_stage0 : $(SOLVER)_stage0.o
        $(RANLIB) ../lib_3dvar.a
        $(FC) -o $(SOLVER)_stage0.exe $(LDFLAGS) $(SOLVER)_stage0.o \
        ../lib_3dvar.a -L../../external/io_netcdf -lwrfio_nf -L$(NETCDFPATH)/lib -lnetcdf \
        -L../../external/io_int -lwrfio_int \
        ../../frame/internal_header_util.o ../../frame/pack_utils.o

$(SOLVER)_stage4_scale : $(SOLVER)_stage4_scale.o
        $(RANLIB) ../lib_3dvar.a
        $(FC) -o $(SOLVER)_stage4_scale.exe $(LDFLAGS) $(SOLVER)_stage4_scale.o ../lib_3dvar.a

$(SOLVER)_diags_read : $(SOLVER)_diags_read.o
        $(RANLIB) ../lib_3dvar.a
        $(FC) -o $(SOLVER)_diags_read.exe $(LDFLAGS) $(SOLVER)_diags_read.o ../lib_3dvar.a

convert_bioemiss : convert_bioemiss.o
	$(RANLIB) libwrflib.a
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o libwrflib.a $(LIB)

convert_emiss : convert_emiss.o
	$(RANLIB) libwrflib.a
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrflib.a $(LIB)

convert_nmm : convert_nmm.o
	$(RANLIB) libwrflib.a
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o libwrflib.a $(LIB)

module_initialize :
	( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

#############################################################################################################################

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ideal.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../share/module_io_domain.o \
	../dyn_$(SOLVER)/$(CASE_MODULE) \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ndown_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_real.o \
	../dyn_em/module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# this already built above :../dyn_em/module_initialize.real.o \
real_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../dyn_em/module_si_io_em.o \
	../dyn_em/module_big_step_utilities_em.o \
	../share/module_io_domain.o \
	../share/module_date_time.o \
	../share/module_optional_si_input.o \
	../share/module_bc_time_utilities.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)
#	../chem/module_input_chem_data.o \
#	../chem/module_input_chem_bioemiss.o \

wrf.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_integrate.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_wrf_error.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

da_3dvar.o: \
	../share/module_wrf_esmf_super.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# DO NOT DELETE
