# CONVERTOR

CONVERTOR_OBJS	=	module_kma2netcdf_interface.o \
		module_netcdf2kma_interface.o

CONVERTOR_MODULES =	module_kma_wave2grid.o \
		module_wave2grid_kma.o

PREGSM : PREGSM.o  $(CONVERTOR_MODULES)
	$(LD) -L. $(LDFLAGS) -o PREGSM.exe PREGSM.o $(CONVERTOR_MODULES)

PREGSM.o : PREGSM.F
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) PREGSM.F > PREGSM.f
	$(FFC) -c $(FIXEDFLAGS) PREGSM.f

module_kma_wave2grid.o : module_kma_wave2grid.f90
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) module_kma_wave2grid.f90 > module_kma_wave2grid.f
	$(FFC) -c $(FIXEDFLAGS) module_kma_wave2grid.f

module_wave2grid_kma.o : module_wave2grid_kma.f90
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) module_wave2grid_kma.f90 > module_wave2grid_kma.f
	$(FFC) -c $(FIXEDFLAGS) module_wave2grid_kma.f
