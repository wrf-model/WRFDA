# CONVERTOR

CONVERTOR_OBJS	=	module_kma2netcdf_interface.o \
		module_netcdf2kma_interface.o

CONVERTOR_MODULES =	module_kma_wave2grid.o \
		module_wave2grid_kma.o \
                da_wrfvar_io.o \
                da_tracing.o \
                da_memory.o \
                da_par_util1.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

