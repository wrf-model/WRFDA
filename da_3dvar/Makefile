#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =  module_wrf_3dvar_io.o \
	   module_wrf_3dvar_interface.o	\
	   module_wrfvar_esmf_super.o
OBJS    = 

da_3dvar_io :	$(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)

include ../configure.wrf

module_wrfvar_esmf_super.o:	\
		module_wrf_3dvar_io.o		\
		module_wrf_3dvar_interface.o
clean:
	$(RM) *.f *.o *.mod
#	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

