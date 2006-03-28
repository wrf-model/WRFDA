#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES_IO		=	module_wrf_3dvar_io.o

MODULES_INTERFACE	=	module_wrf_3dvar_interface.o	\
	   			module_wrfvar_top.o
OBJS    = 

wrfvar_interface :	$(MODULES_INTERFACE) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES_INTERFACE) $(OBJS)

wrfvar_io :	$(MODULES_IO)
		$(AR) ../main/libwrflib.a $(MODULES_IO)

include ../configure.wrf

clean:
	$(RM) *.f *.o *.mod
#	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

