#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES = module_init_utilities.o
 
LIBTARGET    =  dyn_share
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES)
		$(AR) ../main/libwrflib.a $(MODULES)

include ../configure.wrf

clean:
	@ echo 'use the clean script'


