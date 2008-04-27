#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =       module_trace.o
                
OBJS    = 	memory_info.o

include ../configure.wrf

LIBTARGET    =  tracing
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)
		$(RANLIB) ../main/libwrflib.a

clean :
	$(RM) $(MODULES) $(OBJS) *.mod

module_trace.o : da_tracing/module_trace.f90
	$(RM) $@
	$(CPP) -Ida_tracing -I../inc $(CPPFLAGS) da_tracing/module_trace.f90 > module_trace.f
	$(FC) -c $(FCFLAGS) $(MODULE_DIRS) module_trace.f

memory_info.o : da_tracing/memory_info.c
	$(RM) $@
	$(CC) -c $(CCFLAGS) -DNOUNDERSCORE -DMACOS da_tracing/memory_info.c

# DO NOT DELETE
