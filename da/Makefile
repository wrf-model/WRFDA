#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =       da_control.o	\
		da_tracing.o
                
OBJS    =       da_par_util1.o	\
                da_memory.o

include ../configure.wrf

LIBTARGET    =  tracing
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)
		$(RANLIB) ../main/libwrflib.a

clean :
		@ echo 'Use the clean script'

da_par_util1.o : da_par_util/da_par_util1.f90
	$(RM) $@
	$(CPP) -Ida_par_util -I../inc $(CPPFLAGS) da_par_util/da_par_util1.f90 > da_par_util1.f
	$(FC) -c $(FCFLAGS) -I../frame $(MODULE_DIRS) da_par_util1.f

da_tracing.o : da_tracing/da_tracing.f90	\
	da_par_util1.o
	$(RM) $@
	$(CPP) -Ida_tracing -I../inc $(CPPFLAGS) da_tracing/da_tracing.f90 > da_tracing.f
	$(FC) -c $(FCFLAGS) -I../frame $(MODULE_DIRS) da_tracing.f

da_control.o : da_control/da_control.f90
	$(RM) $@
	$(CPP) -Ida_control -I../inc $(CPPFLAGS) da_control/da_control.f90 > da_control.f
	$(FC) -c $(FCFLAGS) -I../frame $(MODULE_DIRS) da_control.f

da_memory.o : da_tracing/da_memory.c
	$(RM) $@
	$(CC) -c $(CCFLAGS) -DNOUNDERSCORE -DMACOS da_tracing/da_memory.c

# DO NOT DELETE
