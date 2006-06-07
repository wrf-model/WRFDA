include ../configure.wrf

MODULES =       module_driver_constants.o  \
                module_domain.o            \
                module_domain1.o           \
                module_domain2.o           \
                module_domain3.o           \
                module_integrate.o         \
                module_integrate_ad.o      \
                module_integrate_tl.o      \
                module_integrate_tst.o     \
                module_timing.o            \
                module_configure.o         \
                module_tiles.o             \
                module_machine.o           \
                module_nesting.o           \
                module_wrf_error.o         \
                module_state_description.o \
                module_sm.o                \
                module_io.o                \
                module_dm.o                \
                module_quilt_outbuf_ops.o  \
                module_io_quilt.o

OBJS    =       wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                libmassv.o                 \
                collect_on_comm.o

#compile as a .o but do not link into the main library
SPECIAL =       module_internal_header_util.o pack_utils.o
   

framework :  $(MODULES) $(OBJS) $(SPECIAL)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)
		$(RANLIB) ../main/libwrflib.a

rsl_src :
	@ $(LN) $(RSL)/module_dm.F .

wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

clean:
	@ $(RM) *.o *.f

superclean : clean

module_state_description.f90 : ../Registry/$(REGISTRY)
		( cd .. ; tools/registry $(REGFLAGS) Registry/$(REGISTRY) )

md_calls.inc : md_calls.m4
	$(M4) md_calls.m4 > md_calls.inc


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

module_state_description.o : module_state_description.f90

module_configure.o: \
		module_state_description.o \
                module_wrf_error.o \
		module_driver_constants.o 

module_dm.o: module_machine.o module_state_description.o module_wrf_error.o \
		module_domain.o \
		module_driver_constants.o \
		module_timing.o \
		module_configure.o 

module_dm_stubs.F: module_domain.o

module_domain.o: module_domain1.o \
                 module_domain2.o \
                 module_domain3.o 

module_domain1.o: module_driver_constants.o \
		module_configure.o \
		module_machine.o  \
		module_state_description.o \
                module_wrf_error.o \
		$(ESMF_MOD_DEPENDENCE)

module_domain2.o: module_domain1.o \
                  module_domain3.o

module_domain3.o: module_domain1.o

module_driver_constants.o: \
		module_state_description.o \
                module_wrf_error.o

module_integrate.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_ad.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_tl.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_tst.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)


module_io.o : md_calls.inc \
		module_state_description.o \
		module_configure.o  \
		module_driver_constants.o 

module_io_quilt.o: module_state_description.o \
		module_internal_header_util.o \
		module_quilt_outbuf_ops.o 

module_machine.o: module_driver_constants.o

module_nesting.o: module_machine.o \
		module_driver_constants.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE) \
		module_domain.o 

module_quilt_outbuf_ops.o: module_state_description.o

module_tiles.o: module_domain.o \
		module_driver_constants.o \
		module_machine.o  \
		module_configure.o  \
                module_wrf_error.o
 
module_timing.o: \
		module_state_description.o \
                module_wrf_error.o

module_wrf_error.o: \
		wrf_shutdown.o \
		$(ESMF_MOD_DEPENDENCE)

# DO NOT DELETE
