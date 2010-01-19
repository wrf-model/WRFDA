#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f



MODULES =       module_driver_constants.o  \
                module_domain.o            \
                module_dfi.o               \
                module_linked_lisk.o       \
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
		memory_info.o              \
                module_trace.o             \
                module_quilt_outbuf_ops.o  \
                module_io_quilt.o

OBJS    =       wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                libmassv.o                 \
                collect_on_comm.o          \
		fast_copy.o                \
		adStack.o                  \
		adBuffer.o                 \
                microclock.o               \
		add_forcing_to_ad.o        \
	        adBuffer.o                 \
	        adStack.o

#compile as a .o but do not link into the main library
SPECIAL =       module_internal_header_util.o pack_utils.o

include ../configure.wrf

LIBTARGET    =  framework
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(EXTRAMODULES) $(OBJS) $(SPECIAL)
		$(AR) ../main/libwrflib.a $(MODULES) $(EXTRAMODULES) $(OBJS)
		$(RANLIB) ../main/libwrflib.a


wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

adBuffer.o:     adBuffer.F
	        $(FC77) -c adBuffer.F
adStack.o:      adStack.c
	        $(CC) -c adStack.c

clean:
	@ echo 'use the clean script'

module_state_description.F : ../Registry/$(REGISTRY)
		( cd .. ; tools/registry $(ARCHFLAGS) $(WRFVAR) Registry/$(REGISTRY) )

md_calls.inc : md_calls.m4
	$(M4) md_calls.m4 > md_calls.inc


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

adStack.o:	adStack.c
		$(CC) $(CFLAGS) -c adStack.c

memory_info.o:	memory_info.c
		$(CC) $(CFLAGS) -c memory_info.c

adBuffer.o:	adBuffer.F
		$(RM) $@
		$(SED_FTN) $*.F > $*.b
		$(CPP) -I../inc $(CPPFLAGS) $*.b  > $*.f
		$(RM) $*.b
		$(RM) $@
		$(FC) -c $(FCFLAGS) $(FORMAT_FIXED) $(PROMOTION) $*.f

module_configure.o: \
		module_state_description.o \
                module_wrf_error.o \
		module_driver_constants.o 

module_dm.o: module_machine.o module_state_description.o module_wrf_error.o \
		module_domain.o \
		module_driver_constants.o \
		module_timing.o \
		module_configure.o 

module_dfi.o: module_wrf_error.o \
		module_domain.o \
		module_configure.o 

module_dm_stubs.F: module_domain.o

module_domain.o: module_driver_constants.o \
		module_configure.o \
		module_machine.o  \
		module_state_description.o \
                module_wrf_error.o \
		$(ESMF_MOD_DEPENDENCE)

module_driver_constants.o: \
		module_state_description.o \
                module_wrf_error.o

module_integrate.o: module_domain.o \
		module_timing.o \
		module_trace.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_ad.o: module_domain.o \
                module_linked_lisk.o \
		module_timing.o \
		module_trace.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_tl.o: module_domain.o \
                module_linked_lisk.o \
		module_timing.o \
		module_trace.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_integrate_tst.o: module_domain.o \
		module_timing.o \
		module_trace.o \
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

module_trace.o: \
		module_dm.o \
		module_driver_constants.o

module_wrf_error.o: \
		wrf_shutdown.o \
		$(ESMF_MOD_DEPENDENCE)

# DO NOT DELETE
