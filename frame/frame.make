# FRAME

FRAME_MODULES =  module_internal_header_util.o \
                 module_driver_constants.o  \
                module_domain.o            \
                module_integrate.o         \
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

FRAME_OBJS    = wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                libmassv.o                 \
                collect_on_comm.o          \
                pack_utils.o

#compile as a .o but do not link into the main library
#FRAME_SPECIAL_OBJS =       module_internal_header_util.o pack_utils.o
   
wrf_num_bytes_between.o :
	$(CC) -c $(CCFLAGS) wrf_num_bytes_between.c

module_state_description.f90 : registry ../Registry/$(REGISTRY)
	./registry $(REGFLAGS) ../Registry/$(REGISTRY)
	$(LN) inc/*.inc .
	$(LN) frame/module_state_description.f90 .

md_calls.inc : md_calls.m4
	$(M4) md_calls.m4 > md_calls.inc


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

# DO NOT DELETE
