# IO_INT

# pack_utils now from frame, removed from here

IO_INT_OBJS = io_int.o module_internal_header_util.o
IO_INT_M4   = -Uinclude -Uindex -Ulen

libwrfio_int.a:		$(OBJS)
			$(RM) libwrfio_int.a
			$(AR) cr libwrfio_int.a $(IO_INT_OBJS)
			$(RANLIB) libwrfio_int.a

io_int.o:       io_int.F90 module_internal_header_util.o
		$(CPP) $(CPPFLAGS) io_int.F90 | $(M4) $(IO_INT_M4) - > io_int.f
		$(FC) $(FCFLAGS) -c io_int.f

#diffwrf:                diffwrf.F pack_utils.o module_machine.o module_wrf_error.o libwrfio_int.a
#			if [ -f pack_utils.o ] ; then \
#			  mv diffwrf.F diffwrf.F90    ; \
#			  $(CPP) $(CPPFLAGS) diffwrf.F90 > diffwrf.f  ; \
#			  $(SFC) -c $(FCFLAGS) diffwrf.f    ; \
#			  mv diffwrf.F90 diffwrf.F ; \
#			  $(SFC) $(FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(OBJSL) \
#                            pack_utils.o module_internal_header_util.o \
#                            module_driver_constants.o \
#			    module_machine.o module_wrf_error.o ; \
#                        fi

#pack_utils.o internal_header_util.o module_machine.o module_wrf_error.o :
#			@echo "Diffwrf will be built later on in this compile. No need to rerun compile. "

