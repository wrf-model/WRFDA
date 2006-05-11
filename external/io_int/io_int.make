# IO_INT

# pack_utils now from frame, removed from here

IO_INT_OBJS = io_int.o module_internal_header_util.o
IO_INT_M4   = -Uinclude -Uindex -Ulen

libwrfio_int.a:		$(OBJS)
			$(RM) libwrfio_int.a
			$(AR) cr libwrfio_int.a $(IO_INT_OBJS)
			$(RANLIB) libwrfio_int.a

io_int.o:       io_int.F90 module_internal_header_util.o
		$(CPP) $(FPPFLAGS) io_int.F90 | $(M4) $(IO_INT_M4) - > io_int.f
		$(FC) $(FCFLAGS) -c io_int.f

