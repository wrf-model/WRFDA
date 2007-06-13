# IO_PHDF5

IO_PHDF5_OBJS    = wrf-phdf5attr.o wrf-phdf5support.o wrf-phdf5.o

libwrfio_phdf5.a:		$(IO_PHDF5_OBJS)
			$(RM) libwrfio_nf.a
			$(AR) libwrfio_nf.a $(IO_PHDF5_OBJS)
			$(RANLIB) libwrfio_nf.a
