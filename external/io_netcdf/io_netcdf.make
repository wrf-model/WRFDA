# IO_NETCDF

IO_NETCDF_OBJS = wrf_io.o field_routines.o
IO_NETCDF_CODE    = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code 
IO_NETCDF_FFLAGS  =  $(FCFLAGS) -I$(NETCDF_PATH)/include
IO_NETCDF_LIBS    = -L$(NETCDF_PATH)/lib -lnetcdf
IO_NETCDF_M4FLAGS      = -Uinclude -Uindex -Ulen

libwrfio_nf.a:		$(IO_NETCDF_OBJS) $(IO_NETCDF_CODE)
			$(RM) libwrfio_nf.a
			$(AR) cr libwrfio_nf.a $(IO_NETCDF_OBJS)
			$(RANLIB) libwrfio_nf.a

wrf_io.o:               wrf_io.F90 $(IO_NETCDF_CODE)
			$(CPP) wrf_io.F90 | $(M4) $(IO_NETCDF_M4FLAGS) - > wrf_io.f
			$(FC) $(IO_NETCDF_FFLAGS) -c wrf_io.f

#diffwrf:                diffwrf.F90
#			$(CPP) diffwrf.F90 > diffwrf.f
#			$(FC) -c $(IO_NETCDF_FFLAGS) diffwrf.f
#			@if [ -f module_wrf_error.o ] ; then \
#			echo "diffwrf is being built now. "; \
#			  $(FC) $(IO_NETCDF_FFLAGS) $(LDFLAGS) -o diffwrf diffwrf.o $(IO_NETCDF_OBJS) $(IO_NETCDF_LIBS) module_wrf_error.o ;\
#                        else \
#			  echo "***************************************************************************** " ; \
#			  echo "*** Rerun compile to make diffwrf in external/io_netcdf directory         *** " ; \
#			  echo "***************************************************************************** " ; \
#                        fi

field_routines.o:	field_routines.F90
			$(CPP) field_routines.F90 > field_routines.f
			$(FC) $(IO_NETCDF_FFLAGS) -c field_routines.f
