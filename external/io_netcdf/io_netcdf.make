# IO_NETCDF

IO_NETCDF_OBJS    = wrf_io.o field_routines.o
IO_NETCDF_CODE    = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code 
IO_NETCDF_FCFLAGS =  -I$(NETCDF_PATH)/include
IO_NETCDF_LIBS    = -L$(NETCDF_PATH)/lib -lnetcdf
IO_NETCDF_M4FLAGS = -Uinclude -Uindex -Ulen

libwrfio_nf.a:		$(IO_NETCDF_OBJS) $(IO_NETCDF_CODE)
			$(RM) libwrfio_nf.a
			$(AR) libwrfio_nf.a $(IO_NETCDF_OBJS)
			$(RANLIB) libwrfio_nf.a

wrf_io.o:               wrf_io.F90 $(IO_NETCDF_CODE)
			$(CPP) $(FPPFLAGS) wrf_io.F90 | $(M4) $(IO_NETCDF_M4FLAGS) - > wrf_io.f
			$(FC) -c $(FCFLAGS) $(IO_NETCDF_FCFLAGS) wrf_io.f

diffwrf_netcdf.o:       diffwrf_netcdf.F90
			$(CPP) $(FPPFLAGS) diffwrf_netcdf.F90 > diffwrf_netcdf.f
			$(FC) -c $(FCFLAGS) $(IO_NETCDF_FCFLAGS) diffwrf_netcdf.f

field_routines.o:	field_routines.F90 \
                        wrf_io.o
			$(CPP) $(FPPFLAGS) field_routines.F90 > field_routines.f
			$(FC) -c $(FCFLAGS) $(IO_NETCDF_FCFLAGS) field_routines.f
