!NETCDF2KMA:DRIVER_LAYER:MAIN
!

PROGRAM netcdf2kma

   USE module_machine
   USE module_domain
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error
#ifdef DM_PARALLEL
   USE module_dm
#endif

   USE module_wrf_3dvar_io
   USE module_netcdf2kma_interface
   USE module_wave2grid_kma

   IMPLICIT NONE

   REAL    :: time

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4*8192
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER (LEN=80)      :: message

!--Define the name of this program (program_name defined in module_domain)

   program_name = "NETCDF2KMA"

!--Get the NAMELIST data for input.

!shc-wei start
!  CALL init_modules
   CALL init_modules(1)
!shc-wei end

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
#else
   CALL start_timing
   CALL initial_config
#endif

!shc-wei start
!  CALL get_debug_level ( debug_level )
   CALL nl_get_debug_level ( 1,debug_level )
!shc-wei end
   CALL set_wrf_debug_level ( debug_level )

!--allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
!                                    local_time = 0 ,                  &     !shc-wei
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )

   if(config_flags%real_data_init_type == 1 .or. &
      config_flags%real_data_init_type == 3) then
     CALL init_wrfio

#ifdef DM_PARALLEL
     CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
     CALL wrf_dm_bcast_bytes( configbuf, nbytes )
     CALL set_config_as_buffer( configbuf, configbuflen )
#endif

   endif
   call med_add_config_info_to_grid ( head_grid )


!-- read input in netcdf format
     CALL med_initialdata_input_3dvar( head_grid , config_flags )  

!--write in grid-array in kma-ascii format

   CALL wrf_debug ( 100 , 'da_3dvar: calling netcdf2kma_interface ' )
   CALL netcdf2kma_interface ( head_grid, config_flags )
   CALL wrf_debug ( 100 , 'da_3dvar: back from netcdf2kma_interface' )

   CALL med_shutdown_io ( head_grid , config_flags )

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
      message = 'Time elapsed in netcdf2kma'
      CALL end_timing ( TRIM(message) )
   END IF
#else
   message = 'Time elapsed in netcdf2kma'
   CALL end_timing ( TRIM(message) )
#endif

   CALL wrf_shutdown

   STOP "Stopped normally. in netcdf2kma"

END PROGRAM netcdf2kma

