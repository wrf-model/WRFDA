!GEN_BE_STATS:DRIVER_LAYER:MAIN
!
PROGRAM da_gen_be_stats

   USE module_machine
   USE module_domain
   USE module_driver_constants
   USE module_configure

   USE module_timing
   USE module_wrf_error




   USE module_wrf_3dvar_io
   USE module_gen_be_stats_interface

   IMPLICIT NONE

   REAL    :: time

   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level








   CHARACTER (LEN=80)      :: message

!--Define the name of this program (program_name defined in module_domain)

   program_name = "GEN_BE_STATS"

!--Get the NAMELIST data for input.

   CALL init_modules

   CALL start_timing
   CALL initial_config


   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

!--allocated and configure the mother domain

   NULLIFY( null_domain )

   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )

   if(config_flags%real_data_init_type == 1) then
     CALL init_wrfio





!--input background from wrf model.
     CALL med_initialdata_input_3dvar( head_grid , config_flags )
   endif

   call med_add_config_info_to_grid ( head_grid )

!--do gen_be_stats

   CALL wrf_debug ( 100 , 'gen_be_stats: calling gen_be_stats_interface ' )
   CALL da_gen_be_stats_interface ( head_grid, config_flags )
   CALL wrf_debug ( 100 , 'gen_be_stats: back from gen_be_stats_interface ' )

!--output gen_be_stats 
   if(config_flags%real_data_init_type == 1) then
!mslee.this should be binary file CALL med_initialdata_output_3dvar( head_grid , config_flags )

     CALL med_shutdown_io ( head_grid , config_flags )
   endif







   message = 'Time elapsed in gen_be_stats'
   CALL end_timing ( TRIM(message) )


   CALL wrf_shutdown

   STOP "Stopped normally in gen_be_stats."

END PROGRAM da_gen_be_stats

