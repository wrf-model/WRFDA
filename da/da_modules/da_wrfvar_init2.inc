subroutine da_wrfvar_init2

   !<DESCRIPTION>
   !     WRF initialization routine, part 2
   !</DESCRIPTION>

   IMPLICIT NONE

   integer :: ierr

#if DM_PARALLEL
   include "mpif.h"
#endif

   IF (trace_use) call da_trace_entry("da_wrfvar_init2")

   !<DESCRIPTION>
   ! Among the configuration variables read from the namelist is
   ! debug_level. This is retrieved using nl_get_debug_level (Registry
   ! generated and defined in frame/module_configure.F).  The value is then
   ! used to set the debug-print information level for use by <a
   ! href=wrf_debug.html>wrf_debug</a> throughout the code. Debug_level
   ! of zero (the default) causes no information to be printed when the
   ! model runs. The higher the number (up to 1000) the more information is
   ! printed.
   ! 
   !</DESCRIPTION>

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   NULLIFY( null_domain )

   !<DESCRIPTION>
   ! RSL is required for WRF nesting options.
   ! The non-MPI build that allows nesting is only supported on machines
   ! with the -DSTUBMPI option.  Check to see if the WRF model is being asked 
   ! for a for a multi-domain run (max_dom > 1, from the namelist).  If so,
   ! then we check to make sure that we are under the parallel
   ! run option or we are on an acceptable machine.
   !</DESCRIPTION>

   IF ( max_dom > 1 ) THEN
#if (! defined(DM_PARALLEL) && ! defined(STUBMPI))
      call da_error(__FILE__,__LINE__, &
         (/'nesting requires either an MPI build or use of the -DSTUBMPI option'/)) 
#endif
   END IF

   !<DESCRIPTION>
   ! The top-most domain in the simulation is then allocated and configured
   ! by calling <a href=alloc_and_configure_domain.html>alloc_and_configure_domain</a>.
   ! Here, in the case of this root domain, the routine is passed the
   ! globally accessible pointer to TYPE(domain), head_grid, defined in
   ! frame/module_domain.F.  The parent is null and the child index is given
   ! as negative, signifying none.  Afterwards, because the call to
   ! alloc_and_configure_domain may modify the model configuration data
   ! stored in model_config_rec, the configuration information is again
   ! repacked into a buffer, broadcast, and unpacked on each task (for
   ! DM_PARALLEL compiles). The call to <a
   ! href=setup_timekeeping.html>setup_timekeeping</a> for head_grid relies
   ! on this configuration information, and it must occur after the second
   ! broadcast of the configuration information.
   ! 
   !</DESCRIPTION>

   call da_messages ((/program_name//" "//subversion_version/))
   call da_trace("da_wrfvar_init2",message="calling alloc_and_configure_domain")
   call alloc_and_configure_domain ( domain_id  = 1 ,                  &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   call da_trace("da_wrfvar_init2",message="calling model_to_grid_config_rec")
   call model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )

   call da_trace("da_wrfvar_init2",message="calling set_scalar_indices_from_config")
   call set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   call da_trace("da_wrfvar_init2",message="calling init_wrfio")
   call init_wrfio

#ifdef DM_PARALLEL
   call get_config_as_buffer( configbuf, configbuflen, nbytes )
   call wrf_dm_bcast_bytes( configbuf, nbytes )
   call set_config_as_buffer( configbuf, configbuflen )
#endif

   call Setup_Timekeeping (head_grid)

   !<DESCRIPTION>
   ! The head grid is initialized with read-in data through the call to <a
   ! href=med_initialdata_input.html>med_initialdata_input</a>, which is
   ! passed the pointer head_grid and a locally declared configuration data
   ! structure, config_flags, that is set by a call to <a
   ! href=model_to_grid_config_rec.html>model_to_grid_config_rec</a>.  It is
   ! also necessary that the indices into the 4d tracer arrays such as
   ! moisture be set with a call to <a
   ! href=set_scalar_indices_from_config.html>set_scalar_indices_from_config</a>
   ! prior to the call to initialize the domain.  Both of these calls are
   ! told which domain they are setting up for by passing in the integer id
   ! of the head domain as <tt>head_grid%id</tt>, which is 1 for the
   ! top-most domain.
   ! 
   ! In the case that write_restart_at_0h is set to true in the namelist,
   ! the model simply generates a restart file using the just read-in data
   ! and then shuts down. This is used for ensemble breeding, and is not
   ! typically enabled.
   ! 
   !</DESCRIPTION>

   ! call med_initialdata_input( head_grid , config_flags,'fg01' )

   IF ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) THEN
      call da_med_initialdata_input( head_grid , config_flags,'fg01')
   END IF

   ! JRB
   CALL da_warning(__FILE__,__LINE__,(/"Fix me"/))
   !  head_grid%start_subtime = head_grid%start_time
   !  head_grid%stop_subtime = head_grid%stop_time

  IF (trace_use) call da_trace_exit("da_wrfvar_init2")

end subroutine da_wrfvar_init2
