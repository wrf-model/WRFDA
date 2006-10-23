subroutine da_wrfvar_run( )

   !<DESCRIPTION>
   !     WRF run routine.
   !</DESCRIPTION>

   !<DESCRIPTION>
   ! Once the top-level domain has been allocated, configured, and
   ! initialized, the model time integration is ready to proceed.  The start
   ! and stop times for the domain are set to the start and stop time of the
   ! model run, and then <a href=integrate.html>integrate</a> is called to
   ! advance the domain forward through that specified time interval.  On
   ! return, the simulation is completed.  A Mediation Layer-provided
   ! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
   ! to allow the the model to do any I/O specific cleanup and shutdown, and
   ! then the WRF Driver Layer routine <a
   ! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
   ! directed to shut down here) is called to properly end the run,
   ! including shutting down the communications (for example, most comm
   ! layers would call MPI_FINALIZE at this point if they are using MPI).
   ! 
   !</DESCRIPTION>

   !  The forecast integration for the most coarse grid is now started.  The
   !  integration is from the first step (1) to the last step of the simulation.

   !--do wrfvar
   IF (trace_use) call da_trace_entry("da_wrfvar_run")

   call da_wrfvar_interface ( head_grid, config_flags )

   IF (trace_use) call da_trace_exit("da_wrfvar_run")

end subroutine da_wrfvar_run
