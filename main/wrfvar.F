!WRFVAR:DRIVER_LAYER:MAIN
!

PROGRAM wrfvar

   USE module_wrfvar_top
   use da_tracing

!<DESCRIPTION>
! Main program of WRFVAR.  Responsible for starting up, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=da_v3d_solve.html>da_v3d_solve</a> routine assimilation.
! After the assimilation is completed, the model is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

   call da_trace_init

!--Initialize WRFVAR
   CALL wrfvar_init

!--WRFVAR Calls integrate().  
   CALL wrfvar_run

!--WRFVAR clean-up.  
   CALL wrfvar_finalize

   call da_trace_report

   IF ( .NOT. PRESENT( no_shutdown ) ) THEN
     ! Finalize time manager
     CALL WRF_UTIL_Finalize
     CALL wrf_shutdown
   ENDIF

END PROGRAM wrfvar

