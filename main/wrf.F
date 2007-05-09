!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_wrf_esmf_super

!<DESCRIPTION>
! Main program of WRF model.  Responsible for starting up the model, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=integrate.html>integrate</a> routine to advance the domain
! to the ending time of the simulation. After the integration is completed, the model
! is properly shut down.
!
!</DESCRIPTION>

   use da_tracing, only : da_trace_init, da_trace_entry, da_trace_exit, &
      da_trace_report
      
   IMPLICIT NONE

   TYPE(ESMF_GridComp) :: gcomp
   TYPE(ESMF_State)    :: importState, exportState
   TYPE(ESMF_Clock)    :: clock
   TYPE(ESMF_VM)       :: vm   
   INTEGER :: rc

   LOGICAL, EXTERNAL :: await_next
!   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
   INTEGER comm

write(0,*)'********* ********* *********'

   ! This call includes everything that must be done before ESMF_Initialize() 
   ! is called.  

#ifdef DM_PARALLEL
! disable quilting for WRFVAR which splits the communicator its own way
   CALL disable_quilting
#endif

   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )
#ifdef DM_PARALLEL
   CALL init_modules(2)   ! 
#endif

#ifdef DM_PARALLEL
#ifndef STAND_ALONE
   DO WHILE ( await_next() )
     call system("echo -n model_start ; date")
#endif
#endif
     CALL wrf_init( gcomp, importState, exportState, clock, rc )
   call da_trace_init
   call da_trace_entry("wrf_init")
     CALL wrf_run( gcomp, importState, exportState, clock, rc )
   call da_trace_exit("wrf_init")
   call da_trace_report
     CALL wrf_finalize( gcomp, importState, exportState, clock, rc )
#ifdef DM_PARALLEL
#ifndef STAND_ALONE
     IF ( wrf_dm_on_monitor() ) THEN
       CALL system("touch wrf_done")
     ENDIF
     call system("echo -n model_end ; date")
   ENDDO
#endif
#endif

#ifdef DM_PARALLEL
   CALL wrf_get_dm_communicator( comm )
   CALL MPI_BARRIER(comm,ierr)
#endif
   CALL wrf_shutdown 

   CALL ESMF_Finalize( rc=rc )

END PROGRAM wrf

LOGICAL FUNCTION await_next()
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER result, myproc

301 CONTINUE
    result = 0

    IF ( wrf_dm_on_monitor() ) THEN
      OPEN( 99, file="wrf_go_ahead",status="old",form="formatted",ERR=303 )
      CLOSE(99)
      CALL system("rm wrf_go_ahead")
      result = 1
303   CONTINUE
      OPEN( 99, file="wrf_stop_now",status="old",form="formatted",ERR=304 )
      CLOSE(99)
      CALL system("rm wrf_stop_now")
      result = -1
304   CONTINUE
    ENDIF
    CALL wrf_dm_bcast_integer( result, 1 )
    IF ( result .EQ. 0 ) THEN
      CALL system("sleep 1")
      GOTO 301
    ELSE IF ( result .GT. 0 ) THEN
      await_next = .TRUE.
    ELSE
      await_next = .FALSE.
    ENDIF

END FUNCTION await_next 

