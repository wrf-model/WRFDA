!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_wrf_top

#if defined(DM_PARALLEL) && defined(VAR4D) 
   USE module_timing
#endif

!<DESCRIPTION>
! Main program of WRF model.  Responsible for starting up the model, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=integrate.html>integrate</a> routine to advance the domain
! to the ending time of the simulation. After the integration is completed, the model
! is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

#if defined(DM_PARALLEL) && defined(VAR4D) 

   LOGICAL, EXTERNAL :: await_next
   INTEGER           :: comm

! disable quilting for WRFVAR which splits the communicator its own way
   CALL disable_quilting

   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

! Initialize utilities (time manager, etc.)
   CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN )

   CALL init_modules(2)   !

   DO WHILE ( await_next() )

   CALL system("echo -n model_start ; date")

#endif

   ! Initialize WRF model.  
   CALL wrf_init

   ! WRF model time-stepping.  Calls integrate().  
   CALL wrf_run


   ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
#if defined(DM_PARALLEL) && defined(VAR4D) 
   CALL wrf_finalize(.true.)

   IF ( wrf_dm_on_monitor() ) THEN
     CALL system("touch wrfnl_done")
   ENDIF
   CALL system("echo -n model_end ; date")
   ENDDO

   CALL wrf_get_dm_communicator( comm )
   CALL MPI_BARRIER(comm,ierr)

   ! Finalize time manager
   CALL WRFU_Finalize
   CALL wrf_shutdown

#else
   CALL wrf_finalize
#endif

END PROGRAM wrf

#if defined(DM_PARALLEL) && defined(VAR4D) 

LOGICAL FUNCTION await_next()
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER result, myproc

301 CONTINUE
    result = 0

    IF ( wrf_dm_on_monitor() ) THEN
      OPEN( 99, file="wrfnl_go_ahead",status="old",form="formatted",ERR=303 )
      CLOSE(99)
      CALL system("rm wrfnl_go_ahead")
      result = 1
303   CONTINUE
      OPEN( 99, file="wrfnl_stop_now",status="old",form="formatted",ERR=304 )
      CLOSE(99)
      CALL system("rm wrfnl_stop_now")
      result = -1
304   CONTINUE
    ENDIF
    CALL wrf_dm_bcast_integer( result, 1 )
    IF ( result .EQ. 0 ) THEN
      CALL system("sleep 1")
      GOTO 301
    ELSE IF ( result .GT. 0 ) THEN
      await_next = .TRUE.
      call system("touch wrfnl_stop_now")
    ELSE
      await_next = .FALSE.
    ENDIF

END FUNCTION await_next
#endif



