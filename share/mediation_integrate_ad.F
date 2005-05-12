!
!WRF:MEDIATION_LAYER:IO
!

SUBROUTINE med_filter_in ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: inname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   TYPE(ESMF_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( config_flags%write_input ) THEN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

     CALL ESMF_ClockGet( grid%domain_clock, CurrTime=CurrTime, rc=ierr )
     CALL wrf_timetoa ( CurrTime, timestr )
     CALL construct_filename2a ( inname , config_flags%input_outname , grid%id , 2 , timestr )

     WRITE ( message , '("med_filter_in 1: opening ",A," for reading. ",I3)') TRIM ( outname ), ierr
     CALL wrf_debug( 1, message )

     CALL open_r_dataset ( fid, TRIM(inname), grid ,  &
                           config_flags , input_model_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal( message )
     ENDIF

   CALL input_model_input ( fid, grid , config_flags , ierr )
   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Reading filter input for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   ENDIF

   RETURN
END SUBROUTINE med_filter_in

SUBROUTINE med_wrfvar_solve_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
  ! Model layer
   USE module_configure
   USE WRF_ESMF_MOD

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                                    :: rc

!  IF( ESMF_AlarmIsRinging( grid%alarms( INPUTBASE_ALARM ), rc=rc ) ) THEN
!    CALL med_filter_in  ( grid , config_flags )
!    CALL ESMF_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
!  ENDIF

   IF( ESMF_AlarmIsRinging( grid%alarms( INPUTFORCING_ALARM ), rc=rc ) ) THEN
     CALL med_filter_in  ( grid , config_flags )
     CALL ESMF_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

! - Look for boundary data after writing out history and restart files
!  CALL med_latbound_in ( grid , config_flags )

   RETURN
END SUBROUTINE med_varvar_solve_io

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
