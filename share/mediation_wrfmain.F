!WRF:MEDIATION_LAYER:
!

SUBROUTINE med_initialdata_input_ptr ( grid , config_flags )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE (domain) , POINTER :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTERFACE 
      SUBROUTINE med_initialdata_input ( grid , config_flags )
         USE module_domain
         USE module_configure
         TYPE (domain) :: grid
         TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
      END SUBROUTINE med_initialdata_input
   END INTERFACE
   CALL  med_initialdata_input ( grid , config_flags )


END SUBROUTINE med_initialdata_input_ptr

SUBROUTINE med_initialdata_input ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
   USE module_timing
  ! Model layer
   USE module_configure
   USE module_bc_time_utilities
   USE module_utility

   IMPLICIT NONE


  ! Interface 
   INTERFACE
     SUBROUTINE start_domain ( grid , allowed_to_read )  ! comes from module_start in appropriate dyn_ directory
       USE module_domain
       TYPE (domain) grid
       LOGICAL, INTENT(IN) :: allowed_to_read 
     END SUBROUTINE start_domain
   END INTERFACE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   INTEGER                :: fid , ierr , myproc
   CHARACTER (LEN=80)     :: inpname , rstname, timestr
   CHARACTER (LEN=80)     :: message
   LOGICAL                :: restart

   REAL    :: jrb_wind
   REAL    :: jrb_temp
   REAL    :: jrb_p
   REAL    :: jrb_ph
   REAL    :: jrb_mu
   REAL    :: jrb_moist


   CALL nl_get_restart( 1, restart )
   IF ( .NOT. restart ) THEN
     !  Initialize the mother domain.
     CALL start_timing
     grid%input_from_file = .true.
     IF ( grid%input_from_file ) THEN

        CALL       wrf_debug ( 1 , 'wrf main: calling open_r_dataset for wrfinput' )

! typically <date> will not be part of input_inname but allow for it
        CALL domain_clock_get( grid, current_timestr=timestr )
        CALL construct_filename2a ( inpname , config_flags%input_inname , grid%id , 2 , timestr )

        CALL open_r_dataset ( fid, TRIM(inpname) , grid , config_flags , "DATASET=INPUT", ierr )
        IF ( ierr .NE. 0 ) THEN
          WRITE( wrf_err_message , * ) 'program wrf: error opening ',TRIM(inpname),' for reading ierr=',ierr
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF
        IF      ( ( grid%id .EQ. 1 ) .OR. ( config_flags%fine_input_stream .EQ. 0 ) ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_model_input' )
           CALL input_model_input      ( fid ,  grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_model_input' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 1 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input1' )
           CALL input_aux_model_input1 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input1' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 2 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input2' )
           CALL input_aux_model_input2 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input2' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 3 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input3' )
           CALL input_aux_model_input3 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input3' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 4 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input4' )
           CALL input_aux_model_input4 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input4' )
        ELSE IF   ( config_flags%fine_input_stream .EQ. 5 ) THEN
           CALL wrf_debug              (   0 , 'med_initialdata_input: calling input_aux_model_input5' )
           CALL input_aux_model_input5 ( fid ,   grid , config_flags , ierr )
           CALL wrf_debug              ( 100 , 'med_initialdata_input: back from input_aux_model_input5' )
        END IF
        CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )
#ifdef MOVE_NESTS
        grid%nest_pos = grid%ht
        where ( grid%nest_pos .gt. 0 ) grid%nest_pos = grid%nest_pos + 500.  ! make a cliff
#endif
     ENDIF
     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
     CALL start_domain ( grid , .TRUE. )
   ELSE
     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( rstname , config_flags%rst_inname , grid%id , 2 , timestr )

     WRITE(message,*)'RESTART run: opening ',TRIM(rstname),' for reading'
     CALL wrf_message (  message )
     CALL open_r_dataset ( fid , TRIM(rstname) , grid , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL WRF_ERROR_FATAL ( message )
     ENDIF
     CALL input_restart ( fid,   grid , config_flags , ierr )
     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
     grid%imask_nostag = 1
     grid%imask_xstag = 1
     grid%imask_ystag = 1
     grid%imask_xystag = 1
     CALL start_domain ( grid , .TRUE. )
   ENDIF

   CALL nl_get_jrb_wind( 1, jrb_wind )
   CALL nl_get_jrb_temp( 1, jrb_temp )
   CALL nl_get_jrb_p( 1, jrb_p )
   CALL nl_get_jrb_ph( 1, jrb_ph )
   CALL nl_get_jrb_mu( 1, jrb_mu )
   CALL nl_get_jrb_moist( 1, jrb_moist )

   IF (jrb_wind > -9999.9) THEN
      call wrf_message("Fiddling with wind in med_initialdata_input")
      grid % em_u_1  = jrb_wind
      grid % em_u_2  = jrb_wind
      grid % em_v_1  = jrb_wind
      grid % em_v_2  = jrb_wind
      grid % em_w_1  = jrb_wind
      grid % em_w_2  = jrb_wind
   end if

   if (jrb_temp > -9999.9) then
      call wrf_message("Fiddling with temp in med_initialdata_input")
      grid % em_t_1  = jrb_temp
      grid % em_t_2  = jrb_temp
   end if

   if (jrb_p > -9999.9) then
      call wrf_message("Fiddling with p in med_initialdata_input")
      grid % em_p    = jrb_p
   end if

   if (jrb_ph > -9999.9) then
      call wrf_message("Fiddling with ph in med_initialdata_input")
      grid % em_ph_1 = jrb_ph
      grid % em_ph_2 = jrb_ph
      grid % em_phb  = jrb_ph
   end if

   if (jrb_mu > -9999.9) then
      call wrf_message("Fiddling with mu in med_initialdata_input")
      grid % em_mu0  = jrb_mu
      grid % em_mub  = jrb_mu
      grid % em_mu_1 = jrb_mu
      grid % em_mu_2 = jrb_mu
   end if

   if (jrb_moist > -9999.9) then
      call wrf_message("Fiddling with moist in med_initialdata_input")
      grid % moist = jrb_moist
   END IF

   RETURN
END SUBROUTINE med_initialdata_input

SUBROUTINE med_shutdown_io ( grid , config_flags )
  ! Driver layer
   USE module_domain
   USE module_io_domain
  ! Model layer
   USE module_configure

   IMPLICIT NONE

  ! Arguments
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  ! Local
   CHARACTER (LEN=80)      :: message
   INTEGER                 :: ierr

   IF ( grid%oid > 0 ) CALL close_dataset ( grid%oid , config_flags , "DATASET=HISTORY" )
   IF ( grid%auxhist1_oid > 0 ) CALL close_dataset ( grid%auxhist1_oid , config_flags , "DATASET=AUXHIST1" )
   IF ( grid%auxhist2_oid > 0 ) CALL close_dataset ( grid%auxhist2_oid , config_flags , "DATASET=AUXHIST2" )
   IF ( grid%auxhist3_oid > 0 ) CALL close_dataset ( grid%auxhist3_oid , config_flags , "DATASET=AUXHIST3" )
   IF ( grid%auxhist4_oid > 0 ) CALL close_dataset ( grid%auxhist4_oid , config_flags , "DATASET=AUXHIST4" )
   IF ( grid%auxhist5_oid > 0 ) CALL close_dataset ( grid%auxhist5_oid , config_flags , "DATASET=AUXHIST5" )

   CALL wrf_ioexit( ierr )    ! shut down the quilt I/O

   RETURN

END SUBROUTINE med_shutdown_io

SUBROUTINE med_add_config_info_to_grid ( grid )

   USE module_domain
   USE module_configure
 
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid

#define SOURCE_RECORD model_config_rec %
#define SOURCE_REC_DEX (grid%id)
#define DEST_RECORD   grid %
#include "config_assigns.inc"

   RETURN

END SUBROUTINE med_add_config_info_to_grid

