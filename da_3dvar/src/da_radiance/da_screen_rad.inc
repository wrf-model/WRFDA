!
SUBROUTINE da_screen_rad ( it, xb, xp, ob, iv )
!------------------------------------------------------------------------------
!  PURPOSE: perform quality control for radiance data.
!
!  METHOD:  d = y - H(x)
!       1. interpolate xb to obs location
!       2. call foreward RTM to get simulated bright temperature
!       3. obs BT - simulated BT
!
!  HISTORY: 2005/10/17 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),     INTENT(IN)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.


!   print *, ' not implemented yet'

   do inst = 1, iv%num_inst                 ! loop for sensor
     if ( iv%instid(inst)%num_rad < 1 ) cycle
     num_levs  = iv%instid(inst)%nlevels
     nchanl    = iv%instid(inst)%nchan

     amsua = trim( inst_name(rtminit_sensor(inst))) == 'amsua'
     amsub = trim( inst_name(rtminit_sensor(inst))) == 'amsub'
     hirs  = trim( inst_name(rtminit_sensor(inst))) == 'hirs'
     msu   = trim( inst_name(rtminit_sensor(inst))) == 'msu'

!  1.0 QC for HIRS
!------------------------------
   if ( hirs ) then
!  infrared cloud check

      call da_screen_hirs()

!  2.0 QC for AMSU-A
!------------------------------
   else if ( amsua ) then

      call da_screen_amsua()

!  microwave precipitation check

!  surface type check (land/sea/sea-ice/snow etc.)

!  high Topography Check

!  latitude band check

!  3.0 QC for AMSU-B
!------------------------------
   else if ( amsub ) then

      call da_screen_amsub()

   else if ( msu ) then

      call da_screen_msu()

   else

     write(*,*) 'Unrecognized Instrument'
     stop ' in da_qc_rad'

   end if

   end do

   return

END SUBROUTINE da_screen_rad
