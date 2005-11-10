!
SUBROUTINE da_thinning_rad ( it, xb, xp, ob, iv )
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

!  1.0 QC for HIRS
!------------------------------
   if ( hirs ) then
!  infrared cloud check


!  2.0 QC for AMSU-A
!------------------------------
   else if ( amsua ) then

!  microwave precipitation check

!  surface type check (land/sea/sea-ice/snow etc.)

!  high Topography Check

!  latitude band check

!  3.0 QC for AMSU-B
!------------------------------
   else if ( amsub ) then


   else
     write(*,*) 'Unrecognized Instrument'
     stop ' in da_qc_rad'
   end do

   return

END SUBROUTINE da_thinning_rad
