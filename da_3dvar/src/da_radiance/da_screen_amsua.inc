!
SUBROUTINE da_amsua_rad ( it, xb, xp, ob, iv )
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

!  1.0  reject all channels over mixture surface type
!***********************************************************************
   lmix = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
   if (lmix) iv%instid(inst)%rad(n)%tb(:)%qc    = -1

!  2.0 check precipitation 
!***********************************************************************

!   2.1 Estimate Cloud Liquid Water (CLW) in mm over sea 
!       ( Grody etal. 2001, JGR, Equation 5b,7c,7d,9 )
!************************************************************

 if (isflg == 0) then
    coszen =  cos(iv%instid(inst)%rad(n)%satzen)
    d0     =  8.24-(2.622-1.846*coszen)*coszen
    d1     =  0.754
    d2     =  -2.265
    ts     =  iv%instid(inst)%rad(n)%ts
    tb1    =  ob%instid(inst)%rad(n)%tb(1)
    tb2    =  ob%instid(inst)%rad(n)%tb(2)
    clw    =  coszen*(d0+d1*log(ts-tb1)+d2*log(ts-tb2))
    clw    =  clw - 0.03
 end if

   return

END SUBROUTINE da_amsua_rad
