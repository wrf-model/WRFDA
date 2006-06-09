 SUBROUTINE da_tpq_to_rh( t, p, q, es, qs, rh )

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/p/q to relative humidity rh.
!
!  METHOD: rh = 100 * q / qs.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.       Dale Barker
!  MODIFIED:	10/01/2002				Wei Huang
!------------------------------------------------------------------------------

   IMPLICIT NONE

   real, intent(in)  :: t, p, q
   real, intent(out) :: es, qs, rh

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity:
!------------------------------------------------------------------------------

   CALL da_tp_to_qs( t, p, es, qs )
   
!------------------------------------------------------------------------------
!  [2.0] Calculate relative humidity:
!------------------------------------------------------------------------------

   rh = 100.0 * q / qs

END SUBROUTINE da_tpq_to_rh

