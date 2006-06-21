subroutine da_tpq_to_rh_lin1( t, p, es, rh, &
                              t_prime, p_prime, q_prime, rh_prime )

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/pressure/q to relative humidity increments.
!
!  METHOD: r~ = r (q~/q - qs~/qs).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!
!  When q approaching to zero, the above formula is undefined. The
!  general formula below must be used:
! 
!  METHOD: r~ = 100 * (q~/qs) - rh*(qs~/qs)
!
!  10/30/2003                                               Yong-Run Guo
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)              :: t        ! Temperature.
   REAL, INTENT(IN)              :: p        ! Pressure.
   REAL, INTENT(IN)              :: es       ! Saturation vapour pressure.
   REAL, INTENT(IN)              :: rh       ! Relative Humidity.
   REAL, INTENT(IN)              :: t_prime  ! Temperature increment.
   REAL, INTENT(IN)              :: p_prime  ! Pressure increment.
   REAL, INTENT(IN)              :: q_prime  ! Pressure increment.
   REAL, INTENT(OUT)             :: rh_prime ! Pressure increment.
   
   REAL                          :: es1, qs  ! Saturation specific humidity.
   REAL                          :: qs_prime_over_qs ! qs~/qs.

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

   call da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )
   
!-----------------------------------------------------------------------------
!  [2.0] Culcalete background saturation specific humidity qs:
!-----------------------------------------------------------------------------

   call da_tp_to_qs( t, p, es1, qs) 
   
!------------------------------------------------------------------------------
!  [3.0] Calculate relative humidity increment:
!------------------------------------------------------------------------------

   rh_prime = 100. * (q_prime / qs) - rh * qs_prime_over_qs

end subroutine da_tpq_to_rh_lin1


