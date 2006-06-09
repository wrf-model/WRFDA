SUBROUTINE da_tprh_to_q_lin1( t, p, es, q, rh, &
                              t_prime, p_prime, rh_prime, q_prime )

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/pressure/rh to specific humidity increments.
!
!  METHOD: q~ = q (rh~/rh + qs~/qs).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)              :: t        ! Temperature.
   REAL, INTENT(IN)              :: p        ! Pressure.
   REAL, INTENT(IN)              :: es       ! Saturation vapour pressure.
   REAL, INTENT(IN)              :: q        ! Specific humidity.
   REAL, INTENT(IN)              :: rh       ! Relative Humidity.
   REAL, INTENT(IN)              :: t_prime  ! Temperature increment.
   REAL, INTENT(IN)              :: p_prime  ! Pressure increment.
   REAL, INTENT(IN)              :: rh_prime ! Pressure increment.
   REAL, INTENT(OUT)             :: q_prime  ! Pressure increment.
   
   REAL                          :: qs_prime_over_qs ! qs~/qs.

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

   CALL da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, qs_prime_over_qs )
   
!------------------------------------------------------------------------------
!  [2.0] Calculate specific humidity increment:
!------------------------------------------------------------------------------

   q_prime = q * ( rh_prime / rh + qs_prime_over_qs )

END SUBROUTINE da_tprh_to_q_lin1
