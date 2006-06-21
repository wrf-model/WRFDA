subroutine da_tprh_to_q_adj1( t, p, es, q, rh, &
                              t_prime, p_prime, rh_prime, q_prime )

!------------------------------------------------------------------------------
!  PURPOSE: Adjoint of DA_TPRH_To_Q_Adj.
!
!  METHOD: Standard adjoint coding.
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   REAL, INTENT(IN)              :: t        ! Temperature.
   REAL, INTENT(IN)              :: es       ! Saturation vapour pressure.
   REAL, INTENT(IN)              :: p        ! Pressure.
   REAL, INTENT(IN)              :: q        ! Specific humidity.
   REAL, INTENT(IN)              :: rh       ! Relative Humidity.
   REAL, INTENT(INOUT)           :: t_prime  ! Temperature increment.
   REAL, INTENT(INOUT)           :: p_prime  ! Pressure increment.
   REAL, INTENT(INOUT)           :: rh_prime ! Pressure increment.
   REAL, INTENT(IN)              :: q_prime  ! Pressure increment.
   
   REAL                          :: temp     ! Temporary storage.
   REAL                          :: qs_prime_over_qs ! qs~/qs.

  temp = q * q_prime

!------------------------------------------------------------------------------
!  [2.0] Calculate relative humidity increment:
!------------------------------------------------------------------------------

  rh_prime = rh_prime + temp / rh
  qs_prime_over_qs = temp

!------------------------------------------------------------------------------
!  [1.0] Calculate saturation specific humidity ratio qs~/qs:
!------------------------------------------------------------------------------

  call da_tp_to_qs_adj1( t, p, es, t_prime, p_prime, qs_prime_over_qs )

end subroutine da_tprh_to_q_adj1
