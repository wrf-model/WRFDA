subroutine da_tp_to_qs_lin1( t, p, es, t_prime, p_prime, &
                             qs_prime_over_qs )

!------------------------------------------------------------------------------
!  PURPOSE: Convert es/p/es_prime to saturation specific humidity increment.
!
!  METHOD: qs~ = qs * ( p es'/es - p' ) / ( p - (1-rd_over_rv) es ).
!          Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   REAL, INTENT(IN)              :: t                ! Temperature.
   REAL, INTENT(IN)              :: p                ! Pressure.
   REAL, INTENT(IN)              :: es               ! Sat. vapour pressure.
   REAL, INTENT(IN)              :: t_prime          ! Temperature increment.
   REAL, INTENT(IN)              :: p_prime          ! Pressure increment.
   REAL, INTENT(OUT)             :: qs_prime_over_qs ! qs~/qs.
   
   REAL                          :: temp           ! Temporary value.
   REAL                          :: es_prime_over_es ! es~/es

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   temp = t + es_gammakelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

   es_prime_over_es = es_gammabeta * t_prime / ( temp * temp )

!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

   qs_prime_over_qs = ( p * es_prime_over_es - p_prime ) / &
                      ( p - rd_over_rv1 * es )

end subroutine da_tp_to_qs_lin1
