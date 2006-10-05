subroutine da_tp_to_qs_adj1( t, p, es, t_prime, p_prime, &
                             qs_prime_over_qs )

   !---------------------------------------------------------------------------
   !  PURPOSE: Adjoint of da_tp_to_qs_lin.
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   REAL, INTENT(IN)              :: t                ! Temperature.
   REAL, INTENT(IN)              :: p                ! Pressure.
   REAL, INTENT(IN)              :: es               ! Sat. vapour pressure.
   REAL, INTENT(INOUT)           :: t_prime          ! Temperature increment.
   REAL, INTENT(INOUT)           :: p_prime          ! Pressure increment.
   REAL, INTENT(IN)              :: qs_prime_over_qs ! qs~/qs.
   
   REAL                          :: temp             ! Temporary storage.
   REAL                          :: es_prime_over_es ! es~/es
      
   !---------------------------------------------------------------------------
   ! [3.0] Calculate saturation specific humidity increment:
   !---------------------------------------------------------------------------

   temp = qs_prime_over_qs / ( p - rd_over_rv1 * es )
   
   es_prime_over_es = temp * p

   p_prime = p_prime - temp
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure increment:
   !---------------------------------------------------------------------------

   temp = t + es_gammakelvin

   t_prime = t_prime + es_gammabeta * es_prime_over_es / ( temp * temp )

end subroutine da_tp_to_qs_adj1


