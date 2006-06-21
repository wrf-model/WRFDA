subroutine da_tp_to_qs( t, p, es, qs)

!------------------------------------------------------------------------------
!  PURPOSE: Convert T/p to saturation specific humidity.
!
!  METHOD: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
!          Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
!  HISTORY: 10/03/2000 - Creation of F90 version.           Dale Barker
!  MODIFIED: 10/01/2002					Wei Huang
!------------------------------------------------------------------------------

   IMPLICIT NONE

   real, intent(in)  :: t, p
   real, intent(out) :: es, qs
   
   REAL              :: t_c              ! T in degreesC.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   t_c = t - t_kelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure:
!------------------------------------------------------------------------------

   es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
   
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity:
!------------------------------------------------------------------------------

   qs = rd_over_rv * es / ( p - rd_over_rv1 * es )

end subroutine da_tp_to_qs
