SUBROUTINE da_check_xtoy_adjoint_ssmi( oi, y, adjtest_lhs, pertile_lhs )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)     :: oi            ! obs. inc. vector (o-b).
   TYPE (y_type) , INTENT(INOUT)  :: y             ! y = h (xa)
   REAL          , INTENT(INOUT)  :: adjtest_lhs, pertile_lhs   

   IF ( oi%num_ssmi_tb > 0 ) THEN

      CALL da_check_xtoy_adjoint_ssmi_tb ( oi, y, adjtest_lhs, pertile_lhs )

   ELSE IF ( oi%num_ssmi_retrieval > 0) THEN

      CALL da_check_xtoy_adjoint_ssmi_rv( oi, y, adjtest_lhs, pertile_lhs )

   ENDIF

END SUBROUTINE da_check_xtoy_adjoint_ssmi

