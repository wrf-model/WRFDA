
SUBROUTINE da_get_innov_vector_ssmi ( it, xb, xp, ob, iv )

   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it         ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb         ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp         ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)    :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv         ! O-B structure.



   IF ( ob%num_ssmi_tb > 0 ) THEN

      CALL da_get_innov_vector_ssmi_tb ( it, xb, xp, ob, iv )

   ELSE IF ( ob%num_ssmi_retrieval > 0) THEN

      CALL da_get_innov_vector_ssmi_rv ( it, xb, xp, ob, iv )

   ENDIF

END SUBROUTINE da_get_innov_vector_ssmi

