subroutine da_get_innov_vector_ssmi ( it, xb, ob, iv )

   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it         ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb         ! first guess state.
   TYPE (y_type),  INTENT(IN)    :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv         ! O-B structure.

   IF ( ob%num_ssmi_tb > 0 ) THEN

      call da_get_innov_vector_ssmi_tb ( it, xb, ob, iv )

   ELSE IF ( ob%num_ssmi_retrieval > 0) THEN

      call da_get_innov_vector_ssmi_rv ( it, xb, ob, iv )

   ENDIF

end subroutine da_get_innov_vector_ssmi

