subroutine da_transform_xtoy_ssmi ( xb, oi, xa, xp, y )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)   :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)   :: oi       ! obs. increment vector (o-b).
   TYPE (x_type),  INTENT(INOUT):: xa       ! gridded analysis increment.
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   IF ( oi%num_ssmi_tb > 0 ) THEN

      call da_transform_xtoy_ssmi_tb ( xb, oi, xa, xp, y )

   ELSE IF ( oi%num_ssmi_retrieval > 0) THEN

      call da_transform_xtoy_ssmi_rv ( xb, oi, xa, y )

   ENDIF

end subroutine da_transform_xtoy_ssmi

