SUBROUTINE DA_Transform_XToY_SSMI_Adj ( xb, oi, xp, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)        :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)        :: oi          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN)     :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(IN)        :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT)     :: jo_grad_x   ! grad_x(jo)

   IF ( oi%num_ssmi_tb > 0 ) THEN

      CALL DA_Transform_XToY_SSMI_Tb_Adj (xb, oi, xp, jo_grad_y, jo_grad_x)

   ELSE IF ( oi%num_ssmi_retrieval > 0) THEN

      CALL DA_Transform_XToY_SSMI_Rv_Adj (xb, oi, jo_grad_y, jo_grad_x )

   ENDIF

END SUBROUTINE DA_Transform_XToY_SSMI_Adj

