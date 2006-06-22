subroutine da_transform_xtoy_ssmi_adj ( xb, oi, jo_grad_y, jo_grad_x )

   IMPLICIT NONE

   TYPE (xb_type), INTENT(IN)        :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)        :: oi          ! obs. inc vector (o-b).
   TYPE (y_type) , INTENT(IN)        :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT)     :: jo_grad_x   ! grad_x(jo)

   IF ( oi%num_ssmi_tb > 0 ) THEN
      call da_transform_xtoy_ssmi_tb_adj (xb, oi, jo_grad_y, jo_grad_x)
   ELSE IF ( oi%num_ssmi_retrieval > 0) THEN
      call da_transform_xtoy_ssmi_rv_adj (xb, oi, jo_grad_y, jo_grad_x )
   ENDIF

end subroutine da_transform_xtoy_ssmi_adj

