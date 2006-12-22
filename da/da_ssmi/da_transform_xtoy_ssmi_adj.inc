subroutine da_transform_xtoy_ssmi_adj ( xb, oi, jo_grad_y, jo_grad_x )

   implicit none

   type (xb_type), intent(in)        :: xb          ! first guess state.
   type (ob_type), intent(in)        :: oi          ! obs. inc vector (o-b).
   type (y_type) , intent(in)        :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)     :: jo_grad_x   ! grad_x(jo)

   if ( oi%num_ssmi_tb > 0 ) then
      call da_transform_xtoy_ssmi_tb_adj (xb, oi, jo_grad_y, jo_grad_x)
   else if ( oi%num_ssmi_retrieval > 0) then
      call da_transform_xtoy_ssmi_rv_adj (xb, oi, jo_grad_y, jo_grad_x )
   end if

end subroutine da_transform_xtoy_ssmi_adj


