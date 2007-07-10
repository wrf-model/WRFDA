subroutine da_transform_xtoy_ssmi_adj (grid, oi, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)        :: grid
   type (ob_type), intent(in)        :: oi          ! obs. inc vector (o-b).
   type (y_type) , intent(in)        :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)     :: jo_grad_x   ! grad_x(jo)

   if (oi%num_ssmi_tb > 0) then
      call da_transform_xtoy_ssmi_tb_adj (grid, oi, jo_grad_y, jo_grad_x)
   else if (oi%num_ssmi_retrieval > 0) then
      call da_transform_xtoy_ssmi_rv_adj (grid, oi, jo_grad_y, jo_grad_x)
   end if

end subroutine da_transform_xtoy_ssmi_adj


