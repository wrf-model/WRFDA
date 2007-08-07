subroutine da_transform_xtoy_ssmi_tb_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n                ! loop counter.

   do n=1, iv % num_ssmi_tb
      call da_interp_lin_2d_adj(jo_grad_x%tb19v, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb19v)
      call da_interp_lin_2d_adj(jo_grad_x%tb19h, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb19h)
      call da_interp_lin_2d_adj(jo_grad_x%tb22v, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb22v)
      call da_interp_lin_2d_adj(jo_grad_x%tb37v, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb37v)
      call da_interp_lin_2d_adj(jo_grad_x%tb37h, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb37h)
      call da_interp_lin_2d_adj(jo_grad_x%tb85v, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb85v)
      call da_interp_lin_2d_adj(jo_grad_x%tb85h, iv%ssmi_tb(n)%loc, jo_grad_y%ssmi_tb(n)%tb85h)
   end do

end subroutine da_transform_xtoy_ssmi_tb_adj


