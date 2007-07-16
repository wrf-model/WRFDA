subroutine da_transform_xtoy_ssmi_tb_adj(grid, oi, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: oi          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n                ! loop counter.
   integer                       :: i,j
   real                          :: dx, dy, dxm, dym

   do n=1, oi % num_ssmi_tb

      i = oi%ssmi_tb(n)%loc%i
      j = oi%ssmi_tb(n)%loc%j
      dx = oi%ssmi_tb(n)%loc%dx
      dy = oi%ssmi_tb(n)%loc%dy
      dxm = oi%ssmi_tb(n)%loc%dxm
      dym = oi%ssmi_tb(n)%loc%dym

      call da_interp_lin_2d_adj(jo_grad_x%tb19v, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19v)
      call da_interp_lin_2d_adj(jo_grad_x%tb19h, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19h)
      call da_interp_lin_2d_adj(jo_grad_x%tb22v, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb22v)
      call da_interp_lin_2d_adj(jo_grad_x%tb37v, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37v)
      call da_interp_lin_2d_adj(jo_grad_x%tb37h, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37h)
      call da_interp_lin_2d_adj(jo_grad_x%tb85v, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85v)
      call da_interp_lin_2d_adj(jo_grad_x%tb85h, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85h)
   end do

end subroutine da_transform_xtoy_ssmi_tb_adj


