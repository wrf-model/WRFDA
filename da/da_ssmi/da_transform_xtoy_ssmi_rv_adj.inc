subroutine da_transform_xtoy_ssmi_rv_adj(grid, oi, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)       :: grid
   type (ob_type), intent(in)       :: oi          ! obs. inc. vector (o-b).
   type (y_type) , intent(in)       :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)    :: jo_grad_x   ! grad_x(jo)

   integer                      :: i, j, n
   real                         :: dx, dy, dxm, dym
        
   do n=1, oi % num_ssmi_retrieval
      i = oi%ssmi_retrieval(n)%loc%i
      j = oi%ssmi_retrieval(n)%loc%j
      dx = oi%ssmi_retrieval(n)%loc%dx
      dy = oi%ssmi_retrieval(n)%loc%dy
      dxm = oi%ssmi_retrieval(n)%loc%dxm
      dym = oi%ssmi_retrieval(n)%loc%dym

      call da_interp_lin_2d_adj(jo_grad_x%tpw, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d_adj(jo_grad_x%speed, ims, ime, jms, &
         jme, i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%speed)
   end do

end subroutine da_transform_xtoy_ssmi_rv_adj


