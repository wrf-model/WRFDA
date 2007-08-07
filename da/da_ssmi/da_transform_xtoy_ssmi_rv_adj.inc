subroutine da_transform_xtoy_ssmi_rv_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)       :: grid
   type (ob_type), intent(in)       :: iv          ! obs. inc. vector (o-b).
   type (y_type) , intent(in)       :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)    :: jo_grad_x   ! grad_x(jo)

   integer :: n
        
   do n=1, iv % num_ssmi_retrieval
      call da_interp_lin_2d_adj(jo_grad_x%tpw, iv%ssmi_retrieval(n)%loc, &
         jo_grad_y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d_adj(jo_grad_x%speed, iv%ssmi_retrieval(n)%loc, &
         jo_grad_y%ssmi_retrieval(n)%speed)
   end do

end subroutine da_transform_xtoy_ssmi_rv_adj


