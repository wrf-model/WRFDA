subroutine da_transform_xtoy_qscat_adj(iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.

   do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat
      ! [1.2] Interpolate horizontally:
      call da_interp_obs_lin_2d_adj(jo_grad_x%u, iv%qscat(n)%loc, jo_grad_y%qscat(n)%u, iv%qscat(n)%zk)
      call da_interp_obs_lin_2d_adj(jo_grad_x%v, iv%qscat(n)%loc, jo_grad_y%qscat(n)%v, iv%qscat(n)%zk)
   end do

end subroutine da_transform_xtoy_qscat_adj


