subroutine da_transform_xtoy_qscat(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.


   do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat
      call da_interp_obs_lin_2d(grid%xa % u, iv%qscat(n)%loc, y%qscat(n)%u, iv%qscat(n)%zk)
      call da_interp_obs_lin_2d(grid%xa % v, iv%qscat(n)%loc, y%qscat(n)%v, iv%qscat(n)%zk)
   end do

end subroutine da_transform_xtoy_qscat


