subroutine da_transform_xtoy_sonde_sfc (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout) :: grid
   type (ob_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound
         call da_interp_obs_lin_2d(grid%xa % u, iv%sonde_sfc(n)%loc, y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % v, iv%sonde_sfc(n)%loc, y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % t, iv%sonde_sfc(n)%loc, y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % q, iv%sonde_sfc(n)%loc, y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

         call da_interp_lin_2d(grid%xa % psfc, iv%sonde_sfc(n)%loc, y%sonde_sfc(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
         iv%ob_numb(iv%current_ob_time)%sound
         call da_transform_xtopsfc(grid, iv%sonde_sfc(n), y%sonde_sfc(n))
      end do
  end if

end subroutine da_transform_xtoy_sonde_sfc


