subroutine da_transform_xtoy_metar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.

   if (trace_use) call da_trace_entry("da_transform_xtoy_metar")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar

         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d(grid%xa % u, iv%metar(n)%loc, y%metar(n)%u, iv%metar(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % v, iv%metar(n)%loc, y%metar(n)%v, iv%metar(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % t, iv%metar(n)%loc, y%metar(n)%t, iv%metar(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % q, iv%metar(n)%loc, y%metar(n)%q, iv%metar(n)%zk)

         call da_interp_lin_2d(grid%xa % psfc, iv%metar(n)%loc, y%metar(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      ! [2.0] Surface assimilation approach 2
      do n=iv%ob_numb(iv%current_ob_time-1)%metar + 1, iv%ob_numb(iv%current_ob_time)%metar
         call da_transform_xtopsfc(grid,iv%metar(n),y%metar(n))
      end do
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_metar")

end subroutine da_transform_xtoy_metar


