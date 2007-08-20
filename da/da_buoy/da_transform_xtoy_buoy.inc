subroutine da_transform_xtoy_buoy (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_buoy")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,buoy) + 1, iv%plocal(iv%time,buoy)
         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d(grid%xa % u, iv%buoy(n)%loc, y%buoy(n)%u, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % v, iv%buoy(n)%loc, y%buoy(n)%v, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % t, iv%buoy(n)%loc, y%buoy(n)%t, iv%buoy(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % q, iv%buoy(n)%loc, y%buoy(n)%q, iv%buoy(n)%zk)

         call da_interp_lin_2d(grid%xa % psfc, iv%buoy(n)%loc, y%buoy(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      ! [2.0] Surface assimilation approach 2
      do n=iv%plocal(iv%time-1,buoy) + 1, iv%plocal(iv%time,buoy)
         call da_transform_xtopsfc(grid,iv%buoy(n),y%buoy(n))
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_buoy")

end subroutine da_transform_xtoy_buoy


