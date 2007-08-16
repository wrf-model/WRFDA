subroutine da_transform_xtoy_synop (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_synop")

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%plocal(iv%time-1,synop) + 1, iv%plocal(iv%time,synop)
         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d(grid%xa % u, iv%synop(n)%loc, y%synop(n)%u, iv%synop(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % v, iv%synop(n)%loc, y%synop(n)%v, iv%synop(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % t, iv%synop(n)%loc, y%synop(n)%t, iv%synop(n)%zk)
         call da_interp_obs_lin_2d(grid%xa % q, iv%synop(n)%loc, y%synop(n)%q, iv%synop(n)%zk)

         call da_interp_lin_2d(grid%xa % psfc, iv%synop(n)%loc, y%synop(n)%p)
      end do
   else if (sfc_assi_options == sfc_assi_options_2) then
      do n=iv%plocal(iv%time-1,synop) + 1, iv%plocal(iv%time,synop)
         call da_transform_xtopsfc(grid, iv%synop(n), y%synop(n))
      end do
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_synop")

end subroutine da_transform_xtoy_synop


