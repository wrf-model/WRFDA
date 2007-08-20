subroutine da_transform_xtoy_pseudo(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pseudo")

   do n=iv%plocal(iv%time-1,pseudo) + 1, iv%plocal(iv%time,pseudo)
      call da_interp_obs_lin_2d(grid%xa % u, iv%pseudo(n)%loc, y%pseudo(n)%u, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d(grid%xa % v, iv%pseudo(n)%loc, y%pseudo(n)%v, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d(grid%xa % t, iv%pseudo(n)%loc, y%pseudo(n)%t, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d(grid%xa % p, iv%pseudo(n)%loc, y%pseudo(n)%p, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d(grid%xa % q, iv%pseudo(n)%loc, y%pseudo(n)%q, iv%pseudo(n)%zk)
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pseudo")

end subroutine da_transform_xtoy_pseudo


