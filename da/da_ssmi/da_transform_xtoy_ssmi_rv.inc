subroutine da_transform_xtoy_ssmi_rv(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! loop counter.

   if (trace_use) call da_trace_entry("da_transform_xtoy_ssmi_rv")

   ! SSMI observation operator y = H(x):

   do n=1, iv % nlocal(ssmi_rv)
      ! Interpolating from 4 grid points to obs location
      call da_interp_lin_2d(grid%xa%tpw,   iv%ssmi_rv(n)%loc, y%ssmi_rv(n)%tpw)
      call da_interp_lin_2d(grid%xa%speed, iv%ssmi_rv(n)%loc, y%ssmi_rv(n)%speed)      
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoy_ssmi_rv")

end subroutine da_transform_xtoy_ssmi_rv


