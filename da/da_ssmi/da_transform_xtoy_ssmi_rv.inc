subroutine da_transform_xtoy_ssmi_rv(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! loop counter.

   ! SSMI observation operator y = H(x):

   do n=1, iv % num_ssmi_retrieval
      ! Interpolating from 4 grid points to obs location
      call da_interp_lin_2d(grid%xa%tpw,   iv%ssmi_retrieval(n)%loc, y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d(grid%xa%speed, iv%ssmi_retrieval(n)%loc, y%ssmi_retrieval(n)%speed)      
   end do

end subroutine da_transform_xtoy_ssmi_rv


