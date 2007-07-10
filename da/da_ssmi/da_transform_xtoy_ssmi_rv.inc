subroutine da_transform_xtoy_ssmi_rv(grid, oi, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)   :: grid
   type (ob_type), intent(in)   :: oi       ! obs. increment vector (o-b).
   type (y_type), intent(inout) :: y        ! y = h (grid%xa)

   integer                      :: n        ! loop counter.
   integer                      :: i, j
   real                         :: dx, dy, dxm, dym

   !  SSMI observation operator y = H(x):

   do n=1, oi % num_ssmi_retrieval
      i = oi%ssmi_retrieval(n)%loc%i
      j = oi%ssmi_retrieval(n)%loc%j
      dx = oi%ssmi_retrieval(n)%loc%dx
      dy = oi%ssmi_retrieval(n)%loc%dy
      dxm = oi%ssmi_retrieval(n)%loc%dxm
      dym = oi%ssmi_retrieval(n)%loc%dym

      ! Interpolating from 4 grid points to obs location

      call da_interp_lin_2d(grid%xa%tpw, ims, ime, jms, jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d(grid%xa%speed, ims, ime, jms, jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%speed)      
   end do

end subroutine da_transform_xtoy_ssmi_rv


