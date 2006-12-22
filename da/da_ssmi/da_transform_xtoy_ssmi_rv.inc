subroutine da_transform_xtoy_ssmi_rv( xb, oi, xa, y )

   implicit none
   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (ob_type), intent(in)   :: oi       ! obs. increment vector (o-b).
   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (y_type), intent(inout) :: y        ! y = h (xa)

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

#ifndef DEREF_KLUDGE
      call da_interp_lin_2d(xa%tpw, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d(xa%speed, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%speed)
#else
      call da_interp_lin_2d(xa%tpw(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                           i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%tpw)
      call da_interp_lin_2d(xa%speed(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%speed)
#endif
      
   end do

end subroutine da_transform_xtoy_ssmi_rv


