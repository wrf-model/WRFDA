!============================================================================
SUBROUTINE DA_Transform_XToY_SSMI_Rv( xb, oi, xa, y )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------
   type (xb_type), intent(in)   :: xb       ! first guess state.
   type (ob_type), intent(in)   :: oi       ! obs. increment vector (o-b).
   type (x_type),  intent(in)   :: xa       ! gridded analysis increment.
   type (y_type), intent(inout) :: y        ! y = h (xa)

! LOCAL VARS

   integer                      :: n        ! loop counter.
   integer                      :: i, j
   real                         :: dx, dy, dxm, dym
!--------------------------------------------------------------------------------
!  SSMI observation operator y = H(x):
!--------------------------------------------------------------------------------

   do n=1, oi % num_ssmi_retrieval

      if(oi%ssmi_retrieval(n)%loc%proc_domain_with_halo) then
         i = oi%ssmi_retrieval(n)%loc%i
         j = oi%ssmi_retrieval(n)%loc%j
         dx = oi%ssmi_retrieval(n)%loc%dx
         dy = oi%ssmi_retrieval(n)%loc%dy
         dxm = oi%ssmi_retrieval(n)%loc%dxm
         dym = oi%ssmi_retrieval(n)%loc%dym

!--------Interpolating from 4 grid points to obs location

#ifndef DEREF_KLUDGE
         call Interp_lin_2D(xa%tpw, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%tpw)
         call Interp_lin_2D(xa%speed, xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%speed)
#else
         call Interp_lin_2D(xa%tpw(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%tpw)
         call Interp_lin_2D(xa%speed(xb%ims,xb%jms), xb%ims, xb%ime, xb%jms, xb%jme, &
                            i, j, dx, dy, dxm, dym, y%ssmi_retrieval(n)%speed)
#endif
      endif
   end do

END SUBROUTINE DA_Transform_XToY_SSMI_Rv

