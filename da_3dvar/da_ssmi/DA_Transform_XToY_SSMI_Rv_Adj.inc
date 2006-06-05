!============================================================================
SUBROUTINE DA_Transform_XToY_SSMI_Rv_Adj( xb, oi, jo_grad_y, jo_grad_x )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------
   type (xb_type), intent(in)       :: xb          ! first guess.
   type (ob_type), intent(in)       :: oi          ! obs. inc. vector (o-b).
   type (y_type) , intent(in)       :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)    :: jo_grad_x   ! grad_x(jo)

! LOCAL VARS

   integer                      :: i, j, n
   real                         :: dx, dy, dxm, dym

!--------------------------------------------------------------------------------
         
   do n=1, oi % num_ssmi_retrieval

      if(oi%ssmi_retrieval(n)%loc%proc_domain_with_halo) then

         i = oi%ssmi_retrieval(n)%loc%i
         j = oi%ssmi_retrieval(n)%loc%j
         dx = oi%ssmi_retrieval(n)%loc%dx
         dy = oi%ssmi_retrieval(n)%loc%dy
         dxm = oi%ssmi_retrieval(n)%loc%dxm
         dym = oi%ssmi_retrieval(n)%loc%dym

#ifndef DEREF_KLUDGE
         call Interp_lin_2D_adj(jo_grad_x%tpw, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%tpw)
         call Interp_lin_2D_adj(jo_grad_x%speed, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%speed)
#else
         call Interp_lin_2D_adj(jo_grad_x%tpw(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%tpw)
         call Interp_lin_2D_adj(jo_grad_x%speed(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_retrieval(n)%speed)
#endif

      endif

   end do

END SUBROUTINE DA_Transform_XToY_SSMI_Rv_Adj

