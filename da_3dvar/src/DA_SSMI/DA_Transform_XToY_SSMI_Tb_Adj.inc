
!============================================================================
SUBROUTINE DA_Transform_XToY_SSMI_Tb_Adj( xb, oi, xp, jo_grad_y, jo_grad_x )
!----------------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------------

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: oi          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

!  LOCAL VARS

   integer                       :: n                ! loop counter.
   integer                       :: i,j
   real                          :: dx, dy, dxm, dym
!  real                          :: usfc, vsfc, uasfc, vasfc
!  real                          :: ADJ_usfc, ADJ_vsfc
!  real                          :: dum1, dum2, zrhom, ADJ_zrhom

!  real, dimension(mix,mjy)      :: ADJ_dotspeed
!  real, dimension(mix,mjy)      :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
!  real, dimension(mix,mjy)      :: ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_tpw
!  real, dimension(mix,mjy)      :: ADJ_htpw,ADJ_speed,ADJ_alw,ADJ_zcld         

!  real, dimension(mix,mjy)      :: tb19v,tb19h, &
!                                   tb22v,       &
!                                   tb37v,tb37h, &
!                                   tb85v,tb85h

!  real, dimension(mix,mjy)      :: ADJ_tb19v,ADJ_tb19h, &
!                                   ADJ_tb22v,           &
!                                   ADJ_tb37v,ADJ_tb37h, &
!                                   ADJ_tb85v,ADJ_tb85h

!  real, dimension(mkz)          :: zh
!  real, dimension(mkz+1)        :: zf
!----------------------------------------------------------------------------
!   print*,'oi % num_ssmi_tb=',oi % num_ssmi_tb
   do n=1, oi % num_ssmi_tb

     if(oi%ssmi_tb(n)%loc%proc_domain_with_halo) then

         i = oi%ssmi_tb(n)%loc%i
         j = oi%ssmi_tb(n)%loc%j
         dx = oi%ssmi_tb(n)%loc%dx
         dy = oi%ssmi_tb(n)%loc%dy
         dxm = oi%ssmi_tb(n)%loc%dxm
         dym = oi%ssmi_tb(n)%loc%dym

#ifndef DEREF_KLUDGE
         call Interp_lin_2D_adj(jo_grad_x%tb19v, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19v)
         call Interp_lin_2D_adj(jo_grad_x%tb19h, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19h)
         call Interp_lin_2D_adj(jo_grad_x%tb22v, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb22v)
         call Interp_lin_2D_adj(jo_grad_x%tb37v, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37v)
         call Interp_lin_2D_adj(jo_grad_x%tb37h, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37h)
         call Interp_lin_2D_adj(jo_grad_x%tb85v, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85v)
         call Interp_lin_2D_adj(jo_grad_x%tb85h, xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85h)
#else
         call Interp_lin_2D_adj(jo_grad_x%tb19v(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19v)
         call Interp_lin_2D_adj(jo_grad_x%tb19h(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb19h)
         call Interp_lin_2D_adj(jo_grad_x%tb22v(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb22v)
         call Interp_lin_2D_adj(jo_grad_x%tb37v(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37v)
         call Interp_lin_2D_adj(jo_grad_x%tb37h(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb37h)
         call Interp_lin_2D_adj(jo_grad_x%tb85v(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85v)
         call Interp_lin_2D_adj(jo_grad_x%tb85h(xb%ims,xb%jms), &
                                xb%ims, xb%ime, xb%jms, xb%jme, &
                                i, j, dx, dy, dxm, dym, jo_grad_y%ssmi_tb(n)%tb85h)
#endif
     endif

   end do

END SUBROUTINE DA_Transform_XToY_SSMI_Tb_Adj

