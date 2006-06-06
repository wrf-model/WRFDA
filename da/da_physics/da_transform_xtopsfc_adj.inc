SUBROUTINE da_transform_xtopsfc_adj(xb, xp, synop, j_synop_y, jo_grad_x ) 

   IMPLICIT NONE

   TYPE (xb_type),    INTENT(IN) :: xb       ! first guess state.
   type (xpose_type), INTENT(IN) :: xp       ! Domain decomposition vars.

   TYPE (synop_type),          INTENT(IN)    :: synop
   TYPE (residual_synop_type), INTENT(INOUT) :: j_synop_y   ! grad_y(jo)
   TYPE (x_type),              INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)
   
   INTEGER                :: i, j     ! Index dimension.
   REAL                   :: dx, dxm  ! 
   REAL                   :: dy, dym  !

   REAL    :: hsm, to, qo
   REAL    :: tsm, qsm, psm
   REAL    :: psm_prime

!--------------------------------------------------------------------
! 1.0 Get cross pt. horizontal interpolation weights:
!--------------------------------------------------------------------

   i   = synop%loc%i
   dy  = synop%loc%dy
   dym = synop%loc%dym
   j   = synop%loc%j
   dx  = synop%loc%dx
   dxm = synop%loc%dxm

   psm_prime = 0.0
   if (synop%p%qc >= 0) then

! ------------------------------------------------------------------------
! 2.0 Calculate gradient with respect the pressure at the observed height: 
! ------------------------------------------------------------------------

! 2.1 Terrain height at the observed site (xj, yi):

#ifndef DEREF_KLUDGE
   call da_interp_lin_2d(xb%terr, xp%ims, xp%ime, xp%jms, xp%jme, &
                       i, j, dx, dy, dxm, dym, hsm)
   call da_interp_lin_2d(xb%t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                       i, j, dx, dy, dxm, dym, tsm)
   call da_interp_lin_2d(xb%q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                       i, j, dx, dy, dxm, dym, qsm)
   call da_interp_lin_2d(xb%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                       i, j, dx, dy, dxm, dym, psm)
#else
   call da_interp_lin_2d(xb%terr(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, hsm)
   call da_interp_lin_2d(xb%t2(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, tsm)
   call da_interp_lin_2d(xb%q2(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, qsm)
   call da_interp_lin_2d(xb%psfc(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, psm)
#endif
  
   to = -888888.0
   qo = -888888.0

! 2.3 Zero out the adjoint variables:
       
! ----------------------------------------------------------------
! 3.0 Surface pressure gradient at the observed height
! ----------------------------------------------------------------

! 4.0 Get the surface pressure gradient at the model surface height (hsm)
! 4.1 Both observed to and qo available:
   if (synop%t%qc >= 0 .and. synop%q%qc >= 0) then
     to = tsm + synop%t%inv
     qo = qsm + synop%q%inv
     call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
                          psm, tsm, qsm, hsm, synop%h, to, qo)

! 4.2 Only observed to available:

   else if (synop%t%qc >= 0 .and. synop%q%qc < 0) then
     to = tsm + synop%t%inv
     call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
                          psm, tsm, qsm, hsm, synop%h, to)

! 4.3 Both observed to and qo missing:

   else
     call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
                          psm, tsm, qsm, hsm, synop%h)
   endif

   endif
!   2.2 convert the jo_grad_y to the model grids (t2, q2, u10, v10, and psfc)
#ifndef DEREF_KLUDGE
   call da_interp_lin_2d_adj(jo_grad_x%t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%t)
   call da_interp_lin_2d_adj(jo_grad_x%q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%q)
   call da_interp_lin_2d_adj(jo_grad_x%u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%u)
   call da_interp_lin_2d_adj(jo_grad_x%v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%v)
   call da_interp_lin_2d_adj(jo_grad_x%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, psm_prime)
#else
   call da_interp_lin_2d_adj(jo_grad_x%t2(xp%ims,xp%jms), &
                          xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%t)
   call da_interp_lin_2d_adj(jo_grad_x%q2(xp%ims,xp%jms), &
                          xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%q)
   call da_interp_lin_2d_adj(jo_grad_x%u10(xp%ims,xp%jms), &
                          xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%u)
   call da_interp_lin_2d_adj(jo_grad_x%v10(xp%ims,xp%jms), &
                          xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, j_synop_y%v)
   call da_interp_lin_2d_adj(jo_grad_x%psfc(xp%ims,xp%jms), &
                          xp%ims, xp%ime, xp%jms, xp%jme, &
                          i, j, dx, dy, dxm, dym, psm_prime)
#endif

END SUBROUTINE da_transform_xtopsfc_adj

