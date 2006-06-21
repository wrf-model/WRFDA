subroutine da_transform_xtopsfc( xb, xa, xp, synop, y_synop )

   TYPE (x_type),     INTENT(IN) :: xa       ! gridded analysis increment.
   TYPE (xb_type),    INTENT(IN) :: xb       ! first guess state.
   type (xpose_type), INTENT(IN) :: xp       ! Domain decomposition vars.

   TYPE (synop_type),          INTENT(IN)  :: synop
   TYPE (residual_synop_type), INTENT(OUT) :: y_synop

   INTEGER                :: i, j     ! Index dimension.
   REAL                   :: dx, dxm  ! 
   REAL                   :: dy, dym  !

   REAL    :: hsm, to, qo
   REAL    :: tsm, qsm, psm    
   REAL    :: psm_prime

!----------------------------------------------------------------------

! 1.0 Get cross pt. horizontal interpolation weights:

   i   = synop%loc%i
   dy  = synop%loc%dy
   dym = synop%loc%dym
   j   = synop%loc%j
   dx  = synop%loc%dx
   dxm = synop%loc%dxm

! 2.0 Surface assimilation approach 2 (2-m t and q, 10-m u and v)

#ifndef DEREF_KLUDGE
   call da_interp_lin_2d(xa%u10, xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%u)
   call da_interp_lin_2d(xa%v10, xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%v)
   call da_interp_lin_2d(xa%psfc, xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, psm_prime)
   call da_interp_lin_2d(xa%t2, xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%t)
   call da_interp_lin_2d(xa%q2, xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%q)
#else
   call da_interp_lin_2d(xa%u10(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%u)
   call da_interp_lin_2d(xa%v10(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%v)
   call da_interp_lin_2d(xa%psfc(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, psm_prime)
   call da_interp_lin_2d(xa%t2(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%t)
   call da_interp_lin_2d(xa%q2(xp%ims,xp%jms), &
                      xp%ims, xp%ime, xp%jms, xp%jme, &
                      i, j, dx, dy, dxm, dym, y_synop%q)
#endif
   if (synop%p%qc < 0) then
       y_synop%p = 0.0
       return
   endif

! 3.0 The pressure at the observed height: 

! 3.1 Constants:

    to = -888888.0
    qo = -888888.0
       
!  ...... Terrain height at the observed site:

! 3.2 model background surface p, t, q, h at observed site:

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

! 3.3 perturbations t, q, p at the model surface:

! 4.0 Compute the perturbation of the surface pressure perturbation 
!     at the observed height

    if (synop%t%qc >= 0 .and. synop%q%qc >= 0) then

! 4.1 Observed value = background + innovation: both t, q available:
!     ----------------------------------------

     to = tsm + synop%t%inv
     qo = qsm + synop%q%inv
     call da_sfc_pre_lin(y_synop%p, psm_prime, y_synop%t, y_synop%q, &
                         psm, tsm, qsm, hsm, synop%h, to, qo)

    else if (synop%t%qc >= 0 .and. synop%q%qc < 0) then

! 4.2 Observed value = background + innovation: only t available
!     ----------------------------------------

     to = tsm + synop%t%inv
     call da_sfc_pre_lin(y_synop%p, psm_prime, y_synop%t, y_synop%q, &
                         psm, tsm, qsm, hsm, synop%h, to)

    else
! 4.3 No observed t and q available:
!     -----------------------------

     call da_sfc_pre_lin(y_synop%p, psm_prime, y_synop%t, y_synop%q, &
                         psm, tsm, qsm, hsm, synop%h)
    endif

end subroutine da_transform_xtopsfc

