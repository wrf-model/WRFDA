subroutine da_transform_xtopsfc(grid, synop, y_synop)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none

   type (domain),              intent(in) :: grid
   type (synop_type),          intent(in)  :: synop
   type (residual_synop_type), intent(out) :: y_synop

   real :: hsm, to, qo
   real :: tsm, qsm, psm    
   real :: psm_prime

   if (trace_use) call da_trace_entry("da_transform_xtopsfc")
stop

   ! 2.0 Surface assmiilation approach 2 (2-m t and q, 10-m u and v)

!   call da_interp_lin_2d(grid%xa%u10, synop%loc, y_synop%u)
!   call da_interp_lin_2d(grid%xa%v10, synop%loc, y_synop%v)
!   call da_interp_lin_2d(grid%xa%psfc, synop%loc, psm_prime)
!   call da_interp_lin_2d(grid%xa%t2, synop%loc, y_synop%t)
!   call da_interp_lin_2d(grid%xa%q2, synop%loc, y_synop%q)
   if (synop%p%qc < 0) then
      y_synop%p = 0.0
      return
   end if

   ! 3.0 The pressure at the observed height: 

   ! 3.1 Constants:

    to = -888888.0
    qo = -888888.0
       
   ! Terrain height at the observed site:

   ! 3.2 model background surface p, t, q, h at observed site:

!   call da_interp_lin_2d(grid%xb%terr, synop%loc, hsm)
!   call da_interp_lin_2d(grid%xb%t2,   synop%loc, tsm)
!   call da_interp_lin_2d(grid%xb%q2,   synop%loc, qsm)
!   call da_interp_lin_2d(grid%xb%psfc, synop%loc, psm)

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
   end if

   if (trace_use) call da_trace_exit("da_transform_xtopsfc")

end subroutine da_transform_xtopsfc


