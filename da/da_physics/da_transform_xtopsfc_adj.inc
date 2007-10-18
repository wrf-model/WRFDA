subroutine da_transform_xtopsfc_adj(grid, synop, j_synop_y, jo_grad_x) 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),              intent(in)    :: grid

   type (synop_type),          intent(in)    :: synop
   type (residual_synop_type), intent(inout) :: j_synop_y   ! grad_y(jo)
   type (x_type),              intent(inout) :: jo_grad_x   ! grad_x(jo)
   

   real :: hsm, to, qo
   real :: tsm, qsm, psm
   real :: psm_prime

   if (trace_use) call da_trace_entry("da_transform_xtopsfc_adj")
stop

   psm_prime = 0.0

   if (synop%p%qc >= 0) then
      !------------------------------------------------------------------------
      ! 2.0 Calculate gradient with respect the pressure at the observed height: 
      !------------------------------------------------------------------------

      ! 2.1 Terrain height at the observed site (xj, yi):

!      call da_interp_lin_2d(grid%xb%terr, synop%loc, hsm)
!      call da_interp_lin_2d(grid%xb%t2,   synop%loc, tsm)
!      call da_interp_lin_2d(grid%xb%q2,   synop%loc, qsm)
!      call da_interp_lin_2d(grid%xb%psfc, synop%loc, psm)

      to = -888888.0
      qo = -888888.0

      ! 2.3 Zero out the adjoint variables:

      !----------------------------------------------------------------
      ! 3.0 Surface pressure gradient at the observed height
      !----------------------------------------------------------------

      ! 4.0 Get the surface pressure gradient at the model surface height (hsm)
      ! 4.1 Both observed to and qo available:
      if (synop%t%qc >= 0 .and. synop%q%qc >= 0) then
         to = tsm + synop%t%inv
         qo = qsm + synop%q%inv
         call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
            psm, tsm, qsm, hsm, synop%h, to, qo)

         ! 4.2 only observed to available:
      else if (synop%t%qc >= 0 .and. synop%q%qc < 0) then
         to = tsm + synop%t%inv
         call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
            psm, tsm, qsm, hsm, synop%h, to)

         ! 4.3 Both observed to and qo missing:
      else
         call da_sfc_pre_adj (j_synop_y%p, psm_prime, j_synop_y%t, j_synop_y%q, &
            psm, tsm, qsm, hsm, synop%h)
      end if
   end if

   ! 2.2 convert the jo_grad_y to the model grids (t2, q2, u10, v10, and psfc)
!   call da_interp_lin_2d_adj(jo_grad_x%t2,   synop%loc, j_synop_y%t)
!   call da_interp_lin_2d_adj(jo_grad_x%q2,   synop%loc, j_synop_y%q)
!   call da_interp_lin_2d_adj(jo_grad_x%u10,  synop%loc, j_synop_y%u)
!   call da_interp_lin_2d_adj(jo_grad_x%v10,  synop%loc, j_synop_y%v)
!   call da_interp_lin_2d_adj(jo_grad_x%psfc, synop%loc, psm_prime)

   if (trace_use) call da_trace_exit("da_transform_xtopsfc_adj")

end subroutine da_transform_xtopsfc_adj


