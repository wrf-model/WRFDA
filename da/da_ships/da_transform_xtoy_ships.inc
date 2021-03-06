subroutine da_transform_xtoy_ships (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout)  :: grid
   type (iv_type),    intent(in)     :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout)  :: y        ! y = h (grid%xa) (linear)

   integer :: n   ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_psfc(:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_ships")

   if (sfc_assi_options == sfc_assi_options_1) then
      allocate (model_u(1,iv%info(ships)%n1:iv%info(ships)%n2))
      allocate (model_v(1,iv%info(ships)%n1:iv%info(ships)%n2))
      allocate (model_t(1,iv%info(ships)%n1:iv%info(ships)%n2))
      allocate (model_q(1,iv%info(ships)%n1:iv%info(ships)%n2))
      allocate (model_psfc(iv%info(ships)%n1:iv%info(ships)%n2))
      ! [1.2] Interpolate horizontally:
      call da_interp_lin_3d (grid%xa%u, iv%info(ships), model_u)
      call da_interp_lin_3d (grid%xa%v, iv%info(ships), model_v)
      call da_interp_lin_3d (grid%xa%t, iv%info(ships), model_t)
      call da_interp_lin_3d (grid%xa%q, iv%info(ships), model_q)

      call da_interp_lin_2d (grid%xa%psfc, iv%info(ships), 1, model_psfc)

      do n=iv%info(ships)%n1,iv%info(ships)%n2
         y%ships(n)%u = model_u(1,n)
         y%ships(n)%v = model_v(1,n)
         y%ships(n)%t = model_t(1,n)
         y%ships(n)%q = model_q(1,n)
         y%ships(n)%p = model_psfc(n)
      end do
      deallocate (model_u)
      deallocate (model_v)
      deallocate (model_t)
      deallocate (model_q)
      deallocate (model_psfc)
   else if (sfc_assi_options == sfc_assi_options_2) then
      ! [2.0] Surface assmiilation approach 2
      call da_transform_xtopsfc(grid,iv,ships,iv%ships(:),y%ships(:))
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_ships")

end subroutine da_transform_xtoy_ships


