subroutine da_transform_xtoy_radar_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   !------------------------------------------------------------------------
   ! This subroutine is the adjoint of Doppler radar observation operators.
   !------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: k  ! Index dimension.

   integer :: n

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qrnb(:,:)
   real, allocatable :: model_ps(:)

   real    :: xr,yr,zr

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar_adj")

   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrnb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   ! W_HALF is vertical velocity at half-sigma levels.

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      model_ps(n) = iv%radar(n)%model_ps  

      ! [1.7] Calculate rv and rf at OBS location

      xr = grid%xb%ds * (iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds * (iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      do k = 1, iv%info(radar)%levels(n)

         model_qrnb(k,n) = iv%radar(n)%model_qrn(k)
         model_p(k,n)    = iv%radar(n)%model_p(k)

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
              iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rf) then
               if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                 call da_reflectivity_adj(jo_grad_y%radar(n)%rf(k), &
                    model_qrn(k,n), model_qrnb(k,n))
               end if
            end if

            if (use_radar_rv) then
               if (iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                  zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                  call da_radial_velocity_adj(jo_grad_y%radar(n)%rv(k), &
                     model_p(k,n), model_u(k,n), model_v(k,n), model_w(k,n),  &
                     model_qrn(k,n), model_ps(n), xr, yr, zr, model_qrnb(k,n))
               end if
            end if
         end if

         jo_grad_y%radar(n)%rv(k) = 0.0
         jo_grad_y%radar(n)%rf(k) = 0.0

      end do
   end do ! n

   ! [1.6] Interpolate horizontally from crs points:

   call da_interp_lin_3d_adj_newest (jo_grad_x % wh,  iv%info(radar), model_w)
   call da_interp_lin_3d_adj_newest (jo_grad_x % qrn, iv%info(radar), model_qrn)
   call da_interp_lin_3d_adj_newest (jo_grad_x % v,   iv%info(radar), model_v)
   call da_interp_lin_3d_adj_newest (jo_grad_x % u,   iv%info(radar), model_u)

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)
   deallocate (model_qrn)
   deallocate (model_qrnb)
   deallocate (model_ps)

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar_adj")

end subroutine da_transform_xtoy_radar_adj


