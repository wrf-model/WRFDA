subroutine da_transform_xtoy_radar_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   !------------------------------------------------------------------------
   ! This subroutine is the adjoint of Doppler radar observation operators.
   !------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.
   integer :: k  ! Index dimension.
   integer :: num_levs

   real    :: model_p(max_ob_levels)    ! Model value p at ob location.
   real    :: model_u(max_ob_levels)    ! Model value u at ob location.
   real    :: model_v(max_ob_levels)    ! Model value v at ob location.
   real    :: model_w(max_ob_levels)    ! Model value w at ob location.
   real    :: model_qrn(max_ob_levels)  ! Model qrn at ob location.
   real    :: model_qrnb(max_ob_levels) ! Model qrn at ob location.
   real    :: model_ps

   real    :: xr,yr,zr

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar_adj")

   ! W_HALF is vertical velocity at half-sigma levels.

   do n=iv%plocal(iv%time-1,radar) + 1, iv%plocal(iv%time,radar)
      do k = 1, max_ob_levels
         model_u(k) = 0.0
         model_v(k) = 0.0
         model_w(k) = 0.0
         model_qrn(k) = 0.0
      end do

      num_levs = iv % radar(n) % info % levels

      model_ps = iv%radar(n)%model_ps

      ! [1.7] Calculate rv and rf at OBS location

      xr = grid%xb%ds * (iv%radar(n)%loc%x - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds * (iv%radar(n)%loc%y - iv%radar(n)%stn_loc%y)

      do k = 1, num_levs

         model_qrnb(k) = iv%radar(n)%model_qrn(k)
         model_p(k)    = iv%radar(n)%model_p(k)

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
              iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rf) then
               if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                 call da_reflectivity_adj(jo_grad_y%radar(n)%rf(k), &
                    model_qrn(k), model_qrnb(k))
               end if
            end if

            if (use_radar_rv) then
               if (iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                  zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                  call da_radial_velocity_adj(jo_grad_y%radar(n)%rv(k), &
                     model_p(k), model_u(k), model_v(k), model_w(k),  &
                     model_qrn(k), model_ps, xr, yr, zr, model_qrnb(k))
               end if
            end if
         end if

         jo_grad_y%radar(n)%rv(k) = 0.0
         jo_grad_y%radar(n)%rf(k) = 0.0

      end do

      ! [1.6] Interpolate horizontally from crs points:

      call da_interp_lin_3d_adj(jo_grad_x % wh,iv%radar(n)%loc, &
         model_w, num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d_adj(jo_grad_x % qrn, iv%radar(n)%loc, &
         model_qrn, num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d_adj(jo_grad_x % v, iv%radar(n)%loc, &
         model_v, num_levs, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d_adj(jo_grad_x % u, iv%radar(n)%loc, &
         model_u, num_levs, iv%radar(n)%zk, num_levs)
   end do

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar_adj")

end subroutine da_transform_xtoy_radar_adj


