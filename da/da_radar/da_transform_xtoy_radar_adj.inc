subroutine da_transform_xtoy_radar_adj(xb, iv, xp, jo_grad_y, jo_grad_x)

   !------------------------------------------------------------------------
   ! This subroutine is the adjoint of Doppler radar observation operators.
   !------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j, k  ! Index dimension.
   integer                       :: num_levs
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   real, dimension(max_ob_levels)   :: model_p  ! Model value p at ob location.
   real, dimension(max_ob_levels)   :: model_u  ! Model value u at ob location.
   real, dimension(max_ob_levels)   :: model_v  ! Model value v at ob location.
   real, dimension(max_ob_levels)   :: model_w  ! Model value w at ob location.
   real, dimension(max_ob_levels)   :: model_rho ! Model rho at ob location.
   real, dimension(max_ob_levels)   :: model_qrn ! Model qrn at ob location.
   real, dimension(max_ob_levels)   :: model_qrnb! Model qrn at ob location.
   real                             :: model_ps

   real                          :: xr,yr,zr

   ! W_HALF is vertical velocity at half-sigma levels.

   if (iv%num_Radar > 0) then
      do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, &
         iv%ob_numb(iv%current_ob_time)%radar
         do k = 1, max_ob_levels
            model_u(k) = 0.
            model_v(k) = 0.
            model_w(k) = 0.
            model_qrn(k) = 0.
         end do

         num_levs = iv % Radar(n) % info % levels

         i = iv%Radar(n)%loc%i
         j = iv%Radar(n)%loc%j
         dx = iv%Radar(n)%loc%dx
         dy = iv%Radar(n)%loc%dy
         dxm = iv%Radar(n)%loc%dxm
         dym = iv%Radar(n)%loc%dym

         model_ps = iv%Radar(n)%model_ps

         ! [1.7] Calculate rv and rf at OBS location

         xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
         yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

         do k = 1, num_levs

            model_qrnb(k) = iv%Radar(n)%model_qrn(k)
            model_p(k)    = iv%Radar(n)%model_p(k)

            if (iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
                 iv % Radar(n) % height_qc(k) /= above_model_lid) then

               if (use_Radar_rf) then
                  if (iv % Radar(n) % rf(k) % qc >= obs_qc_pointer) then
                    call da_reflectivity_adj(jo_grad_y%Radar(n)%rf(k), &
                         model_rho(k), model_qrn(k), model_qrnb(k))
                  end if
               end if

               if (use_Radar_rv) then
                  if (iv % Radar(n) % rv(k) % qc >= obs_qc_pointer) then
                     zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                     call da_radial_velocity_adj(jo_grad_y%Radar(n)%rv(k), &
                        model_p(k), model_u(k), model_v(k), model_w(k),  &
                        model_qrn(k), model_ps, xr, yr, zr, model_qrnb(k))
                  end if
               end if
            end if

            jo_grad_y%Radar(n)%rv(k) = 0.0
            jo_grad_y%Radar(n)%rf(k) = 0.0

         end do


         ! [1.6] Interpolate horizontally from crs points:

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d_adj(jo_grad_x % wh, xp,   &
                             i, j, dx, dy, dxm, dym, &
                             model_w, num_levs, iv%Radar(n)%zk, &
                             num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % qrn, xp, &
                             i, j, dx, dy, dxm, dym, &
                             model_qrn, num_levs, iv%Radar(n)%zk, &
                             num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % v, xp, i, j, dx, dy, &
            dxm, dym, model_v, num_levs, iv%Radar(n)%zk, num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % u, xp, i, j, dx, dy, &
            dxm, dym, model_u, num_levs, iv%Radar(n)%zk, num_levs)
#else
         call da_interp_lin_3d_adj(jo_grad_x % wh(xp%ims,xp%jms,xp%kms), &
            xp, i, j, dx, dy, dxm, dym, model_w(1), &
            num_levs, iv%Radar(n)%zk(1), num_levs)
         call da_interp_lin_3d_adj(jo_grad_x%qrn(xp%ims,xp%jms,xp%kms), & 
                                 xp, i, j, dx, dy, dxm, dym, model_qrn(1), &
                                 num_levs, iv%Radar(n)%zk(1), num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, &
            i, j, dx, dy, dxm, dym, &
            model_v(1), num_levs, iv%Radar(n)%zk(1), &
            num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, &
            i, j, dx, dy, dxm, dym, &
            model_u(1), num_levs, iv%Radar(n)%zk(1), num_levs)
#endif
      end do
   end if

end subroutine da_transform_xtoy_radar_adj


