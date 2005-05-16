SUBROUTINE da_transform_xtoy_Radar_Adj( xb, iv, xp, jo_grad_y, jo_grad_x )

!---------------------------------------------------------------------------
!  This subroutine is the adjoint of Doppler radar observation operators.
!  Qingnong Xiao, September 2002.
!  Modified, Jianfeng Gu, April 2004.
!  Multi Radar Obs. Jianfeng Gu, Sept. 2004.
!---------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(in)    :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)    :: iv          ! obs. inc vector (o-b).
   TYPE (xpose_type), INTENT(IN) :: xp          ! Domain decomposition vars.
   TYPE (y_type) , INTENT(INOUT) :: jo_grad_y   ! grad_y(jo)
   TYPE (x_type) , INTENT(INOUT) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j, k  ! Index dimension.
   INTEGER                       :: num_levs
   INTEGER                       :: max_num_levs
   REAL                          :: dx, dxm  ! 
   REAL                          :: dy, dym  !
   REAL                          :: dz, dzm  !

   REAL, DIMENSION(max_ob_levels)   :: model_p  ! Model value p at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_u  ! Model value u at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_v  ! Model value v at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_w  ! Model value w at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_rho ! Model rho at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_rhob! Model rho at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_qrn ! Model qrn at ob location.
   REAL, DIMENSION(max_ob_levels)   :: model_qrnb! Model qrn at ob location.
   REAL                             :: model_ps

   REAL                          :: xr,yr,zr

!  W_HALF is vertical velocity at half-sigma levels.

   IF ( iv%num_Radar > 0 ) THEN

      do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, iv%ob_numb(iv%current_ob_time)%radar

       if(iv%Radar(n)%loc%proc_domain_with_halo) then

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

!        [1.7] Calculate rv and rf at OBS location

         xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
         yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

         do k = 1, num_levs

            model_qrnb(k) = iv%Radar(n)%model_qrn(k)
            model_p(k)    = iv%Radar(n)%model_p(k)

            IF ( iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
                 iv % Radar(n) % height_qc(k) /= above_model_lid ) THEN

             IF ( USE_Radar_rf ) THEN
             
              IF ( iv % Radar(n) % rf(k) % qc >= obs_qc_pointer ) THEN

                call da_reflectivity_Adj( jo_grad_y%Radar(n)%rf(k), &
                     model_rho(k), model_qrn(k), model_rhob(k), model_qrnb(k) )

              ENDIF
             ENDIF

             IF ( USE_Radar_rv ) THEN

              IF ( iv % Radar(n) % rv(k) % qc >= obs_qc_pointer ) THEN

                zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                call da_radial_velocity_Adj( jo_grad_y%Radar(n)%rv(k), &
                     model_p(k), model_u(k), model_v(k), model_w(k),  &
                     model_qrn(k), model_ps, xr, yr, zr, model_qrnb(k) )

              ENDIF
             ENDIF

            ENDIF

            jo_grad_y%Radar(n)%rv(k) = 0.0
            jo_grad_y%Radar(n)%rf(k) = 0.0

         enddo


!        [1.6] Interpolate horizontally from crs points:

#ifndef DEREF_KLUDGE
         call Interp_Lin_3D_Adj(jo_grad_x % wh, xp,   &
                            i, j, dx, dy, dxm, dym, &
                            model_w, num_levs, iv%Radar(n)%zk, &
                            num_levs )
         call Interp_lin_3D_Adj(jo_grad_x % qrn, xp, &
                            i, j, dx, dy, dxm, dym, &
                            model_qrn, num_levs, iv%Radar(n)%zk, &
                            num_levs)
         call Interp_Lin_3D_Adj(jo_grad_x % v, xp, i, j, dx, dy, dxm, dym, &
                             model_v, num_levs, iv%Radar(n)%zk, num_levs)
         call Interp_Lin_3D_Adj(jo_grad_x % u, xp, i, j, dx, dy, dxm, dym, &
                             model_u, num_levs, iv%Radar(n)%zk, num_levs)
#else
         call Interp_Lin_3D_Adj(jo_grad_x % wh(xp%ims,xp%jms,xp%kms), xp,  &
                                i, j, dx, dy, dxm, dym, model_w(1), &
                                num_levs, iv%Radar(n)%zk(1), num_levs )
         call Interp_Lin_3D_Adj(jo_grad_x%qrn(xp%ims,xp%jms,xp%kms), & 
                                xp, i, j, dx, dy, dxm, dym, model_qrn(1), &
                                num_levs, iv%Radar(n)%zk(1), num_levs )
         call Interp_Lin_3D_Adj(jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, &
                             i, j, dx, dy, dxm, dym, &
                             model_v(1), num_levs, iv%Radar(n)%zk(1), num_levs)
         call Interp_Lin_3D_Adj(jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, &
                             i, j, dx, dy, dxm, dym, &
                             model_u(1), num_levs, iv%Radar(n)%zk(1), num_levs)
#endif
       endif

      END DO

   END IF

END SUBROUTINE da_transform_xtoy_Radar_Adj
