subroutine da_transform_xtoy_radar ( xa, xb, iv, xp, y )

   !---------------------------------------------------------------------
   ! This subroutine is to calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   !---------------------------------------------------------------------
 
   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   type (xb_type), intent(in)   :: xb          ! first guess state.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa) (linear)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   INTEGER                      :: num_levs ! Number of obs levels.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !

   REAL, DIMENSION(max_ob_levels)  :: model_p  ! Model value p at ob location.
   REAL, DIMENSION(max_ob_levels)  :: model_u  ! Model value u at ob location.
   REAL, DIMENSION(max_ob_levels)  :: model_v  ! Model value v at ob location.
   REAL, DIMENSION(max_ob_levels)  :: model_w  ! Model value w at ob location.
   REAL, DIMENSION(max_ob_levels)  :: model_qrn ! Model qrn at ob location.
   REAL, DIMENSION(max_ob_levels)  :: model_qrnb! Model qrn at ob location.
   REAL                            :: model_ps

   REAL                         :: xr,yr,zr

   IF ( iv%num_Radar > 0 ) THEN
      do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, &
         iv%ob_numb(iv%current_ob_time)%radar
         if(iv%Radar(n)%loc%proc_domain_with_halo) then
            num_levs = iv % Radar(n) % info % levels

            ! [1.3] Get dot pt. horizontal interpolation weights:

            i = iv%Radar(n)%loc%i
            j = iv%Radar(n)%loc%j
            dx = iv%Radar(n)%loc%dx
            dy = iv%Radar(n)%loc%dy
            dxm = iv%Radar(n)%loc%dxm
            dym = iv%Radar(n)%loc%dym

            do k = 1, num_levs
               model_qrnb(k) = iv%Radar(n)%model_qrn(k)
               model_p(k) = iv%Radar(n)%model_p(k)
            end do

            model_ps   = iv%Radar(n)%model_ps

            ! [1.4] Interpolate horizontally from dot points:
#ifndef DEREF_KLUDGE
            call da_interp_lin_3d(xa % u, xp, i, j, dx, dy, dxm, dym, &
                               model_u, num_levs, iv%Radar(n)%zk, &
                               num_levs)
            call da_interp_lin_3d(xa % v, xp, i, j, dx, dy, dxm, dym, &
                               model_v, num_levs, iv%Radar(n)%zk, &
                               num_levs)
            call da_interp_lin_3d(xa % qrn, xp, &
                               i, j, dx, dy, dxm, dym, &
                               model_qrn, num_levs, iv%Radar(n)%zk, &
                               num_levs)
            call da_interp_lin_3d(xa%wh, xp,   &
                               i, j, dx, dy, dxm, dym, &
                               model_w, num_levs, iv%Radar(n)%zk, &
                               num_levs )
#else
            call da_interp_lin_3d(xa % u(xp%ims,xp%jms,xp%kms), xp, &
                               i, j, dx, dy, dxm, dym, &
                               model_u(1), num_levs, iv%Radar(n)%zk(1), &
                               num_levs)
            call da_interp_lin_3d(xa % v(xp%ims,xp%jms,xp%kms), xp, &
                               i, j, dx, dy, dxm, dym, &
                               model_v(1), num_levs, iv%Radar(n)%zk(1), &
                               num_levs)
            call da_interp_lin_3d(xa % qrn(xp%ims,xp%jms,xp%kms), xp, &
                               i, j, dx, dy, dxm, dym, &
                               model_qrn(1), num_levs, iv%Radar(n)%zk(1), &
                               num_levs)
            call da_interp_lin_3d(xa%wh(xp%ims,xp%jms,xp%kms), xp,   &
                               i, j, dx, dy, dxm, dym, &
                               model_w(1), num_levs, iv%Radar(n)%zk(1), &
                               num_levs )
#endif

            ! [1.7] Calculate rv and rf at OBS location

            xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
            yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

            do k = 1, num_levs

               ! y%Radar(n)%rv(k) = 0.0
               ! y%Radar(n)%rf(k) = 0.0

               IF ( iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
                    iv % Radar(n) % height_qc(k) /= above_model_lid ) THEN
                  IF ( USE_Radar_rv ) THEN
                     IF ( iv % Radar(n) % rv(k) % qc >= obs_qc_pointer ) THEN
                        zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                        call da_radial_velocity_lin(y%Radar(n)%rv(k), &
                           model_p(k), &
                           model_u(k), model_v(k), model_w(k), model_qrn(k),    &
                           model_ps, xr, yr, zr, model_qrnb(k) )
                     ENDIF
                  ENDIF

                  IF ( USE_Radar_rf ) THEN
                     IF ( iv % Radar(n) % rf(k) % qc >= obs_qc_pointer ) THEN
                        call da_reflectivity_lin( y%Radar(n)%rf(k), &
                           model_qrn(k), &
                           model_qrnb(k) )
                     ENDIF
                  ENDIF
               ENDIF
            end do
         end if
      end do
   ENDIF

end subroutine da_transform_xtoy_radar 

