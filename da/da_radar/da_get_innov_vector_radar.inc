subroutine da_get_innov_vector_radar ( it, xb, xp, ob, iv )

   implicit none

   integer, intent(in)            :: it       ! External iteration.
   type (xb_type), intent(in)     :: xb       ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type (y_type),  intent(inout)  :: ob       ! Observation structure.
   type (ob_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs ! Number of obs levels.
   real                           :: dx, dxm  ! Interpolation weights.
   real                           :: dy, dym  ! Interpolation weights.

   real, dimension(max_ob_levels) :: model_p   ! Model value p at ob location.
   real, dimension(max_ob_levels) :: model_u   ! Model value u at ob location.
   real, dimension(max_ob_levels) :: model_v   ! Model value v at ob location.
   real, dimension(max_ob_levels) :: model_w   ! Model value w at ob location.
   real, dimension(max_ob_levels) :: model_rho ! Model value rho at ob location.
   real, dimension(max_ob_levels) :: model_qrn ! Model value qrn at ob location.

   real, dimension(max_ob_levels) :: model_rv ! Model value rv at ob location.
   real, dimension(max_ob_levels) :: model_rf ! Model value rf at ob location.

   real, dimension(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.

   real                           :: model_ps

   real                           :: xr,yr,zr
   integer                        :: irv, irvf
   integer                        :: irf, irff


   if ( iv % num_Radar < 1 ) return

   irv = 0; irvf = 0; irf = 0; irff = 0

   do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, iv%ob_numb(iv%current_ob_time)%radar

      num_levs = iv % Radar(n) % info % levels

      if ( num_levs < 1 ) cycle

      model_p(:) = 0.0
      model_u(:) = 0.0
      model_v(:) = 0.0
      model_w(:) = 0.0
      model_rho(:) = 0.0
      model_qrn(:) = 0.0

      ! [1.0] Get horizontal interpolation weights:

      i = iv%Radar(n)%loc%i
      j = iv%Radar(n)%loc%j
      dx = iv%Radar(n)%loc%dx
      dy = iv%Radar(n)%loc%dy
      dxm = iv%Radar(n)%loc%dxm
      dym = iv%Radar(n)%loc%dym

      do k=xp%kts,xp%kte
         v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
      end do

      do k=1, num_levs
         call da_to_zk(iv%Radar(n)%height(k), v_h, xp, v_interp_h, iv%Radar(n)%zk(k))

         if ( iv%Radar(n)%zk(k) < 1.0 ) then
            iv%Radar(n)%height_qc(k) = below_model_surface
         else if ( iv%Radar(n)%zk(k) > mkz ) then
            iv%Radar(n)%height_qc(k) = above_model_lid
         end if
      end do


      ! [2.0] Interpolate horizontally to ob points:

#ifndef DEREF_KLUDGE

      call da_interp_lin_3d(xb % p, xp, i, j, dx, dy, dxm, dym, &
                         model_p, max_ob_levels, iv%Radar(n)%zk, num_levs )

      call da_interp_lin_3d(xb % u, xp, i, j, dx, dy, dxm, dym, &
                         model_u, max_ob_levels, iv%Radar(n)%zk, num_levs )

      call da_interp_lin_3d(xb % v, xp, i, j, dx, dy, dxm, dym, &
                         model_v, max_ob_levels, iv%Radar(n)%zk, num_levs )

      call da_interp_lin_3d(xb % wh, xp, i, j, dx, dy, dxm, dym, &
                         model_w, max_ob_levels, iv%Radar(n)%zk, num_levs )

      call da_interp_lin_3d(xb % rho, xp, i, j, dx, dy, dxm, dym, &
                         model_rho, max_ob_levels, iv%Radar(n)%zk, num_levs )

      call da_interp_lin_3d(xb % qrn, xp, i, j, dx, dy, dxm, dym, &
                         model_qrn, max_ob_levels, iv%Radar(n)%zk, num_levs )

#else

      call da_interp_lin_3d(xb%p(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_p(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

      call da_interp_lin_3d(xb%u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_u(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

      call da_interp_lin_3d(xb%v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_v(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

      call da_interp_lin_3d(xb%wh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_w(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

      call da_interp_lin_3d(xb%rho(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_rho(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

      call da_interp_lin_3d(xb%qrn(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
         dxm, dym, model_qrn(1), max_ob_levels, iv%Radar(n)%zk(1), num_levs )

#endif

      ! Test 5.E-5 as critical value. It can not be smaller.

      do k=1,num_levs
         model_qrn(k)=amax1(5.E-5,model_qrn(k))
      end do

      model_ps = dxm * (dym * xb % psac(i,  j) + dy * xb%psac(i+1,  j)) + &
                 dx  * (dym * xb % psac(i,j+1) + dy * xb%psac(i+1,j+1)) + &
                 xb % ptop

      iv%Radar(n)%model_p(1:num_levs) = model_p(1:num_levs)
      iv%Radar(n)%model_rho(1:num_levs) = model_rho(1:num_levs)
      iv%Radar(n)%model_qrn(1:num_levs) = model_qrn(1:num_levs)
      iv%Radar(n)%model_ps = model_ps

      ! [3.0] Calculate rv, rf at OBS location and initialise components of &
      ! innovation vector:

      if(fg_format == fg_format_wrf) then
         call da_latlon_to_ij(map_info, &
                           iv%Radar(n)%stn_loc%lat, iv%Radar(n)%stn_loc%lon, &
                           iv%Radar(n)%stn_loc%x,   iv%Radar(n)%stn_loc%y )
      else
         call da_llxy ( iv%Radar(n)%stn_loc%lat, iv%Radar(n)%stn_loc%lon, &
                     iv%Radar(n)%stn_loc%x,   iv%Radar(n)%stn_loc%y )
      end if

      xr = xb%ds * (iv%Radar(n)%loc%x - iv%Radar(n)%stn_loc%x)
      yr = xb%ds * (iv%Radar(n)%loc%y - iv%Radar(n)%stn_loc%y)

      model_rv(:) = 0.0
      model_rf(:) = 0.0

      do k=1, num_levs
         iv % Radar(n) % rv(k) % inv = 0.0
         iv % Radar(n) % rf(k) % inv = 0.0

         if ( iv % Radar(n) % height_qc(k) /= below_model_surface .and.  &
              iv % Radar(n) % height_qc(k) /= above_model_lid ) then

            if (use_Radar_rv) then
               if ( ABS(iv % Radar(n) % rv(k) % qc - missing_data) > 1 ) then
                  if ( ABS(ob % Radar(n) % rv(k) - missing_r) > 1. .AND. &
                       iv % Radar(n) % rv(k) % qc >= obs_qc_pointer ) then

                     zr=iv%Radar(n)%height(k) - iv%Radar(n)%stn_loc%elv

                     call da_radial_velocity(model_rv(k), model_p(k),  &
                        model_u(k), model_v(k), model_w(k),          &
                        model_qrn(k), model_ps, xr, yr, zr)

                     iv % Radar(n) % rv(k) % inv = ob % Radar(n) % rv(k) - &
                                              model_rv(k)
                  end if
               end if
            end if

            if (use_Radar_rf) then
               if ( ABS(iv % Radar(n) % rf(k) % qc - missing_data) > 1 ) then
                  if ( ABS(ob % Radar(n) % rf(k) - missing_r) > 1. .AND. &
                     iv % Radar(n) % rf(k) % qc >= obs_qc_pointer ) then
                     call da_reflectivity( model_rf(k), model_rho(k), &
                        model_qrn(k) )

                     iv % Radar(n) % rf(k) % inv = ob % Radar(n) % rf(k) - &
                                              model_rf(k)
                  end if
               end if
            end if
         end if
      end do

      !------------------------------------------------------------------------
      ! [4.0] Perform optional maximum error check:  
      !------------------------------------------------------------------------

      if (check_max_iv )  then
         call da_check_max_iv_radar(it, iv % Radar(n), irv, irf, irvf, irff )
      end if
   end do

   if (check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(/,A,i5,A)')   &
         'For outer iteration ', it, ', Total Rejections for Radar follows:'

      if ( use_Radar_rv ) then
          write ( unit = check_max_iv_unit, fmt = '(/,2(A,I6))' ) &
            'Number of failed rv observations:     ',irvf, ' on ',irv
      end if

      if ( use_Radar_rf ) then
         write ( unit = check_max_iv_unit, fmt = '(/,2(A,I6))' ) &
            'Number of failed rf observations:     ',irff, ' on ',irf
      end if
   end if

end subroutine da_get_innov_vector_radar


