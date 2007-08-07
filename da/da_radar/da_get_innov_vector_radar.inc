subroutine da_get_innov_vector_radar (it, xb, xp, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(xb_type),    intent(in)    :: xb       ! first guess state.
   type(xpose_type), intent(in)    :: xp       ! Domain decomposition vars.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(ob_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real    :: model_p  (max_ob_levels) ! Model value p at ob location.
   real    :: model_u  (max_ob_levels) ! Model value u at ob location.
   real    :: model_v  (max_ob_levels) ! Model value v at ob location.
   real    :: model_w  (max_ob_levels) ! Model value w at ob location.
   real    :: model_rho(max_ob_levels) ! Model value rho at ob location.
   real    :: model_qrn(max_ob_levels) ! Model value qrn at ob location.

   real    :: model_rv(max_ob_levels) ! Model value rv at ob location.
   real    :: model_rf(max_ob_levels) ! Model value rf at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   real    :: model_ps

   real    :: xr,yr,zr
   integer :: irv, irvf
   integer :: irf, irff
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_radar")

   irv = 0; irvf = 0; irf = 0; irff = 0

   do n=iv%ob_numb(iv%current_ob_time-1)%radar + 1, iv%ob_numb(iv%current_ob_time)%radar

      num_levs = iv % radar(n) % info % levels

      if (num_levs < 1) cycle

      model_p(:) = 0.0
      model_u(:) = 0.0
      model_v(:) = 0.0
      model_w(:) = 0.0
      model_rho(:) = 0.0
      model_qrn(:) = 0.0

      ! [1.0] Get horizontal interpolation weights:

      i = iv%radar(n)%loc%i
      j = iv%radar(n)%loc%j
      dx = iv%radar(n)%loc%dx
      dy = iv%radar(n)%loc%dy
      dxm = iv%radar(n)%loc%dxm
      dym = iv%radar(n)%loc%dym

      do k=kts,kte
         v_h(k) = dym*(dxm*xb%h(i,j,k)+dx*xb%h(i+1,j,k)) + dy*(dxm*xb%h(i,j+1,k)+dx*xb%h(i+1,j+1,k))
      end do

      do k=1, num_levs
         call da_to_zk(iv%radar(n)%height(k), v_h, xp, v_interp_h, iv%radar(n)%zk(k))

         if (iv%radar(n)%zk(k) < 1.0) then
            iv%radar(n)%height_qc(k) = below_model_surface
         else if (iv%radar(n)%zk(k) > mkz) then
            iv%radar(n)%height_qc(k) = above_model_lid
         end if
      end do


      ! [2.0] Interpolate horizontally to ob points:

      call da_interp_lin_3d(xb % p,   iv%radar(n)%loc, model_p,   max_ob_levels, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(xb % u,   iv%radar(n)%loc, model_u,   max_ob_levels, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(xb % v,   iv%radar(n)%loc, model_v,   max_ob_levels, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(xb % wh,  iv%radar(n)%loc, model_w,   max_ob_levels, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(xb % rho, iv%radar(n)%loc, model_rho, max_ob_levels, iv%radar(n)%zk, num_levs)
      call da_interp_lin_3d(xb % qrn, iv%radar(n)%loc, model_qrn, max_ob_levels, iv%radar(n)%zk, num_levs)

      ! Test 5.0E-5 as critical value. It can not be smaller.

      do k=1,num_levs
         model_qrn(k)=amax1(5.0E-5,model_qrn(k))
      end do

      model_ps = dxm *(dym * xb % psac(i,  j) + dy * xb%psac(i+1,  j)) + &
                 dx  *(dym * xb % psac(i,j+1) + dy * xb%psac(i+1,j+1)) + &
                 xb % ptop

      iv%radar(n)%model_p(1:num_levs) = model_p(1:num_levs)
      iv%radar(n)%model_rho(1:num_levs) = model_rho(1:num_levs)
      iv%radar(n)%model_qrn(1:num_levs) = model_qrn(1:num_levs)
      iv%radar(n)%model_ps = model_ps

      ! [3.0] Calculate rv, rf at OBS location and initialise components of &
      ! innovation vector:

      if (fg_format == fg_format_wrf) then
         call da_latlon_to_ij(map_info, &
            iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      else
         call da_llxy( iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      end if

      xr = xb%ds *(iv%radar(n)%loc%x - iv%radar(n)%stn_loc%x)
      yr = xb%ds *(iv%radar(n)%loc%y - iv%radar(n)%stn_loc%y)

      model_rv(:) = 0.0
      model_rf(:) = 0.0

      do k=1, num_levs
         iv % radar(n) % rv(k) % inv = 0.0
         iv % radar(n) % rf(k) % inv = 0.0

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
             iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rv) then
               if (abs(iv % radar(n) % rv(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rv(k) - missing_r) > 1.0 .AND. &
                       iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then

                     zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                     call da_radial_velocity(model_rv(k), model_p(k),  &
                        model_u(k), model_v(k), model_w(k),          &
                        model_qrn(k), model_ps, xr, yr, zr)

                     iv % radar(n) % rv(k) % inv = ob % radar(n) % rv(k) - &
                                              model_rv(k)
                  end if
               end if
            end if

            if (use_radar_rf) then
               if (abs(iv % radar(n) % rf(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rf(k) - missing_r) > 1.0 .AND. &
                     iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     call da_reflectivity( model_rf(k), model_rho(k), &
                        model_qrn(k))

                     iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - &
                                              model_rf(k)
                  end if
               end if
            end if
         end if
      end do

      !------------------------------------------------------------------------
      ! [4.0] Perform optional maximum error check:  
      !------------------------------------------------------------------------

      if (check_max_iv)  then
         call da_check_max_iv_radar(it, iv % radar(n), irv, irf, irvf, irff)
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(/,A,i5,A)')   &
         'For outer iteration ', it, ', Total Rejections for radar follows:'

      if (use_radar_rv) then
          write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rv observations:     ',irvf, ' on ',irv
      end if

      if (use_radar_rf) then
         write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rf observations:     ',irff, ' on ',irf
      end if
   end if
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_radar")

end subroutine da_get_innov_vector_radar


