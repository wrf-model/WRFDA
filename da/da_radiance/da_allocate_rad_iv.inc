subroutine da_llxy_latlon(lat, lon, proj, x, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat
   real, intent(in)             :: lon
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: x
   real, intent(out)            :: y

   real                         :: deltalat
   real                         :: deltalon
   real                         :: lon360
   real                         :: latinc
   real                         :: loninc

   if (trace_use_frequent) call da_trace_entry("da_llxy_latlon")

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   latinc = proj%truelat1
   loninc = proj%stdlon

   ! Compute deltalat and deltalon as the difference between the input 
   ! lat/lon and the origin lat/lon

   deltalat = lat - proj%lat1

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   if (lon < 0) then 
      lon360 = lon + 360.0 
   else 
      lon360 = lon
   end if    
   deltalon = lon360 - proj%lon1      
    
   ! Compute x/y
   x = deltalon/loninc + 1.0
   y = deltalat/latinc + 1.0

   if (trace_use_frequent) call da_trace_exit("da_llxy_latlon")

end subroutine da_llxy_latlon


 max_wave, be % cv % size2c, &
         xbx % lenr, xbx % lenwrk, xbx % lensav, &
         xbx % inc, xbx % alp_size, xbx % alp, &
         xbx % wsave, be%v2%power(0:be % v2 % max_wave,k), &
         cv(cv_s:cv_e), vv%v2(its:ite,jts:jte,k))
      cv_s = cv_e + 1
   end do

   do k = 1, be%v3%mz
      cv_e = cv_s + 2 * be % cv % size3c - 1
      call da_vtovv_spectral_adj(be % v3 % max_wave, be % cv % size3c, &
         xbx % lenr, xbx % lenwrk, xbx % lensav, &
         xbx % inc, xbx % alp_size, xbx % alp, &
         xbx % wsave, be%v3%power(0:be % v3 % max_wave,k), &
         cv(cv_s:cv_e), vv%v3(its:ite,jts:jte,k))
      cv_s = cv_e + 1
   end do

   do k = 1, be%v4%mz
      cv_e = cv_s + 2 * be % cv % size4c - 1
      call da_vtovv_spectral_adj(be % v4 % max_wave, be % cv % size4c, &
         xbx % lenr, xbx % lenwrk, xbx % lensav, &
         xbx % inc, xbx % alp_size, xbx % alp, &
         xbx % wsave, be%v4%power(0:be % v4 % max_wave,k), &
         cv(cv_s:cv_e), vv%v4(its:ite,jts:jte,k))
      cv_s = cv_e + 1
   end do

   do k = 1, be%v5%mz
     cv_e = cv_s + 2 * be % cv % size5c - 1 
     call da_vtovv_spectral_adj(be % v5 % max_wave, be % cv % size5c, &
        xbx % lenr, xbx % lenwrk, xbx % lensav, &
        xbx % inc, xbx % alp_size, xbx % alp, &
        xbx % wsave, be%v5%power(0:be % v5 % max_wave,k), &
        cv(cv_s:cv_e), vv%v5(its:ite,jts:jte,kts:kte))
     cv_s = cv_e + 1
   end do

   !-------------------------------------------------------------------------
   ! [2] Spectral to grid transform for flow-dependent control variables:
   !-------------------------------------------------------------------------

   do m = 1, be % ne
      cv_e = cv_s + 2 * be % cv % size_alphac - 1
      call da_vtovv_spectral_adj(be % alpha % max_wave, be % cv % size_alphac, &
         xbx % lenr, xbx % lenwrk, xbx % lensav, &
         xbx % inc, xbx % alp_size, xbx % alp, &
         xbx % wsave, be % alpha % power(0:be%alpha%max_wave,1), &
         cv(cv_s:cv_e), vv%alpha(its:ite,jts:jte,m))
      cv_s = cv_e + 1
   end do

   if (trace_use) call da_trace_exit("da_transform_vtovv_global_adj")

end subroutine da_transform_vtovv_global_adj


     allocate (iv%instid(i)%tb_xb(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_qc(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_inv(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_error(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%emiss(iv%instid(i)%nchannels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%scanpos(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%scanline(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%ifgat(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%cloud_flag(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%satzen(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%satazi(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%solzen(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%solazi(iv%instid(i)%num_rad))
      if (rtm_option == rtm_option_crtm) then
         allocate(iv%instid(i)%water_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%land_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%ice_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%snow_coverage(iv%instid(i)%num_rad))
         if (use_crtm_kmatrix) then
         !  allocate(iv%instid(i)%ts_jacobian(nchan,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%windspeed_jacobian(nchan,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%emiss_jacobian(nchan,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%t_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%q_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%water_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%ice_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%rain_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%snow_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%graupel_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%hail_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%water_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%ice_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%rain_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%snow_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%graupel_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         !  allocate(iv%instid(i)%hail_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
         end if
      end if

      call da_trace_exit("da_allocate_rad_iv")

end subroutine da_allocate_rad_iv

