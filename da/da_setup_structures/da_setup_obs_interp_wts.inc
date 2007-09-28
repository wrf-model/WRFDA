subroutine da_setup_obs_interp_wts (iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)  :: iv         ! Innovation vector (O-B).

   integer                        :: i, n          ! Loop counter.

   if (trace_use) call da_trace_entry("da_setup_obs_interp_wts")

   do n=1, iv % nlocal(sound)
      call da_store_obs_grid_info ( iv%sound(n)%loc)
      call da_store_obs_grid_info ( iv%sonde_sfc(n)%loc)
   end do

   do n=1, iv % nlocal(synop)
      call da_store_obs_grid_info ( iv%synop(n)%loc)
   end do

   do n=1, iv % nlocal(geoamv)
      call da_store_obs_grid_info ( iv%geoamv(n)%loc)
   end do

   do n=1, iv % nlocal(polaramv)
      call da_store_obs_grid_info ( iv%polaramv(n)%loc)
   end do

   do n=1, iv % nlocal(airep)
      call da_store_obs_grid_info ( iv%airep(n)%loc)
   end do

   do n=1, iv % nlocal(metar)
      call da_store_obs_grid_info ( iv%metar(n)%loc)
   end do

   do n=1, iv % nlocal(ships)
      call da_store_obs_grid_info ( iv%ships(n)%loc)
   end do

   do n=1, iv % nlocal(gpspw)
      call da_store_obs_grid_info ( iv%gpspw(n)%loc)
   end do

   do n=1, iv % nlocal(gpsref)
      call da_store_obs_grid_info ( iv%gpsref(n)%loc)
   end do

   do n=1, iv % nlocal(ssmi_tb)
      call da_store_obs_grid_info ( iv%ssmi_tb(n)%loc)
   end do

   do n=1, iv % nlocal(ssmi_rv)
      call da_store_obs_grid_info ( iv%ssmi_rv(n)%loc)
   end do

   do n=1, iv % nlocal(pilot)
      call da_store_obs_grid_info ( iv%pilot(n)%loc)
   end do

   do n=1, iv % nlocal(radar)
      call da_store_obs_grid_info ( iv%radar(n)%loc)
   end do

   do n=1, iv % nlocal(pseudo)
      call da_store_obs_grid_info ( iv%pseudo(n)%loc)
   end do

   do n=1, iv % nlocal(satem)
      call da_store_obs_grid_info ( iv%satem(n)%loc)
   end do

   do n=1, iv % nlocal(ssmt1)
      call da_store_obs_grid_info ( iv%ssmt1(n)%loc)
   end do

   do n=1, iv % nlocal(ssmt2)
      call da_store_obs_grid_info ( iv%ssmt2(n)%loc)
   end do
   
   do n=1, iv % nlocal(qscat)
      call da_store_obs_grid_info ( iv%qscat(n)%loc)
   end do

   do n=1, iv % nlocal(profiler)
      call da_store_obs_grid_info ( iv%profiler(n)%loc)
   end do

   do n=1, iv % nlocal(buoy)
      call da_store_obs_grid_info ( iv%buoy(n)%loc)
   end do

   do n=1, iv % nlocal(bogus)
      call da_store_obs_grid_info ( iv%bogus(n)%loc)
   end do

   do n=1, iv % nlocal(airsr)
      call da_store_obs_grid_info ( iv%airsr(n)%loc)
   end do

   do i = 1, iv % num_inst
      if (iv % instid(i) % num_rad < 1) cycle
      call da_store_obs_grid_info_new (iv % instid(i) % loc_i(:), &
         iv % instid(i) % loc_j(:), iv%instid(i)%proc_domain(:))
   end do

   if (trace_use) call da_trace_exit("da_setup_obs_interp_wts")

end subroutine da_setup_obs_interp_wts


