subroutine da_setup_obs_interp_wts (xp, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)  :: xp         ! Domain decomposition vars.
   type (ob_type), intent(inout)  :: iv         ! Innovation vector (O-B).

   integer                        :: i, n          ! Loop counter.

   if (trace_use) call da_trace_entry("da_setup_obs_interp_wts")

   do n=1, iv % num_sound
      call da_store_obs_grid_info (xp, iv%sound(n)%loc)
      call da_store_obs_grid_info (xp, iv%sonde_sfc(n)%loc)
   end do

   do n=1, iv % num_synop
      call da_store_obs_grid_info (xp, iv%synop(n)%loc)
   end do

   do n=1, iv % num_geoamv
      call da_store_obs_grid_info (xp, iv%geoamv(n)%loc)
   end do

   do n=1, iv % num_polaramv
      call da_store_obs_grid_info (xp, iv%polaramv(n)%loc)
   end do

   do n=1, iv % num_airep
      call da_store_obs_grid_info (xp, iv%airep(n)%loc)
   end do

   do n=1, iv % num_metar
      call da_store_obs_grid_info (xp, iv%metar(n)%loc)
   end do

   do n=1, iv % num_ships
      call da_store_obs_grid_info (xp, iv%ships(n)%loc)
   end do

   do n=1, iv % num_gpspw
      call da_store_obs_grid_info (xp, iv%gpspw(n)%loc)
   end do

   do n=1, iv % num_gpsref
      call da_store_obs_grid_info (xp, iv%gpsref(n)%loc)
   end do

   do n=1, iv % num_ssmi_tb
      call da_store_obs_grid_info (xp, iv%ssmi_tb(n)%loc)
   end do

   do n=1, iv % num_ssmi_retrieval
      call da_store_obs_grid_info (xp, iv%ssmi_retrieval(n)%loc)
   end do

   do n=1, iv % num_pilot
      call da_store_obs_grid_info (xp, iv%pilot(n)%loc)
   end do

   do n=1, iv % num_Radar
      call da_store_obs_grid_info (xp, iv%Radar(n)%loc)
   end do

   do n=1, iv % num_pseudo
      call da_store_obs_grid_info (xp, iv%pseudo(n)%loc)
   end do

   do n=1, iv % num_satem
      call da_store_obs_grid_info (xp, iv%satem(n)%loc)
   end do

   do n=1, iv % num_ssmt1
      call da_store_obs_grid_info (xp, iv%ssmt1(n)%loc)
   end do

   do n=1, iv % num_ssmt2
      call da_store_obs_grid_info (xp, iv%ssmt2(n)%loc)
   end do
   
   do n=1, iv % num_qscat
      call da_store_obs_grid_info (xp, iv%qscat(n)%loc)
   end do

   do n=1, iv % num_profiler
      call da_store_obs_grid_info (xp, iv%profiler(n)%loc)
   end do

   do n=1, iv % num_buoy
      call da_store_obs_grid_info (xp, iv%buoy(n)%loc)
   end do

   do n=1, iv % num_bogus
      call da_store_obs_grid_info (xp, iv%bogus(n)%loc)
   end do

   do n=1, iv % num_airsr
      call da_store_obs_grid_info (xp, iv%airsr(n)%loc)
   end do

   do i = 1, iv % num_inst
      if (iv % instid(i) % num_rad < 1) cycle
      do n=1, iv % instid(i) % num_rad
         call da_store_obs_grid_info (xp, iv % instid(i) % loc(n))
      end do
   end do

   if (trace_use) call da_trace_exit("da_setup_obs_interp_wts")

end subroutine da_setup_obs_interp_wts


