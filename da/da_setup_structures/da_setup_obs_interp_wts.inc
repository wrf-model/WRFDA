subroutine da_setup_obs_interp_wts ( xp, iv )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN)  :: xp         ! Domain decomposition vars.
   TYPE (ob_type), INTENT(INOUT)  :: iv         ! Innovation vector (O-B).

   INTEGER                        :: inst, n          ! Loop counter.

   IF (trace_use) call da_trace_entry("da_setup_obs_interp_wts")


   do n=1, iv % num_sound
      call da_store_obs_grid_info( xp, iv%sound(n)%loc )
      call da_store_obs_grid_info( xp, iv%sonde_sfc(n)%loc )
   enddo

   do n=1, iv % num_synop
      call da_store_obs_grid_info( xp, iv%synop(n)%loc )
   enddo

   do n=1, iv % num_geoamv
      call da_store_obs_grid_info( xp, iv%geoamv(n)%loc )
   enddo

   do n=1, iv % num_polaramv
      call da_store_obs_grid_info( xp, iv%polaramv(n)%loc )
   enddo

   do n=1, iv % num_airep
      call da_store_obs_grid_info( xp, iv%airep(n)%loc )
   enddo

   do n=1, iv % num_metar
      call da_store_obs_grid_info( xp, iv%metar(n)%loc )
   enddo

   do n=1, iv % num_ships
      call da_store_obs_grid_info( xp, iv%ships(n)%loc )
   enddo

   do n=1, iv % num_gpspw
      call da_store_obs_grid_info( xp, iv%gpspw(n)%loc )
   enddo

   do n=1, iv % num_gpsref
      call da_store_obs_grid_info( xp, iv%gpsref(n)%loc )
   enddo

   do n=1, iv % num_ssmi_tb
      call da_store_obs_grid_info( xp, iv%ssmi_tb(n)%loc )
   enddo

   do n=1, iv % num_ssmi_retrieval
      call da_store_obs_grid_info( xp, iv%ssmi_retrieval(n)%loc )
   enddo

   do n=1, iv % num_pilot
      call da_store_obs_grid_info( xp, iv%pilot(n)%loc )
   enddo

   do n=1, iv % num_Radar
      call da_store_obs_grid_info( xp, iv%Radar(n)%loc )
   enddo

   do n=1, iv % num_pseudo
      call da_store_obs_grid_info( xp, iv%pseudo(n)%loc )
   enddo

   do n=1, iv % num_satem
      call da_store_obs_grid_info( xp, iv%satem(n)%loc )
   enddo

   do n=1, iv % num_ssmt1
      call da_store_obs_grid_info( xp, iv%ssmt1(n)%loc )
   enddo

   do n=1, iv % num_ssmt2
      call da_store_obs_grid_info( xp, iv%ssmt2(n)%loc )
   enddo
   
   do n=1, iv % num_qscat
      call da_store_obs_grid_info( xp, iv%qscat(n)%loc )
   enddo

   do n=1, iv % num_profiler
      call da_store_obs_grid_info( xp, iv%profiler(n)%loc )
   enddo

   do n=1, iv % num_buoy
      call da_store_obs_grid_info( xp, iv%buoy(n)%loc )
   enddo

   do n=1, iv % num_bogus
      call da_store_obs_grid_info( xp, iv%bogus(n)%loc )
   enddo

   do inst = 1, iv % num_inst
      if (iv % instid(inst) % num_rad < 1) cycle
      do n=1, iv % instid(inst) % num_rad
         call da_store_obs_grid_info( xp, iv % instid(inst) % rad(n)%loc )
      enddo
   end do

   IF (trace_use) call da_trace_exit("da_setup_obs_interp_wts")

end subroutine da_setup_obs_interp_wts

