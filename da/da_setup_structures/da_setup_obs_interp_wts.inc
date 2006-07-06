SUBROUTINE da_setup_obs_interp_wts ( xp, iv )

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN)  :: xp         ! Domain decomposition vars.
   TYPE (ob_type), INTENT(INOUT)  :: iv         ! Innovation vector (O-B).

   INTEGER                        :: inst, n          ! Loop counter.

   IF (trace_use) CALL da_trace_entry("da_setup_obs_interp_wts")


   do n=1, iv % num_sound
      CALL da_store_obs_grid_info( xp, iv%sound(n)%loc )
      CALL da_store_obs_grid_info( xp, iv%sonde_sfc(n)%loc )
   enddo

   do n=1, iv % num_synop
      CALL da_store_obs_grid_info( xp, iv%synop(n)%loc )
   enddo

   do n=1, iv % num_geoamv
      CALL da_store_obs_grid_info( xp, iv%geoamv(n)%loc )
   enddo

   do n=1, iv % num_polaramv
      CALL da_store_obs_grid_info( xp, iv%polaramv(n)%loc )
   enddo

   do n=1, iv % num_airep
      CALL da_store_obs_grid_info( xp, iv%airep(n)%loc )
   enddo

   do n=1, iv % num_metar
      CALL da_store_obs_grid_info( xp, iv%metar(n)%loc )
   enddo

   do n=1, iv % num_ships
      CALL da_store_obs_grid_info( xp, iv%ships(n)%loc )
   enddo

   do n=1, iv % num_gpspw
      CALL da_store_obs_grid_info( xp, iv%gpspw(n)%loc )
   enddo

   do n=1, iv % num_gpsref
      CALL da_store_obs_grid_info( xp, iv%gpsref(n)%loc )
   enddo

   do n=1, iv % num_ssmi_tb
      CALL da_store_obs_grid_info( xp, iv%ssmi_tb(n)%loc )
   enddo

   do n=1, iv % num_ssmi_retrieval
      CALL da_store_obs_grid_info( xp, iv%ssmi_retrieval(n)%loc )
   enddo

   do n=1, iv % num_pilot
      CALL da_store_obs_grid_info( xp, iv%pilot(n)%loc )
   enddo

   do n=1, iv % num_Radar
      CALL da_store_obs_grid_info( xp, iv%Radar(n)%loc )
   enddo

   do n=1, iv % num_pseudo
      CALL da_store_obs_grid_info( xp, iv%pseudo(n)%loc )
   enddo

   do n=1, iv % num_satem
      CALL da_store_obs_grid_info( xp, iv%satem(n)%loc )
   enddo

   do n=1, iv % num_ssmt1
      CALL da_store_obs_grid_info( xp, iv%ssmt1(n)%loc )
   enddo

   do n=1, iv % num_ssmt2
      CALL da_store_obs_grid_info( xp, iv%ssmt2(n)%loc )
   enddo
   
   do n=1, iv % num_qscat
      CALL da_store_obs_grid_info( xp, iv%qscat(n)%loc )
   enddo

   do n=1, iv % num_profiler
      CALL da_store_obs_grid_info( xp, iv%profiler(n)%loc )
   enddo

   do n=1, iv % num_buoy
      CALL da_store_obs_grid_info( xp, iv%buoy(n)%loc )
   enddo

   do n=1, iv % num_bogus
      CALL da_store_obs_grid_info( xp, iv%bogus(n)%loc )
   enddo

   do n=1, iv % num_airsr
      CALL da_store_obs_grid_info( xp, iv%airsr(n)%loc )
   enddo

   do inst = 1, iv % num_inst
      if (iv % instid(inst) % num_rad < 1) cycle
      do n=1, iv % instid(inst) % num_rad
         CALL da_store_obs_grid_info( xp, iv % instid(inst) % rad(n)%loc )
      enddo
   end do

   IF (trace_use) CALL da_trace_exit("da_setup_obs_interp_wts")

END SUBROUTINE da_setup_obs_interp_wts

