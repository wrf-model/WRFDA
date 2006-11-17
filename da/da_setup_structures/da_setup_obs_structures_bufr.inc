subroutine da_setup_obs_structures_bufr( xp, ob, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xpose_type), intent(in) :: xp         ! Domain decomposition vars.
   type ( y_type), intent(out)  :: ob          ! Observation structure.
   type (ob_type), intent(out)  :: iv          ! O-B structure.

   character(len=120)           :: filename
   integer                      :: n   

   if (trace_use) call da_trace_entry("da_setup_obs_structures_bufr")

   !--------------------------------------------------------------------------
   ! [0.0] Initialize the counters.
   !--------------------------------------------------------------------------

   iv%total_obs = 0
   iv%num_sound = 0
   iv%num_synop = 0
   iv%num_pilot = 0
   iv%num_geoamv = 0
   iv%num_polaramv = 0
   iv%num_satem = 0
   iv%num_airep = 0
   iv%num_metar = 0
   iv%num_ships = 0
   iv%num_gpspw = 0
   iv%num_gpsref = 0
   iv%num_ssmi_retrieval = 0
   iv%num_ssmi_tb       = 0
   iv%num_ssmt1 = 0
   iv%num_ssmt2 = 0
   iv%num_pseudo = 0
   iv%num_qscat = 0
   iv%num_profiler  = 0
   iv%num_buoy  = 0
   iv%num_radar  = 0
   iv%num_bogus  = 0
   iv%num_airsr = 0

   do n=0, max_fgat_time
      iv%ob_numb(n)%total = 0
      iv%ob_numb(n)%sound = 0
      iv%ob_numb(n)%synop = 0
      iv%ob_numb(n)%pilot = 0
      iv%ob_numb(n)%geoamv = 0
      iv%ob_numb(n)%polaramv = 0
      iv%ob_numb(n)%satem = 0
      iv%ob_numb(n)%airep = 0
      iv%ob_numb(n)%metar = 0
      iv%ob_numb(n)%ships = 0
      iv%ob_numb(n)%gpspw = 0
      iv%ob_numb(n)%gpsref = 0
      iv%ob_numb(n)%ssmi_retrieval = 0
      iv%ob_numb(n)%ssmi_tb       = 0
      iv%ob_numb(n)%ssmt1 = 0
      iv%ob_numb(n)%ssmt2 = 0
      iv%ob_numb(n)%pseudo = 0
      iv%ob_numb(n)%qscat = 0
      iv%ob_numb(n)%profiler  = 0
      iv%ob_numb(n)%buoy  = 0
      iv%ob_numb(n)%radar  = 0
      iv%ob_numb(n)%bogus  = 0
      iv%ob_numb(n)%airsr = 0
   end do

   !--------------------------------------------------------------------------
   ! [1.0] Scan BUFR observation header and get idea of number of obs:
   !--------------------------------------------------------------------------

   if (num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2,a)') 'ob', n,'.bufr'

         ! scan PREPBUFR OBSERVATION FILE
         call da_scan_bufr_obs (iv, xp, filename)


         ! if (Use_RadarObs) then
         !    ! scan Radar OBSERVATION FILE
         !    write(filename(1:10), fmt='(a, i2.2,a)') 'radar', n,'.dat'
         !    call da_scan_bufr_radar(iv, xp, filename)
         ! end if
      end do
   else
      iv%current_ob_time = 1
      filename="ob.bufr"
      call da_scan_bufr_obs(iv, xp, filename)

      ! scan MAin BODY OF Radar OBSERVATION FILE
      ! if(Use_RadarObs) &
      ! call da_scan_bufr_radar(iv, xp,'radar.dat')
   end if

   !-----------------------------------------------------------------
   ! Safety guard.
   !-----------------------------------------------------------------

   if(iv%num_sound > max_sound_input ) &
      iv%num_sound = max_sound_input

   if(iv%num_synop > max_synop_input ) &
      iv%num_synop = max_synop_input
   if(iv%num_pilot > max_pilot_input ) &
      iv%num_pilot = max_pilot_input
   if(iv%num_geoamv > max_geoamv_input ) &
      iv%num_geoamv = max_geoamv_input
   if(iv%num_polaramv > max_polaramv_input ) &
      iv%num_polaramv = max_polaramv_input
   if(iv%num_satem > max_satem_input ) &
      iv%num_satem = max_satem_input
   if(iv%num_airep > max_airep_input ) &
      iv%num_airep = max_airep_input
   if(iv%num_metar > max_metar_input ) &
      iv%num_metar = max_metar_input
   if(iv%num_ships > max_ships_input ) &
      iv%num_ships = max_ships_input
   if(iv%num_gpspw > max_gpspw_input ) &
      iv%num_gpspw = max_gpspw_input
   if(iv%num_gpsref > max_gpsref_input ) &
      iv%num_gpsref = max_gpsref_input
   if(iv%num_ssmi_retrieval > max_ssmi_ret_input ) &
      iv%num_ssmi_retrieval = max_ssmi_ret_input
   if(iv%num_ssmi_tb > max_ssmi_tb_input ) &
      iv%num_ssmi_tb = max_ssmi_tb_input
   if(iv%num_ssmt1 > max_ssmt1_input ) &
      iv%num_ssmt1 = max_ssmt1_input
   if(iv%num_ssmt2 > max_ssmt2_input ) &
      iv%num_ssmt2 = max_ssmt2_input
   if(iv%num_profiler > max_profiler_input ) &
      iv%num_profiler = max_profiler_input
   if(iv%num_buoy > max_buoy_input ) &
      iv%num_buoy = max_buoy_input
   if(iv%num_radar > max_radar_input ) &
      iv%num_radar = max_radar_input
   if(iv%num_bogus > max_bogus_input ) &
      iv%num_bogus = max_bogus_input
   if(iv%num_airsr > max_airsret_input ) &
      iv%num_airsr = max_airsret_input

   !-------------------------------------------------------------------------
   ! Allocate the ob based on inputed number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations ( iv )

   iv%total_obs = 0
   iv%num_sound = 0
   iv%num_synop = 0
   iv%num_pilot = 0
   iv%num_geoamv = 0
   iv%num_polaramv = 0
   iv%num_satem = 0
   iv%num_airep = 0
   iv%num_metar = 0
   iv%num_ships = 0
   iv%num_gpspw = 0
   iv%num_gpsref = 0
   iv%num_ssmi_retrieval = 0
   iv%num_ssmi_tb       = 0
   iv%num_ssmt1 = 0
   iv%num_ssmt2 = 0
   iv%num_pseudo = 0
   iv%num_qscat = 0
   iv%num_profiler  = 0
   iv%num_buoy  = 0
   iv%num_radar  = 0
   iv%num_bogus  = 0
   iv%num_airsr  = 0

   if (num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2)') 'ob.', n

         ! Read PREPBUFR OBSERVATION FILE
         call da_read_bufr_obs (iv, xp, filename)

         ! if (Use_RadarObs) then
         !    ! Read Radar OBSERVATION FILE
         !    write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
         !    call da_read_bufr_radar(iv, xp, filename)
         ! end if
      end do
   else

      iv%current_ob_time = 1

      filename="ob.bufr"
      call da_read_bufr_obs(iv, xp,filename)

      ! if (Use_RadarObs) then
      !    !Read Radar OBSERVATION FILE
      !    call da_read_bufr_radar(iv, xp)
      ! end if
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures(xp, iv, ob )

   iv%current_ob_time = 1

   if (trace_use) call da_trace_exit("da_setup_obs_structures_bufr")

end subroutine da_setup_obs_structures_bufr

