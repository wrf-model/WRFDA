SUBROUTINE da_setup_obs_structures_ascii( xp, ob, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: Define, allocate and read of observation structure.
   !          07/03/2006 - Update for AIRS retrieval       Syed RH Rizvi
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN) :: xp         ! Domain decomposition vars.
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   CHARACTER(len=120)           :: filename
   integer                      :: n

   IF (trace_use) CALL da_trace_entry("da_setup_obs_structures_ascii")
   
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
   iv%num_airsr  = 0

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
      iv%ob_numb(n)%airsr  = 0
   enddo

   !--------------------------------------------------------------------------
   ! [1.0] Scan GTS observation header and get idea of number of obs:
   !--------------------------------------------------------------------------
  
   if(num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

         ! scan main body of gts observation file
         call da_scan_obs (gts_iunit, iv, xp, filename)

         if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
            ! scan SSMI observation file
            write(filename(1:10), fmt='(a, i2.2)') 'ssmi_ob.', n
            call da_scan_ssmi (ssmi_iunit, iv, xp, filename)
         endif

         if(Use_RadarObs) then
            ! scan radar observation file
            write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
            call da_scan_radar(Radar_iunit, iv, xp, filename)
         endif
      enddo
   else
      iv%current_ob_time = 1
      filename="ob.ascii"

      call da_scan_obs(gts_iunit, iv, xp, filename)

      !-----------------------------------------------------------------------
      ! read header of ssmi observation file
      !-----------------------------------------------------------------------
      if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
         call da_scan_ssmi(ssmi_iunit, iv, xp)
      endif

      ! scan main body of radar observation file
      if(Use_RadarObs) then
         call da_scan_radar(Radar_iunit, iv, xp)
      endif
   endif

   !--------------------------------------------------------------------  
   ! Safety guard.
   !--------------------------------------------------------------------

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
   ! if(iv%num_pseudo > max_pseudo_input ) &
   !    iv%num_pseudo = max_pseudo_input


   !--------------------------------------------------------------------------
   ! Allocate the ob based on inputed number of obs:
   !--------------------------------------------------------------------------

   CALL da_allocate_observations ( iv )

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

   iv%num_sound_glo=0
   iv%num_synop_glo=0
   iv%num_pilot_glo=0
   iv%num_geoamv_glo=0
   iv%num_polaramv_glo=0
   iv%num_satem_glo=0
   iv%num_airep_glo=0
   iv%num_metar_glo=0
   iv%num_ships_glo=0
   iv%num_gpspw_glo=0
   iv%num_gpsref_glo=0
   iv%num_ssmi_retrieval_glo=0
   iv%num_ssmi_tb_glo=0
   iv%num_ssmt1_glo=0
   iv%num_ssmt2_glo=0
   iv%num_pseudo_glo=0
   iv%num_qscat_glo=0
   iv%num_profiler_glo=0
   iv%num_buoy_glo=0
   iv%num_Radar_glo=0
   iv%num_bogus_glo=0
   iv%num_airsr_glo=0

   if(num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

         ! Read gts observation file
         call da_read_obs (gts_iunit, iv, xp, filename)

         if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
            ! read ssmi observation file
            write(filename(1:10), fmt='(a, i2.2)') 'ssmi_ob.', n
            call da_read_ssmi (ssmi_iunit, iv, xp, filename)
         endif

         if(Use_RadarObs) then
            ! read radar observation file
            write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
            call da_read_radar(Radar_iunit, iv, xp, filename)
         endif
      enddo
   else
      iv%current_ob_time = 1
      filename="ob.ascii"

      call da_read_obs(gts_iunit, iv, xp, filename)

      if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
         ! read ssmi observation file
         call da_read_ssmi (ssmi_iunit, iv, xp)
      endif

      if(Use_RadarObs) then
         ! read radar observation file
         call da_read_radar(Radar_iunit, iv, xp)
      endif
   endif

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   CALL da_fill_obs_structures( iv, ob )

   iv%current_ob_time = 1

   IF (trace_use) CALL da_trace_exit("da_setup_obs_structures_ascii")

END SUBROUTINE da_setup_obs_structures_ascii

