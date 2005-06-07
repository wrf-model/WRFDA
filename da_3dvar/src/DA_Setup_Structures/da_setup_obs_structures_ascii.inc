SUBROUTINE da_setup_obs_structures_ascii( xp, ob, iv )

!------------------------------------------------------------------------------
! PURPOSE: Define, allocate and read of observation structure.
!
! METHOD:  Define, allocate and read of observation structure.
!
! HISTORY: 01/20/2000 - Creation of F90 version.           Dale Barker
!          10/19/2001 - Mods for parallel implementation.  Al Bourgeois
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xpose_type), INTENT(IN) :: xp         ! Domain decomposition vars.
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   CHARACTER(len=120)           :: filename
   integer                      :: n
   
!-----------------------------------------------------------------------------
!  [0.0] Initialize the counters.
!-----------------------------------------------------------------------------

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
   enddo

!-----------------------------------------------------------------------------
!  [1.0] Scan GTS observation header and get idea of number of obs:
!-----------------------------------------------------------------------------
  
   if(num_fgat_time > 1) then
      filename = ' '

      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

!--------scan MAIN BODY OF GTS OBSERVATION FILE
         call DA_Scan_Obs (gts_iunit, iv, xp, filename)

         if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
!-----------SCAN SSMI OBSERVATION FILE
            write(filename(1:10), fmt='(a, i2.2)') 'ssmi_ob.', n
            call DA_Scan_SSMI (ssmi_iunit, iv, xp, filename)
         endif

         if(Use_RadarObs) then
!-----------scan Radar OBSERVATION FILE
            write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
            call DA_Scan_Radar(Radar_iunit, iv, xp, filename)
         endif
      enddo
   else
      iv%current_ob_time = 1
      call DA_Scan_Obs(gts_iunit, iv, xp)

!-----------------------------------------------------------------------------
!     READ HEADER OF SSMI OBSERVATION FILE
!-----------------------------------------------------------------------------
      if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
         call DA_Scan_SSMI(ssmi_iunit, iv, xp)
      endif

!-----scan MAIN BODY OF Radar OBSERVATION FILE
      if(Use_RadarObs) then
         call DA_Scan_Radar(Radar_iunit, iv, xp)
      endif
   endif

!--------------------------------------------------------------------
!--Safety guard.
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
   if(iv%num_ssmi_retrieval > max_ssmi_retrieval_input ) &
      iv%num_ssmi_retrieval = max_ssmi_retrieval_input
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
!  if(iv%num_pseudo > max_pseudo_input ) &
!     iv%num_pseudo = max_pseudo_input

!-----------------------------------------------------------------------------
!   Allocate the ob based on inputed number of obs:
!-----------------------------------------------------------------------------

   CALL DA_Allocate_Observations ( iv )

!-----------------------------------------------------------------------------

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

!-----------------------------------------------------------------------------

   if(num_fgat_time > 1) then
      do n=1, num_fgat_time
         iv%current_ob_time = n

         write(filename(1:10), fmt='(a, i2.2)') 'fgat_ob.', n

!--------Read GTS OBSERVATION FILE
         call DA_Read_Obs (gts_iunit, iv, xp, filename)

         if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
!-----------READ SSMI OBSERVATION FILE
            write(filename(1:10), fmt='(a, i2.2)') 'ssmi_ob.', n
            call DA_Read_SSMI (ssmi_iunit, iv, xp, filename)
         endif

         if(Use_RadarObs) then
!-----------Read Radar OBSERVATION FILE
            write(filename(1:10), fmt='(a, i2.2)') 'radarob.', n
            call DA_Read_Radar(Radar_iunit, iv, xp, filename)
         endif
      enddo
   else

      iv%current_ob_time = 1

      call DA_Read_Obs(gts_iunit, iv, xp)

      if(Use_SsmiRetrievalObs .or. Use_SsmiTbObs) then
!--------READ SSMI OBSERVATION FILE
         call DA_Read_SSMI (ssmi_iunit, iv, xp)
      endif

      if(Use_RadarObs) then
!--------Read Radar OBSERVATION FILE
         call DA_Read_Radar(Radar_iunit, iv, xp)
      endif
   endif

!-----------------------------------------------------------------------------
!  [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
!-----------------------------------------------------------------------------

   CALL da_fill_obs_structures( iv, ob )

   iv%current_ob_time = 1

END SUBROUTINE da_setup_obs_structures_ascii

