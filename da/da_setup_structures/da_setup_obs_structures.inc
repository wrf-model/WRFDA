subroutine da_setup_obs_structures( xp, ob, iv ,xb)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate and read in components of observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (xpose_type), intent(in):: xp          ! Domain decomposition vars. 
   type (y_type), intent(out)  :: ob          ! Observation structure.
   type (ob_type), intent(out)  :: iv          ! O-B structure.
   TYPE (xb_type)               :: xb          ! First guess structure.

   integer :: iost, ssmi_unit

   if (trace_use) call da_trace_entry("da_setup_obs_structures")

   call da_message((/'Set up observations (ob)'/))

   ! Adjust obs switches


   if (Use_SynopObs .OR. Use_ShipsObs .OR. Use_MetarObs .OR. Use_PilotObs .OR. &
      Use_ProfilerObs .OR. Use_BuoyObs .OR. Use_SoundObs .OR. Use_BogusObs .OR. &
      Use_RadarObs .OR. Use_Radar_rv .OR. Use_Radar_rf .OR. Use_SatemObs .OR. &
      Use_GeoAMVObs .OR. Use_PolarAMVObs .OR. Use_AirepObs .OR. &
      Use_GpspwObs .OR. Use_GpsrefObs .OR. Use_SsmiRetrievalObs .OR. &
      Use_SsmiTbObs .OR. Use_SSMT1Obs .OR. Use_SSMT2Obs .OR. use_qscatobs) then
 
      use_obsgts = .true.
   else
      use_obsgts = .false.
   end if

   if (use_Hirs2Obs .OR. use_Hirs3Obs .OR. use_MsuObs .OR. use_AmsuaObs .OR. &
      use_AmsubObs .OR. use_AirsObs .OR. use_Eos_AmsuaObs .OR. &
      use_Eos_RadObs .OR. use_HsbObs .OR. use_kma1dvar .OR. use_filtered_rad) then
      use_radiance = .true.
   else
      use_radiance = .false.
   end if

   ! testing_dm_exact can be set to .true. to force DM_PARALLEL runs 
   ! to produce results that are bitwise-identical regardless of the number of 
   ! MPI tasks used.  This is useful for validating that multi-processor runs 
   ! are not a source of bugs.  Runtime will be much longer.  This option is 
   ! automatically overridden to .false. for serial or 1-MPI-task runs.  

   if (testing_dm_exact) then
      if (num_procs == 1) then
         testing_dm_exact = .false.
         write(unit=stdout,fmt='(A)') &
            ' testing_dm_exact overridden to .false. for serial or 1-MPI-task run'
      end if
      ! only implmenented for Sound and Synop, so switch other types off
      Use_ShipsObs         = .false.
      Use_MetarObs         = .false.
      Use_BogusObs         = .false.
      Use_PilotObs         = .false.
      Use_AirepObs         = .false.
      Use_GeoAMVObs        = .false.
      Use_PolarAMVObs      = .false.
      Use_BuoyObs          = .false.
      Use_ProfilerObs      = .false.
      Use_SatemObs         = .false.
      Use_GpspwObs         = .false.
      Use_GpsrefObs        = .false.
      Use_SsmiRetrievalObs = .false.
      Use_SsmiTbObs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_Hirs2Obs         = .false.
      use_Hirs3Obs         = .false.
      use_MsuObs           = .false.
      use_AmsuaObs         = .false.
      use_AmsubObs         = .false.
      use_AirsObs          = .false.
      use_Eos_AmsuaObs     = .false.
      use_Eos_radObs       = .false.
      use_HsbObs           = .false.
      Use_Obsgts           = .false.
      Use_Radiance         = .false.
   end if
    
   if (num_pseudo > 0) then
      call da_message((/"Single OBS Test:: Turn off all the OBS switches ***"/))
      Use_SynopObs         = .false.
      Use_ShipsObs         = .false.
      Use_MetarObs         = .false.
      Use_SoundObs         = .false.
      Use_BogusObs         = .false.
      Use_PilotObs         = .false.
      Use_AirepObs         = .false.
      Use_GeoAMVObs        = .false.
      Use_PolarAMVObs      = .false.
      Use_BuoyObs          = .false.
      Use_ProfilerObs      = .false.
      Use_SatemObs         = .false.
      Use_GpspwObs         = .false.
      Use_GpsrefObs        = .false.
      Use_SsmiRetrievalObs = .false.
      Use_SsmiTbObs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_Hirs2Obs         = .false.
      use_Hirs3Obs         = .false.
      use_MsuObs           = .false.
      use_AmsuaObs         = .false.
      use_AmsubObs         = .false.
      use_AirsObs          = .false.
      use_Eos_AmsuaObs     = .false.
      use_Eos_radObs       = .false.
      use_HsbObs           = .false.
      Use_Obsgts           = .true.
      Use_Radiance         = .false.
   end if

   if (sfc_assi_options < 1 .OR. sfc_assi_options > 2) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid sfc_assi_option = ', sfc_assi_options
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if (Use_SsmiRetrievalObs .OR. Use_SsmiTbObs) then
      call da_get_unit(ssmi_unit)
      open (unit   = ssmi_unit,     &
             FORM   = 'FORMATTED',  &
             ACCESS = 'SEQUENTIAL', &
             iostat =  iost,     &
             STATUS = 'OLD')
      close(unit=ssmi_unit)
      call da_free_unit(ssmi_unit)

      if (iost /= 0) then
         write (unit=stderr,fmt='(/,A,I3,A,/)') &
            ' inPUT FILE unit ',ssmi_unit, &
            ' FOR SSMI OBSERVATIONS CANNOT BE FOUND OR CANNOT BE openED'
         Use_SsmiRetrievalObs=.false.
         Use_SsmiTbObs=.false.
      end if 
   end if 


   !---------------------------------------------------------------------------      
   ! [1.0] Setup and read in fields from first guess:
   !----------------------------------------------------------------------------     

   iv%missing = missing
   ! iv%ptop    = xb%ptop

   iv%total_obs         = 0
   iv%total_rad_pixel   = 0
   iv%total_rad_channel = 0
   
   iv%num_sound=0
   iv%num_synop=0
   iv%num_geoamv=0
   iv%num_polaramv=0
   iv%num_airep=0
   iv%num_metar=0
   iv%num_ships=0
   iv%num_pilot=0
   iv%num_satem=0
   iv%num_gpspw=0
   iv%num_gpsref=0
   iv%num_qscat=0
   iv%num_ssmt1=0
   iv%num_ssmt2=0
   iv%num_ssmi_tb=0
   iv%num_ssmi_retrieval=0
   iv%num_buoy=0
   iv%num_profiler=0
   iv%num_radar=0
   iv%num_bogus=0
   iv%num_airsr=0
   iv%num_inst=0
   iv%num_pseudo=0          

   iv%num_sound_glo=0
   iv%num_synop_glo=0
   iv%num_geoamv_glo=0
   iv%num_polaramv_glo=0
   iv%num_airep_glo=0
   iv%num_metar_glo=0
   iv%num_ships_glo=0
   iv%num_pilot_glo=0
   iv%num_satem_glo=0
   iv%num_gpspw_glo=0
   iv%num_gpsref_glo=0
   iv%num_qscat_glo=0
   iv%num_ssmt1_glo=0
   iv%num_ssmt2_glo=0
   iv%num_ssmi_tb_glo=0
   iv%num_ssmi_retrieval_glo=0
   iv%num_buoy_glo=0
   iv%num_profiler_glo=0
   iv%num_Radar_glo=0
   iv%num_bogus_glo=0
   iv%num_airsr_glo=0
   iv%num_pseudo_glo=0             


   ob%num_sound=0
   ob%num_synop=0
   ob%num_geoamv=0
   ob%num_polaramv=0
   ob%num_airep=0
   ob%num_metar=0
   ob%num_ships=0
   ob%num_pilot=0
   ob%num_satem=0
   ob%num_gpspw=0
   ob%num_gpsref=0
   ob%num_qscat=0
   ob%num_ssmt1=0
   ob%num_ssmt2=0
   ob%num_ssmi_tb=0
   ob%num_ssmi_retrieval=0
   ob%num_buoy=0
   ob%num_profiler=0
   ob%num_radar=0
   ob%num_bogus=0
   ob%num_airsr=0
   ob%num_inst=0
   ob%num_pseudo=0          

   if (use_obsgts) then
      ! Conventional obs can be in BUFR or ascii format
      if (ob_format == ob_format_bufr) then
         call da_message((/'Using BUFR format observation input'/))
         ! [1.1] BUFR observation input file:
            call da_setup_obs_structures_bufr(xp, ob, iv)
      else if (ob_format == ob_format_ascii) then
         ! [1.2] GTS observation input file:
         call da_setup_obs_structures_ascii( xp, ob, iv, xb )
      end if
   end if

   ! Radiance files can only be in BUFR

   if (use_radiance) then
#ifndef RTTOV
      ! Stop any radiance work if RTTOV not defined, as there are
      ! huge preprocessor defined holes in the subsequent code, made so people
      ! can compile without the RTTOV library
      call da_error(__FILE__,__LINE__, &
         (/"Cannot run radiance code if compile directive RTTOV false"/))
#endif
      call da_message((/'Using NCEP BUFR radiance 1b input'/))
      call da_setup_bufrtovs_structures(xp, ob, iv)

      ! These are per processor, so uninteresting
      ! write (unit=stderr,fmt=*) ' iv%total_rad_channel = ', iv%total_rad_channel
      ! write (unit=stderr,fmt=*) ' iv%total_rad_pixel   = ', iv%total_rad_pixel
   end if

   ! Get horizontal interpolation weights.

   call da_setup_obs_interp_wts(xp, iv) 

   if (trace_use) call da_trace_exit("da_setup_obs_structures")    

end subroutine da_setup_obs_structures


