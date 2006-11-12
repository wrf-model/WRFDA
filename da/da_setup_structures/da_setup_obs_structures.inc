SUBROUTINE da_setup_obs_structures( xp, ob, iv )

   !---------------------------------------------------------------------------
   ! PURPOSE: Allocate and read in components of observation structure.
   !---------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), INTENT(IN):: xp          ! Domain decomposition vars. 
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   integer :: iost, ssmi_unit

   IF (trace_use) CALL da_trace_entry("da_setup_obs_structures")

   CALL da_message((/'[4.0] Set up observations (ob)'/))

   ! Adjust obs switches


   IF (Use_SynopObs .OR. Use_ShipsObs .OR. Use_MetarObs .OR. Use_PilotObs .OR. &
      Use_ProfilerObs .OR. Use_BuoyObs .OR. Use_SoundObs .OR. Use_BogusObs .OR. &
      Use_RadarObs .OR. Use_Radar_rv .OR. Use_Radar_rf .OR. Use_SatemObs .OR. &
      Use_GeoAMVObs .OR. Use_PolarAMVObs .OR. Use_AirepObs .OR. &
      Use_GpspwObs .OR. Use_GpsrefObs .OR. Use_SsmiRetrievalObs .OR. &
      Use_SsmiTbObs .OR. Use_SSMT1Obs .OR. Use_SSMT2Obs .OR. use_qscatobs) THEN
 
      use_obsgts = .TRUE.
   ELSE
      use_obsgts = .FALSE.
   END IF

   IF (use_Hirs2Obs .OR. use_Hirs3Obs .OR. use_MsuObs .OR. use_AmsuaObs .OR. &
      use_AmsubObs .OR. use_AirsObs .OR. use_Eos_AmsuaObs .OR. &
      use_Eos_RadObs .OR. use_HsbObs .OR. use_kma1dvar .OR. use_filtered_rad) THEN
      use_radiance = .TRUE.
   ELSE
      use_radiance = .FALSE.
   END IF

   ! testing_dm_exact can be set to .true. to force DM_PARALLEL runs 
   ! to produce results that are bitwise-identical regardless of the number of 
   ! MPI tasks used.  This is useful for validating that multi-processor runs 
   ! are not a source of bugs.  Runtime will be much longer.  This option is 
   ! automatically overridden to .false. for serial or 1-MPI-task runs.  

   IF (testing_dm_exact) THEN
      IF (num_procs == 1) THEN
         testing_dm_exact = .FALSE.
         WRITE(UNIT=stdout,FMT='(A)') &
            ' testing_dm_exact overridden to .FALSE. for serial or 1-MPI-task run'
      ENDIF
      ! Only implmenented for Sound and Synop, so switch other types off
      Use_ShipsObs         = .FALSE.
      Use_MetarObs         = .FALSE.
      Use_BogusObs         = .FALSE.
      Use_PilotObs         = .FALSE.
      Use_AirepObs         = .FALSE.
      Use_GeoAMVObs        = .FALSE.
      Use_PolarAMVObs      = .FALSE.
      Use_BuoyObs          = .FALSE.
      Use_ProfilerObs      = .FALSE.
      Use_SatemObs         = .FALSE.
      Use_GpspwObs         = .FALSE.
      Use_GpsrefObs        = .FALSE.
      Use_SsmiRetrievalObs = .FALSE.
      Use_SsmiTbObs        = .FALSE.
      use_ssmt1obs         = .FALSE.
      use_ssmt2obs         = .FALSE.
      use_qscatobs         = .FALSE.
      use_Hirs2Obs         = .FALSE.
      use_Hirs3Obs         = .FALSE.
      use_MsuObs           = .FALSE.
      use_AmsuaObs         = .FALSE.
      use_AmsubObs         = .FALSE.
      use_AirsObs          = .FALSE.
      use_Eos_AmsuaObs     = .FALSE.
      use_Eos_radObs       = .FALSE.
      use_HsbObs           = .FALSE.
      Use_Obsgts           = .FALSE.
      Use_Radiance         = .FALSE.
   ENDIF
    
   if (num_pseudo > 0) then
      call da_message((/"Single OBS Test:: Turn off all the OBS switches ***"/))
      Use_SynopObs         = .FALSE.
      Use_ShipsObs         = .FALSE.
      Use_MetarObs         = .FALSE.
      Use_SoundObs         = .FALSE.
      Use_BogusObs         = .FALSE.
      Use_PilotObs         = .FALSE.
      Use_AirepObs         = .FALSE.
      Use_GeoAMVObs        = .FALSE.
      Use_PolarAMVObs      = .FALSE.
      Use_BuoyObs          = .FALSE.
      Use_ProfilerObs      = .FALSE.
      Use_SatemObs         = .FALSE.
      Use_GpspwObs         = .FALSE.
      Use_GpsrefObs        = .FALSE.
      Use_SsmiRetrievalObs = .FALSE.
      Use_SsmiTbObs        = .FALSE.
      use_ssmt1obs         = .FALSE.
      use_ssmt2obs         = .FALSE.
      use_qscatobs         = .FALSE.
      use_Hirs2Obs         = .FALSE.
      use_Hirs3Obs         = .FALSE.
      use_MsuObs           = .FALSE.
      use_AmsuaObs         = .FALSE.
      use_AmsubObs         = .FALSE.
      use_AirsObs          = .FALSE.
      use_Eos_AmsuaObs     = .FALSE.
      use_Eos_radObs       = .FALSE.
      use_HsbObs           = .FALSE.
      Use_Obsgts           = .TRUE.
      Use_Radiance         = .FALSE.
   endif

   IF ( sfc_assi_options < 1 .OR. sfc_assi_options > 2 ) THEN
      WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
         'Invalid sfc_assi_option = ', sfc_assi_options
      call da_error(__FILE__,__LINE__,errmsg(1:1))
   END IF

   IF (Use_SsmiRetrievalObs .OR. Use_SsmiTbObs) THEN
      call da_get_unit(ssmi_unit)
      OPEN ( UNIT   = ssmi_unit,     &
             FORM   = 'FORMATTED',  &
             ACCESS = 'SEQUENTIAL', &
             IOSTAT =  iost,     &
             STATUS = 'OLD')
      CLOSE(UNIT=ssmi_unit)
      call da_free_unit(ssmi_unit)

      IF (iost /= 0) THEN
         WRITE (UNIT=stderr,FMT='(/,A,I3,A,/)') &
            ' INPUT FILE UNIT ',ssmi_unit, &
            ' FOR SSMI OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         Use_SsmiRetrievalObs=.false.
         Use_SsmiTbObs=.false.
      END IF 
   END IF 


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

   iv%num_pseudo=num_pseudo
   iv%num_pseudo_glo=iv%num_pseudo

   IF (use_obsgts) then
      ! Conventional obs can be in BUFR or ascii format
      IF ( ob_format == ob_format_bufr ) THEN
         CALL da_message((/'Using BUFR format observation input'/))
         ! [1.1] BUFR observation input file:
         IF (iv%num_pseudo > 0) THEN
            CALL da_message((/'Pseudo test only.'/))
            CALL da_allocate_y( iv, ob )
            CALL da_setup_pseudo_obs( iv, ob )
            iv%pseudo(1)%loc%i=int(iv%pseudo(1)%loc%x)
            iv%pseudo(1)%loc%j=int(iv%pseudo(1)%loc%y)
            iv%pseudo(1)%loc%dx=iv%pseudo(1)%loc%x-real(iv%pseudo(1)%loc%i)
            iv%pseudo(1)%loc%dy=iv%pseudo(1)%loc%y-real(iv%pseudo(1)%loc%j)
            iv%pseudo(1)%loc%dxm=1.0-iv%pseudo(1)%loc%dx
            iv%pseudo(1)%loc%dym=1.0-iv%pseudo(1)%loc%dy
         ELSE
            CALL da_setup_obs_structures_bufr( xp, ob, iv )
         END IF 
      ELSE IF ( ob_format == ob_format_ascii ) THEN
         ! [1.2] GTS observation input file:
         CALL da_setup_obs_structures_ascii( xp, ob, iv )
      END IF
   END IF

   ! Radiance files can only be in BUFR

   IF (use_radiance) then
#ifndef RTTOV
      ! Stop any radiance work if RTTOV not defined, as there are
      ! huge preprocessor defined holes in the subsequent code, made so people
      ! can compile without the RTTOV library
      CALL da_error(__FILE__,__LINE__, &
         (/"Cannot run radiance code if compile directive RTTOV false"/))
#endif
      CALL da_message((/'Using NCEP BUFR radiance 1b input'/))
      CALL da_setup_bufrtovs_structures( xp, ob, iv )
      ! These are per processor, so uninteresting
      ! write (UNIT=stderr,FMT=*) ' iv%total_rad_channel = ', iv%total_rad_channel
      ! write (UNIT=stderr,FMT=*) ' iv%total_rad_pixel   = ', iv%total_rad_pixel
   END IF

   ! Get horizontal interpolation weights.
   call da_setup_obs_interp_wts( xp, iv ) 

   IF (trace_use) CALL da_trace_exit("da_setup_obs_structures")    

END SUBROUTINE da_setup_obs_structures

