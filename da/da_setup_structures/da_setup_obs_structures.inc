subroutine da_setup_obs_structures( grid, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate and read in components of observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain),     intent(inout) :: grid ! Model data
   type (y_type),     intent(out)   :: ob  ! Observation structure.
   type (ob_type),    intent(out)   :: iv  ! O-B structure.

   if (trace_use) call da_trace_entry("da_setup_obs_structures")

   call da_message((/'Set up observations (ob)'/))

   ! Adjust obs switches


   if (use_synopobs .OR. use_shipsobs .OR. use_metarobs .OR. use_pilotobs .OR. &
      use_profilerobs .OR. use_buoyobs .OR. use_soundobs .OR. use_bogusobs .OR. &
      use_radarobs .OR. use_radar_rv .OR. use_radar_rf .OR. use_satemobs .OR. &
      use_geoamvobs .OR. use_polaramvobs .OR. use_airepobs .OR. &
      use_gpspwobs .OR. use_gpsrefobs .OR. use_ssmiretrievalobs .OR. &
      use_ssmitbobs .OR. use_ssmt1obs .OR. use_ssmt2obs .OR. use_qscatobs .or. &
      use_airsretobs ) then
 
      use_obsgts = .true.
   else
      use_obsgts = .false.
   end if

   if (use_hirs2obs .OR. use_hirs3obs .OR. use_msuobs .OR. use_amsuaobs .OR. &
      use_amsubobs .OR. use_airsobs .OR. use_eos_amsuaobs .OR. &
      use_eos_radobs .OR. use_hsbobs .OR. use_kma1dvar .OR. use_filtered_rad .or. &
      use_ssmisobs .or. use_hirs4obs .or. use_mhsobs) then
      use_rad = .true.
   else
      use_rad = .false.
   end if

   ! test_dm_exact can be set to .true. to force DM_PARALLEL runs 
   ! to produce results that are bitwise-identical regardless of the number of 
   ! MPI tasks used.  This is useful for validating that multi-processor runs 
   ! are not a source of bugs.  Runtime will be much longer.  This option is 
   ! automatically overridden to .false. for serial or 1-MPI-task runs.  

   if (test_dm_exact) then
      if (num_procs == 1) then
         test_dm_exact = .false.
         write(unit=stdout,fmt='(A)') &
            ' test_dm_exact overridden to .false. for serial or 1-MPI-task run'
      end if
      ! only implmenented for Sound and Synop, so switch other types off
      use_shipsobs         = .false.
      use_metarobs         = .false.
      use_bogusobs         = .false.
      use_pilotobs         = .false.
      use_airepobs         = .false.
      use_geoamvobs        = .false.
      use_polaramvobs      = .false.
      use_buoyobs          = .false.
      use_profilerobs      = .false.
      use_satemobs         = .false.
      use_gpspwobs         = .false.
      use_gpsrefobs        = .false.
      use_ssmiretrievalobs = .false.
      use_ssmitbobs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_hirs2obs         = .false.
      use_hirs3obs         = .false.
      use_hirs4obs         = .false.
      use_mhsobs           = .false.
      use_msuobs           = .false.
      use_amsuaobs         = .false.
      use_amsubobs         = .false.
      use_airsobs          = .false.
      use_eos_amsuaobs     = .false.
      use_eos_radobs       = .false.
      use_hsbobs           = .false.
      use_obsgts           = .false.
      use_rad         = .false.
      use_airsretobs       = .false.
   end if
    
   if (num_pseudo > 0) then
      call da_message((/"Single OBS Test:: Turn off all the OBS switches ***"/))
      use_synopobs         = .false.
      use_shipsobs         = .false.
      use_metarobs         = .false.
      use_soundobs         = .false.
      use_bogusobs         = .false.
      use_pilotobs         = .false.
      use_airepobs         = .false.
      use_geoamvobs        = .false.
      use_polaramvobs      = .false.
      use_buoyobs          = .false.
      use_profilerobs      = .false.
      use_satemobs         = .false.
      use_gpspwobs         = .false.
      use_gpsrefobs        = .false.
      use_ssmiretrievalobs = .false.
      use_ssmitbobs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_hirs2obs         = .false.
      use_hirs3obs         = .false.
      use_hirs4obs         = .false.
      use_mhsobs           = .false.
      use_msuobs           = .false.
      use_amsuaobs         = .false.
      use_amsubobs         = .false.
      use_airsobs          = .false.
      use_eos_amsuaobs     = .false.
      use_eos_radobs       = .false.
      use_hsbobs           = .false.
      use_obsgts           = .true.
      use_rad              = .false.
      use_airsretobs       = .false.
   end if

   if (sfc_assi_options < 1 .OR. sfc_assi_options > 2) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid sfc_assi_option = ', sfc_assi_options
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   !---------------------------------------------------------------------------      
   ! [1.0] Setup and read in fields from first guess:
   !----------------------------------------------------------------------------     

   iv%missing = missing
   ! iv%ptop    = grid%xb%ptop

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
   iv%num_radar_glo=0
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
            call da_setup_obs_structures_bufr(grid%xp, ob, iv)
      else if (ob_format == ob_format_ascii) then
         ! [1.2] GTS observation input file:
         call da_setup_obs_structures_ascii( grid%xp, ob, iv, grid%xb )
      end if
   end if

   ! Radiance files can only be in BUFR

   if (use_rad) then
      call da_message((/'Using NCEP BUFR radiance 1b input'/))
      call da_setup_bufrtovs_structures(grid, ob, iv)
   end if

   ! Get horizontal interpolation weights.

   call da_setup_obs_interp_wts( grid%xp, iv) 

   if (trace_use) call da_trace_exit("da_setup_obs_structures")    

end subroutine da_setup_obs_structures


