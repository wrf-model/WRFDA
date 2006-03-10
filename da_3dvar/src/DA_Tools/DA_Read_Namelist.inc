SUBROUTINE DA_Read_Namelist

!----------------------------------------------------------------------------
!  PURPOSE:       Read in 3DVAR input parameters from namelist.
!
!  METHOD:        Open namelist input file and read in values.
!
!  MODIFICATIONS:
!  99/11/30: Creation (D. Barker)
!
!  Additions:
!             07/08/2003      Profiler and Buoy Obs            Syed RH Rizvi
!             01/31/2005      Added global option              Syed RH Rizvi
!             07/18/2005      Add rtminit options              Zhiquan Liu
!----------------------------------------------------------------------------
   IMPLICIT NONE

!  Local scalars:

   CHARACTER*20              :: namelist_file      ! Input namelist filename.
   INTEGER, PARAMETER        :: namelist_unit = 7  ! Input namelist unit.
   INTEGER                   :: iost               ! Error code.

   CHARACTER(LEN=24)         :: tmp_date
   INTEGER                   :: len
   INTEGER                   :: nproc

!  Namelist contents :

   NAMELIST /RECORD1/  MODEL_TYPE, write_increments, global, print_detail, lvar4d, &
                       stdout_unit, stderr_unit
   NAMELIST /RECORD2/  ANALYSIS_TYPE, ANALYSIS_DATE, ANALYSIS_ACCU, W_INCREMENTS, &
                       DT_cloud_model, &
                       write_qcw, write_qrn, write_qci, write_qsn, write_qgr
   NAMELIST /RECORD3/  fg_format, ob_format, num_fgat_time
   NAMELIST /RECORD4/  PROCESS_OBS, obs_qc_pointer, &
                       use_SynopObs, &
                       use_ShipsObs, &
                       use_MetarObs, &
                       use_RadarObs, &
                       use_Radar_rv, &
                       use_Radar_rf, &
                       use_PilotObs, &
                       use_SoundObs, &
                       use_SatemObs, &
                       use_GeoAMVObs, &
                       use_PolarAMVObs, &
                       use_AirepObs, &
                       use_GpspwObs, &
                       use_GpsrefObs, &
                       use_SsmiRetrievalObs, &
                       use_SsmiTbObs, &
                       use_ssmt1obs, &
                       use_ssmt2obs, &
                       use_qscatobs, &
                       use_ProfilerObs, &
                       use_BuoyObs, &
                       use_BogusObs, &
                       use_Hirs2Obs, use_Hirs3Obs,  &
                       use_MsuObs, &
                       use_AmsuaObs, use_AmsubObs,  &
                       use_AirsObs, &
                       use_Eos_amsuaObs, &
                       use_Eos_radObs, &
                       use_HsbObs, &
                       check_max_iv, &
                       use_obs_errfac, &
                       put_rand_seed, &
                       omb_set_rand, &
                       omb_add_noise, &
                       max_sound_input, &
                       max_synop_input, &
                       max_geoamv_input, &
                       max_polaramv_input, &
                       max_Radar_input, &
                       max_airep_input, &
                       max_satem_input, &
                       max_pilot_input, &
                       max_bogus_input, &
                       max_metar_input, &
                       max_gpspw_input, &
                       max_ships_input, &
                       max_profiler_input, &
                       max_buoy_input, &
                       max_ssmi_ret_input, &
                       max_ssmi_tb_input, &
                       max_ssmt1_input, &
                       max_ssmt2_input, &
                       max_qscat_input 
   NAMELIST /RECORD5/  TIME_WINDOW, print_obs_info
   NAMELIST /RECORD6/  max_ext_its, EPS, NTMAX, NSAVE, &
                       write_switch, write_interval
   NAMELIST /RECORD7/  rf_passes, &
                       var_scaling1, var_scaling2, var_scaling3, &
                       var_scaling4, var_scaling5, &
                       len_scaling1, len_scaling2, len_scaling3, &
                       len_scaling4, len_scaling5
   NAMELIST /RECORD8/  def_sub_domain, &
                       x_start_sub_domain, x_end_sub_domain, &
                       y_start_sub_domain, y_end_sub_domain
   NAMELIST /RECORD10/ Testing_3DVAR, test_transforms, &
                       test_statistics, interpolate_stats, &
                       testing_dm_exact
   NAMELIST /RECORD11/ cv_options, cv_options_hum, check_rh, &
                       as1,as2,as3,as4,as5, set_omb_rand_fac, &
                       seed_array1, seed_array2, &
                       sfc_assi_options, calculate_cg_cost_fn, &
                       minimisation_option, write_outer_loop,lat_stats_option
   NAMELIST /RECORD12/ balance_type
   NAMELIST /RECORD13/ vert_corr, vertical_ip, vert_evalue, &
                       max_vert_var1, max_vert_var2, &
                       max_vert_var3, max_vert_var4, max_vert_var5
   NAMELIST /RECORD14/ rtminit_print, rtminit_nsensor, &
                       rtminit_platform,rtminit_satid, rtminit_sensor,  &
                       use_kma1dvar,  &
                       lmonitoring,lread_biascoef,lbiascorr, lrttov_scatt, &
                       lwrite_iv,lwrite_profile,use_landem, mw_emis_sea
   NAMELIST /pseudo_ob_nl/ num_pseudo, pseudo_x, pseudo_y, pseudo_z, &
                           pseudo_val, pseudo_err, pseudo_var

!----------------------------------------------------------------------------------
   

   CALL wrf_message("[1.0] Read 3DVAR namelist")

!  Namelist defaults:

!--record1
   MODEL_TYPE       = 'WRF'
   WRITE_INCREMENTS = .FALSE.
   lvar4d           = .false.
   PRINT_DETAIL     = 0
   global           = .FALSE. 
   stdout_unit      = 6
   stderr_unit      = 0


!--record2
   ANALYSIS_TYPE = '3D-VAR'
   ANALYSIS_DATE = '2002-08-03_00:00:00.0000'
   ANALYSIS_ACCU = 900
   W_INCREMENTS  = .FALSE.
   DT_cloud_model = .FALSE.
   write_qcw = .FALSE.
   write_qrn = .FALSE.
   write_qci = .FALSE.
   write_qsn = .FALSE.
   write_qgr = .FALSE.

!--record3
   fg_format = 1
   ob_format = 2
   num_fgat_time = 1

!--record4
   PROCESS_OBS    = 'YES'
   obs_qc_pointer = 0
   Use_SynopObs   = .TRUE.
   Use_ShipsObs   = .TRUE.
   Use_MetarObs   = .TRUE.
   Use_SoundObs   = .TRUE.
   Use_PilotObs   = .TRUE.
   Use_AirepObs   = .TRUE.
   Use_GeoAMVObs   = .TRUE.
   Use_PolarAMVObs   = .TRUE.
   Use_BogusObs   = .FALSE.
   Use_BuoyObs    = .FALSE.
   Use_ProfilerObs= .FALSE.
   Use_SatemObs   = .FALSE.
   Use_GpspwObs   = .FALSE.
   Use_GpsrefObs   = .FALSE.
   Use_SsmiRetrievalObs = .FALSE.
   Use_SsmiTbObs  = .FALSE.
   use_ssmt1obs   = .FALSE.
   use_ssmt2obs   = .FALSE.
   use_qscatobs   = .FALSE.
   use_hirs2Obs   = .FALSE.       
   use_hirs3Obs   = .FALSE.       
   Use_MsuObs     = .FALSE.       
   Use_AmsuaObs   = .FALSE.       
   Use_AmsubObs   = .FALSE.
   Use_AirsObs    = .FALSE.
   Use_Eos_AmsuaObs = .FALSE.
   Use_Eos_RadObs = .FALSE.
   Use_HsbObs     = .FALSE.
   check_max_iv   = .TRUE.
   use_obs_errfac = .FALSE.
   put_rand_seed  = .FALSE.
   omb_set_rand   = .FALSE.
   omb_add_noise  = .FALSE.

   max_sound_input = max_sound
   max_synop_input = max_synop
   max_geoamv_input = max_geoamv
   max_polaramv_input = max_polaramv
   max_airep_input = max_airep
   max_satem_input = max_satem
   max_pilot_input = max_pilot
   max_Radar_input = max_Radar
   max_metar_input = max_metar
   max_gpspw_input = max_gpspw
   max_ships_input = max_ships
   max_profiler_input = max_profiler
   max_bogus_input = max_bogus
   max_buoy_input = max_buoy
   max_ssmi_ret_input = max_ssmi_retrieval
   max_ssmi_tb_input = max_ssmi_tb
   max_ssmt1_input = max_ssmt1
   max_ssmt2_input = max_ssmt2
   max_qscat_input = max_qscat
   max_gpsref_input = max_gpsref

!--record5
   TIME_WINDOW    = 3.
   print_obs_info = .false.

!--record6
   max_ext_its    = 1
   EPS            = 1.E-02
   NTMAX          = 100
   NSAVE          = 4
   WRITE_SWITCH   = .FALSE.
   WRITE_INTERVAL = 5

!--record7
   RF_PASSES      = 6
   VAR_SCALING1   = 1.0
   VAR_SCALING2   = 1.0
   VAR_SCALING3   = 1.0
   VAR_SCALING4   = 1.0
   VAR_SCALING5   = 1.0
   LEN_SCALING1   = 1.0
   LEN_SCALING2   = 1.0
   LEN_SCALING3   = 1.0
   LEN_SCALING4   = 1.0
   LEN_SCALING5   = 1.0

!--record8
   def_sub_domain = .FALSE.
   x_start_sub_domain = 1.0
   y_start_sub_domain = 1.0
   x_end_sub_domain   = 1.0e10
   y_end_sub_domain   = 1.0e10   

!--record10
   Testing_3DVAR  = .FALSE.
   Test_Transforms = .FALSE.
   Test_Statistics = .FALSE.
   Interpolate_Stats = .TRUE.
   testing_dm_exact = .FALSE.

!--record11
   minimisation_option = 1
   cv_options     = 2
   cv_options_hum = 1
   check_rh       = 1
   set_omb_rand_fac = 1.0
   seed_array1    = 0
   seed_array2    = 0
   sfc_assi_options = 1
   minimisation_option = 1
   calculate_cg_cost_fn = .true.
   write_outer_loop = .false.
   lat_stats_option = .true.

   as1(1) = 0.02
   as2(1) = 0.02
   as3(1) = 0.5
   as4(1) = 0.5
   as5(1) = 0.5

   as1(2) = 0.5
   as2(2) = 0.5
   as3(2) = 0.5
   as4(2) = 0.5
   as5(2) = 0.5

   as1(3) = 1.5
   as2(3) = 1.5
   as3(3) = 1.5
   as4(3) = 1.5
   as5(3) = 1.5

!--record12
   balance_type   = 1

!--record13
   vert_corr      = 2
   vertical_ip    = 0
   vert_evalue    = 1
   max_vert_var1  = 99.0
   max_vert_var2  = 99.0
   max_vert_var3  = 99.0
   max_vert_var4  = 99.0
   max_vert_var5  = 0.0

!--record14
   rtminit_print         = 1
   rtminit_nsensor       = 1
   rtminit_platform (:)  = -1
   rtminit_satid    (:)  = -1
   rtminit_sensor   (:)  = -1


!--pseudo_ob_nl
   num_pseudo     = 0
   pseudo_x       = 1.0
   pseudo_y       = 1.0
   pseudo_z       = 1.0
   pseudo_val     = 1.0
   pseudo_err     = 1.0
   pseudo_var     = 't'


!----------------------------------------------------------------------------
!  [1.0] Open namelist file:
!----------------------------------------------------------------------------

   namelist_file = 'namelist.3dvar'
   WRITE (UNIT=stdout,FMT='(3x,A,A)' ) ' 3DVAR namelist options used are in: ', namelist_file
   IOST = 0

   OPEN ( FILE   = namelist_file, UNIT   = namelist_unit, &
          STATUS = 'OLD' , ACCESS = 'SEQUENTIAL', &
          FORM   = 'FORMATTED', ACTION = 'READ', &
          IOSTAT = IOST )
     IF (iost /= 0) THEN
       CALL wrf_error_fatal3(__FILE__,__LINE__, &
         "ERROR OPENING NAMELIST FILE "//TRIM (namelist_file))
     END IF

!----------------------------------------------------------------------------
!  [2.0] Read namelist and close:
!----------------------------------------------------------------------------
   IOST = 0

   READ  ( UNIT = namelist_unit, NML = RECORD1 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF

   stdout = stdout_unit
   stderr = stderr_unit

   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,3X,A)')   ' MODEL_TYPE       = ', trim(MODEL_TYPE)
      WRITE(UNIT=stdout,FMT='(A,I4)')     ' print_detail     = ',print_detail
      WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' write_increments = ', write_increments
      WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' Global analysis option=', global         
      WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' VAR4d            =', lvar4d         
      WRITE(UNIT=stdout,FMT='(A,3X,I5)')  ' stdout_unit      =', stdout_unit     
      WRITE(UNIT=stdout,FMT='(A,3X,I5)')  ' stderr_unit      =', stderr_unit
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD2 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF

   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,3X,A6    )') ' ANALYSIS TYPE = ',ANALYSIS_TYPE
      WRITE(UNIT=stdout,FMT='(A,3X,A     )') ' ANALYSIS DATE = ',ANALYSIS_DATE
      WRITE(UNIT=stdout,FMT='(A,3X,i6    )') ' ANALYSIS ACCU = ',ANALYSIS_ACCU
      WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' W INCREMENTS  = ',W_INCREMENTS
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD3 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF

   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,3X,i4    )') ' fg_format     = ',    fg_format
      WRITE(UNIT=stdout,FMT='(A,3X,i4    )') ' ob_format     = ',    ob_format
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD4 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,3X,A8)') ' PROCESS_OBS      = ', PROCESS_OBS
      WRITE(UNIT=stdout,FMT='(A,3X,I4)') ' obs_qc_pointer   = ', obs_qc_pointer
      WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' Use_SynopObs     = ', Use_SynopObs, &
                                          ' Use_ShipsObs     = ', Use_ShipsObs, &
                                          ' Use_MetarObs     = ', Use_MetarObs, &
                                          ' Use_PilotObs     = ', Use_PilotObs, &
                                          ' Use_ProfilerObs  = ', Use_ProfilerObs, &
                                          ' Use_BuoyObs      = ', Use_BuoyObs, &
                                          ' Use_SoundObs     = ', Use_SoundObs, &
                                          ' Use_BogusObs     = ', Use_BogusObs, &
                                          ' Use_RadarObs     = ', Use_RadarObs, &
                                          ' Use_Radar_rv     = ', Use_Radar_rv, &
                                          ' Use_Radar_rf     = ', Use_Radar_rf, &
                                          ' Use_SatemObs     = ', Use_SatemObs, &
                                          ' Use_GeoAMVObs    = ', Use_GeoAMVObs, &
                                          ' Use_PolarAMVObs  = ', Use_PolarAMVObs, &
                                          ' Use_AirepObs     = ', Use_AirepObs, &
                                          ' Use_GpspwObs     = ', Use_GpspwObs, &
                                          ' Use_GpsrefObs    = ', Use_GpsrefObs, &
                                          ' Use_SsmiRetrievalObs =', Use_SsmiRetrievalObs, &
                                          ' Use_SsmiTbObs    = ', Use_SsmiTbObs, &
                                          ' Use_SSMT1Obs     = ', Use_SSMT1Obs, &
                                          ' Use_SSMT2Obs     = ', Use_SSMT2Obs, &
                                          ' use_qscatobs     = ', use_qscatobs, &
                                          ' use_Hirs2Obs     = ', use_Hirs2Obs , &
                                          ' use_Hirs3Obs     = ', use_Hirs3Obs , &
                                          ' use_MsuObs       = ', use_MsuObs , & 
                                          ' use_AmsuaObs     = ', use_AmsuaObs , &
                                          ' use_AmsubObs     = ', use_AmsubObs , &
                                          ' use_AirsObs      = ', use_AirsObs , &
                                          ' use_Eos_AmsuaObs = ', use_Eos_AmsuaObs , &
                                          ' use_Eos_RadObs   = ', use_Eos_RadObs , &
                                          ' use_HsbObs       = ', use_HsbObs , &
                                          ' check_max_iv     = ', check_max_iv, &
                                          ' use_obs_errfac   = ', use_obs_errfac, &
                                          ' put_rand_seed    = ', put_rand_seed, &
                                          ' omb_set_rand     = ', omb_set_rand, &
                                          ' omb_add_noise    = ', omb_add_noise
   end if

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
      use_Eos_RadObs .OR. use_HsbObs) THEN
    use_radiance = .TRUE.
  ELSE
    use_radiance = .FALSE.
  END IF


   READ  ( UNIT = namelist_unit, NML = RECORD5 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,1X,F6.2,A)') ' TIME_WINDOW   = ',TIME_WINDOW, ' hour(s)'
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD6 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' max_ext_its   = ',max_ext_its
      WRITE(UNIT=stdout,FMT='(A,3X,20E10.2)') ' EPS          = ',EPS
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' NTMAX         = ',NTMAX
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' NSAVE         = ',NSAVE
      WRITE(UNIT=stdout,FMT='(A,L10      )') ' WRITE_SWITCH  = ',write_switch
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' WRITE_INTERVAL= ',write_interval
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD7 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' RF_PASSES       = ',RF_PASSES
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING1 = ',var_scaling1
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING2 = ',var_scaling2
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING3 = ',var_scaling3
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING4 = ',var_scaling4
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING5 = ',var_scaling5
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING1 = ',len_scaling1
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING2 = ',len_scaling2
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING3 = ',len_scaling3
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING4 = ',len_scaling4
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING5 = ',len_scaling5
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD8 , IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A, L10)') ' def_sub_domain = ', def_sub_domain
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD10, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
! testing_dm_exact can be set to .true. to force DM_PARALLEL runs 
! to produce results that are bitwise-identical regardless of the number of 
! MPI tasks used.  This is useful for validating that multi-processor runs are 
! not a source of bugs.  Runtime will be much longer.  This option is 
! automatically overridden to .false. for serial or 1-MPI-task runs.  
   IF ( testing_dm_exact ) THEN
     CALL wrf_get_nproc( nproc )
     IF ( nproc == 1 ) THEN
       testing_dm_exact = .FALSE.
       IF ( print_detail >= 1 ) THEN
         WRITE(UNIT=stdout,FMT='(A)') &
           ' testing_dm_exact overridden to .FALSE. for serial or 1-MPI-task run'
       ENDIF
     ENDIF
   ENDIF

   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,L10)')               ' Testing_3DVAR = ',Testing_3DVAR
      WRITE(UNIT=stdout,FMT='(A,L10)')               ' Test_Inverse  = ',test_transforms
      WRITE(UNIT=stdout,FMT='(A,L10)')               ' Test_Statisti = ',test_statistics
      WRITE(UNIT=stdout,FMT='(A,L10)')               ' Interpolate_S = ',interpolate_stats
      WRITE(UNIT=stdout,FMT='(A,L10)')               ' testing_dm_exact = ',testing_dm_exact
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD11, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' Minimisation_Option = ', Minimisation_Option 
      WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' calculate_cg_cost_fn = ',calculate_cg_cost_fn
      WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Write_Outer_Loop    = ', write_outer_loop
      WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Lat_Stats_Option    = ', lat_stats_option
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' Balance_Type        = ', balance_type
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' CV_Options          = ', cv_options
      WRITE(UNIT=stdout,FMT='(A,3X,3F9.5 )') ' as1                 = ', as1(1:3), &
                            ' as2                 = ', as2(1:3), &
                            ' as3                 = ', as3(1:3), &
                            ' as4                 = ', as4(1:3), &
                            ' as5                 = ', as5(1:3)
      WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' set_omb_rand_fac    = ', set_omb_rand_fac
      WRITE(UNIT=stdout,FMT='(A,i16   )')    ' seed_array1         = ', seed_array1
      WRITE(UNIT=stdout,FMT='(A,i16   )')    ' seed_array2         = ', seed_array2
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' sfc_assi_optn       = ', sfc_assi_options
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' CV_Options_Hum      = ', cv_options_hum
      WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' check_rh            = ', check_rh
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD12, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I10)')      ' Balance_Type  = ',balance_type
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD13, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' vert_corr     = ',vert_corr
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' vertical_ip   = ',vertical_ip
      WRITE(UNIT=stdout,FMT='(A,I4       )') ' vert_evalue   = ',vert_evalue
      WRITE(UNIT=stdout,FMT='(A,5f10.3)')    ' Vert var limit= ',max_vert_var1, &
                                                   max_vert_var2, &
                                                   max_vert_var3, &
                                                   max_vert_var4, &
                                                   max_vert_var5
   end if

   READ  ( UNIT = namelist_unit, NML = RECORD14, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF
   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I4  )') ' rtminit_print    = ', rtminit_print
      WRITE(UNIT=stdout,FMT='(A,I4  )') ' rtminit_nsensor  = ', rtminit_nsensor
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_platform = ', rtminit_platform(1:rtminit_nsensor)
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_satid    = ', rtminit_satid   (1:rtminit_nsensor)
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_sensor   = ', rtminit_sensor  (1:rtminit_nsensor)
   end if
   
   READ  ( UNIT = namelist_unit, NML = pseudo_ob_nl, IOSTAT = IOST)
   IF (iost /= 0) THEN
     CALL wrf_error_fatal3(__FILE__,__LINE__, &
       "ERROR READING NAMELIST FILE "//TRIM (namelist_file))
   ENDIF

   if ( print_detail >= 1 ) then
      WRITE(UNIT=stdout,FMT='(A,I5)')               ' num_pseudo = ',   num_pseudo
      IF ( num_pseudo > 0 ) THEN
         WRITE(UNIT=stdout,FMT='(A,F12.2)')               ' pseudo_x   = ', pseudo_x
         WRITE(UNIT=stdout,FMT='(A,F12.2)')               ' pseudo_y   = ', pseudo_y
         WRITE(UNIT=stdout,FMT='(A,F12.2)')               ' pseudo_z   = ', pseudo_z
         WRITE(UNIT=stdout,FMT='(A,F12.2)')               ' pseudo_val = ', pseudo_val
         WRITE(UNIT=stdout,FMT='(A,F12.2)')               ' pseudo_err = ', pseudo_err
         WRITE(UNIT=stdout,FMT='(A,3x,A3)')     ' pseudo_var = ', pseudo_var
      end if
   end if
           
   if ( num_pseudo > 0 ) then
     CALL wrf_message("Single OBS Test:: Turn off all the OBS switches ***")
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
     Use_Obsgts           = .FALSE.
     Use_Radiance         = .FALSE.
   endif
  

   CLOSE (UNIT=namelist_unit)

   len = len_trim(ANALYSIS_DATE)

   if ( len < 24 ) then
      tmp_date(1:len) = ANALYSIS_DATE(1:len)
      ANALYSIS_DATE = tmp_date
   end if
      
!------------------------------------------------------------------------------
!  [3.0] Error traps:
!------------------------------------------------------------------------------

   IF ( cv_options < 1 .OR. cv_options > 5 ) THEN
     WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
       'Invalid cv_options = ', cv_options
     CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
   END IF
   
   IF ( cv_options_hum < 1 .OR. cv_options_hum > 3 ) THEN
     WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
       'Invalid cv_options_hum = ', cv_options_hum
     CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
   END IF

   IF ( sfc_assi_options < 1 .OR. sfc_assi_options > 2 ) THEN
     WRITE(UNIT=errmsg(1),FMT='(A,I3)') &
       'Invalid sfc_assi_option = ', sfc_assi_options
     CALL wrf_error_fatal3(__FILE__,__LINE__,errmsg(1))
   END IF

   IF (Use_SsmiRetrievalObs .OR. Use_SsmiTbObs) THEN
     OPEN ( UNIT   = ssmi_iunit,     &
          FORM   = 'FORMATTED',  &
          ACCESS = 'SEQUENTIAL', &
          IOSTAT =  iost,     &
          STATUS = 'OLD')
     CLOSE(UNIT=ssmi_iunit)

     IF (iost /= 0) THEN
       WRITE (UNIT=stderr,FMT='(/,A,I3,A,/)') &
         ' INPUT FILE UNIT ',ssmi_iunit, &
         ' FOR SSMI OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
       Use_SsmiRetrievalObs=.false.
       Use_SsmiTbObs=.false.

     ENDIF
     
   END IF 

   
   IF ( cv_options == 1 ) THEN
      IF ( cv_options_hum == 2 ) THEN
         CALL wrf_error_fatal3(__FILE__,__LINE__, &
           "Can only have cv_option_hum=2 with cv_option=2.")
      END IF
   
      IF ( vert_corr == 2 ) THEN
         WRITE (UNIT=stderr,FMT='(A,A)') ' DA_Read_Namelist: Invalid cv_options /', &
                           ' vert_corr combination. Set vert_corr=1'
         vert_corr = 1
      END IF
      
   END IF

   IF ( vert_corr == 2 ) THEN
      IF ( vertical_ip < 0 .OR. vertical_ip > 2 ) THEN
         WRITE (UNIT=stderr,FMT='(A,I3)') ' DA_Read_Namelist: Invalid vertical_ip = ', &
                           vertical_ip
      END IF
   END IF

   IF ( 0.5 * REAL(rf_passes) /= REAL(rf_passes / 2) ) THEN
      WRITE(UNIT=stdout,FMT='(A,I4,A)')' DA_Read_Namelist: rf_passes = ', &
                         rf_passes, ' .Should be even.'
      rf_passes = INT( REAL( rf_passes / 2 ) )
      WRITE(UNIT=stdout,FMT='(A,I4)') 'Resetting rf_passes = ', rf_passes
   END IF

   if ( analysis_type == 'randomcv' ) then
      ntmax = 0
      write(UNIT=stdout)' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

!----------------------------------------------------------------------------
!  [4.0] Print namelist:
!----------------------------------------------------------------------------
   
  if ( print_detail >= 1 ) then
   WRITE(UNIT=stdout,FMT='(A,3X,A  )') ' MODEL_TYPE       = ', trim(MODEL_TYPE)
   WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' write_increments = ', write_increments
   WRITE(UNIT=stdout,FMT='(A,3X,A6 )') ' ANALYSIS TYPE    = ', ANALYSIS_TYPE
   WRITE(UNIT=stdout,FMT='(A,3X,A  )') ' ANALYSIS DATE    = ', ANALYSIS_DATE
   WRITE(UNIT=stdout,FMT='(A,3X,i6 )') ' ANALYSIS ACCU    = ', ANALYSIS_ACCU
   WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' W INCREMENTS     = ', W_INCREMENTS
   WRITE(UNIT=stdout,FMT='(A,3X,i4 )') ' fg_format        = ', fg_format
   WRITE(UNIT=stdout,FMT='(A,3X,i4 )') ' ob_format        = ', ob_format
   WRITE(UNIT=stdout,FMT='(A,3X,A8 )') ' PROCESS_OBS      = ', PROCESS_OBS
   WRITE(UNIT=stdout,FMT='(A,3X,I4 )') ' obs_qc_pointer   = ', obs_qc_pointer
   WRITE(UNIT=stdout,FMT='(A,3X,L10)') ' Use_SynopObs     = ', Use_SynopObs, &
                                       ' Use_ShipsObs     = ', Use_ShipsObs, &
                                       ' Use_MetarObs     = ', Use_MetarObs, &
                                       ' Use_PilotObs     = ', Use_PilotObs, &
                                       ' Use_ProfilerObs  = ', Use_ProfilerObs, &
                                       ' Use_BuoyObs      = ', Use_BuoyObs, &
                                       ' Use_SoundObs     = ', Use_SoundObs, &
                                       ' Use_RadarObs     = ', Use_RadarObs, &
                                       ' Use_Radar_rv     = ', Use_Radar_rv, &
                                       ' Use_Radar_rf     = ', Use_Radar_rf, &
                                       ' Use_SatemObs     = ', Use_SatemObs, &
                                       ' Use_GeoAMVObs    = ', Use_GeoAMVObs, &
                                       ' Use_PolarAMVObs  = ', Use_PolarAMVObs, &
                                       ' Use_BogusObs     = ', Use_BogusObs, &
                                       ' Use_AirepObs     = ', Use_AirepObs, &
                                       ' Use_GpspwObs     = ', Use_GpspwObs, &
                                       ' Use_GpsrefObs    = ', Use_GpsrefObs, &
                                       ' Use_SsmiRetrievalObs =', Use_SsmiRetrievalObs, &
                                       ' Use_SsmiTbObs    = ', Use_SsmiTbObs, &
                                       ' Use_SSMT1Obs     = ', Use_SSMT1Obs, &
                                       ' Use_SSMT2Obs     = ', Use_SSMT2Obs, &
                                       ' use_qscatobs     = ', use_qscatobs, &
                                       ' use_Hirs2Obs     = ', use_hirs2Obs , &
                                       ' use_Hirs3Obs     = ', use_hirs3Obs, &
                                       ' use_MsuObs       = ', use_MsuObs, & 
                                       ' use_AmsuaObs     = ', use_AmsuaObs, & 
                                       ' use_AmsubObs     = ', use_AmsubObs, & 
                                       ' use_AirsObs      = ', use_AirsObs, & 
                                       ' use_Eos_AmsuaObs = ', use_Eos_AmsuaObs, & 
                                       ' use_Eos_RadObs   = ', use_Eos_RadObs, &  
                                       ' use_Eos_HsbObs   = ', use_HsbObs, & 
                                       ' check_max_iv     = ', check_max_iv, &
                                       ' use_obs_errfac   = ', use_obs_errfac, &
                                       ' put_rand_seed    = ', put_rand_seed, &
                                       ' omb_set_rand     = ', omb_set_rand, &
                                       ' omb_add_noise    = ', omb_add_noise, &
                                       ' use_radiance     = ', use_radiance, &
                                       ' use_obsgts       = ', use_obsgts
   WRITE(UNIT=stdout,FMT='(A,1X,F6.2,A)') ' TIME_WINDOW      = ',TIME_WINDOW, ' hour(s)'
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' print_detail     = ',print_detail
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' max_ext_its      = ',max_ext_its
   WRITE(UNIT=stdout,FMT='(A,3X,20E10.2)')' EPS              = ',EPS
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' NTMAX            = ',NTMAX
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' NSAVE            = ',NSAVE
   WRITE(UNIT=stdout,FMT='(A,L10      )') ' WRITE_SWITCH     = ',write_switch
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' WRITE_INTERVAL   = ',write_interval
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' RF_PASSES        = ',RF_PASSES
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING1 = ',var_scaling1
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING2 = ',var_scaling2
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING3 = ',var_scaling3
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING4 = ',var_scaling4
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_VAR_SCALING5 = ',var_scaling5
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING1 = ',len_scaling1
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING2 = ',len_scaling2
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING3 = ',len_scaling3
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING4 = ',len_scaling4
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' RF_LEN_SCALING5 = ',len_scaling5
   WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' Minimise _Option = ',Minimisation_Option 
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' In_CG_COST_FN_NEEDED = ',calculate_cg_cost_fn
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Outer_Loop_Write   = ',write_outer_loop
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Lat_Stats_Option   = ',lat_stats_option
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Testing_3DVAR = ',Testing_3DVAR
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Test_Inverse  = ',test_transforms
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Test_Statisti = ',test_statistics
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' Interpolate_S = ',interpolate_stats
   WRITE(UNIT=stdout,FMT='(A,3X,L10   )') ' testing_dm_exact = ',testing_dm_exact
   WRITE(UNIT=stdout,FMT='(A,3X,I10   )') ' Balance_Type  = ',balance_type
   WRITE(UNIT=stdout,FMT='(A,3X,I10   )') ' CV_Options    = ',cv_options
   WRITE(UNIT=stdout,FMT='(A,3X,3F9.5 )') ' as1           = ',as1(1:3), &
                                          ' as2           = ',as2(1:3), &   
                                          ' as3           = ',as3(1:3), &   
                                          ' as4           = ',as4(1:3), &   
                                          ' as5           = ',as5(1:3)      
   WRITE(UNIT=stdout,FMT='(A,1PE8.2   )') ' set_omb_rand_fac = ',set_omb_rand_fac
   WRITE(UNIT=stdout,FMT='(A,i16   )')    ' seed_array1   = ', seed_array1
   WRITE(UNIT=stdout,FMT='(A,i16   )')    ' seed_array2   = ', seed_array2
   WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' sfc_assi_optn = ', sfc_assi_options
   WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' CV_Options_Hum= ', cv_options_hum
   WRITE(UNIT=stdout,FMT='(A,3X,I4    )') ' check_rh      = ', check_rh
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' vert_corr     = ',vert_corr
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' vertical_ip   = ',vertical_ip
   WRITE(UNIT=stdout,FMT='(A,I4       )') ' vert_evalue   = ',vert_evalue
   WRITE(UNIT=stdout,FMT='(A,5f10.3)')    ' Vert var limit= ',max_vert_var1, &
                                                max_vert_var2, &
                                                max_vert_var3, &
                                                max_vert_var4, &
                                                max_vert_var5

      WRITE(UNIT=stdout,FMT='(A,I4  )') ' rtminit_print    = ', rtminit_print
      WRITE(UNIT=stdout,FMT='(A,I4  )') ' rtminit_nsensor  = ', rtminit_nsensor
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_platform = ', rtminit_platform(1:rtminit_nsensor)
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_satid    = ', rtminit_satid   (1:rtminit_nsensor)
      WRITE(UNIT=stdout,FMT='(A,10I4)') ' rtminit_sensor   = ', rtminit_sensor  (1:rtminit_nsensor)

   WRITE(UNIT=stdout,FMT='(A,I5)')        ' num_pseudo = ',   num_pseudo
   IF ( num_pseudo > 0 ) THEN
     WRITE(UNIT=stdout,FMT='(A,F12.2)')  ' pseudo_x   = ', pseudo_x
     WRITE(UNIT=stdout,FMT='(A,F12.2)')  ' pseudo_y   = ', pseudo_y
     WRITE(UNIT=stdout,FMT='(A,F12.2)')  ' pseudo_z   = ', pseudo_z
     WRITE(UNIT=stdout,FMT='(A,F12.2)')  ' pseudo_val = ', pseudo_val
     WRITE(UNIT=stdout,FMT='(A,F12.2)')  ' pseudo_err = ', pseudo_err
     WRITE(UNIT=stdout,FMT='(A,3x,A3)')  ' pseudo_var = ', pseudo_var
   end if
   
  end if
   
   WRITE(UNIT=stdout,FMT='(A)') "----------------------------------------"

   RETURN

END SUBROUTINE DA_Read_Namelist

