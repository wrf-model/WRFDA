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
!----------------------------------------------------------------------------
   IMPLICIT NONE

!  Local scalars:

   CHARACTER*20              :: namelist_file      ! Input namelist filename.
   INTEGER, PARAMETER        :: namelist_unit = 7  ! Input namelist unit.
   INTEGER                   :: iost               ! Error code.

   CHARACTER(LEN=24)         :: tmp_date
   INTEGER                   :: len

!  Namelist contents :

   NAMELIST /RECORD1/  MODEL_TYPE, write_increments, global
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
                       use_SatobObs, &
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
                       check_max_iv, &
                       use_obs_errfac, &
                       put_rand_seed, &
                       omb_set_rand, &
                       omb_add_noise, &
                       max_sound_input, &
                       max_synop_input, &
                       max_satob_input, &
                       max_Radar_input, &
                       max_airep_input, &
                       max_satem_input, &
                       max_pilot_input, &
                       max_metar_input, &
                       max_gpspw_input, &
                       max_ships_input, &
                       max_profiler_input, &
                       max_buoy_input, &
                       max_ssmi_retrieval_input, &
                       max_ssmi_tb_input, &
                       max_ssmt1_input, &
                       max_ssmt2_input, &
                       max_qscat_input 
   NAMELIST /RECORD5/  TIME_WINDOW, print_detail, print_obs_info
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
                       test_statistics, interpolate_stats
   NAMELIST /RECORD11/ cv_options, cv_options_hum, check_rh, &
                       as1,as2,as3,as4,as5, set_omb_rand_fac, &
                       seed_array1, seed_array2, &
                       sfc_assi_options, calculate_cg_cost_function, &
                       minimisation_option, write_outer_loop,lat_stats_option
   NAMELIST /RECORD12/ balance_type
   NAMELIST /RECORD13/ vert_corr, vertical_ip, vert_evalue, &
                       max_vert_var1, max_vert_var2, &
                       max_vert_var3, max_vert_var4, max_vert_var5
   NAMELIST /pseudo_ob_nl/ num_pseudo, pseudo_x, pseudo_y, pseudo_z, &
                           pseudo_val, pseudo_err, pseudo_var

!----------------------------------------------------------------------------------
   write(unit=*, fmt='(a)') &
    ' -------------------------', &
    ' [1.0] Read 3DVAR namelist', &
    ' -------------------------'

!  Namelist defaults:

!--record1
   MODEL_TYPE       = 'WRF'
   WRITE_INCREMENTS = .FALSE.

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
   global = .FALSE. 

!--record4
   PROCESS_OBS    = 'YES'
   obs_qc_pointer = 0
   Use_SynopObs   = .TRUE.
   Use_ShipsObs   = .TRUE.
   Use_MetarObs   = .TRUE.
   Use_SoundObs   = .TRUE.
   Use_PilotObs   = .TRUE.
   Use_AirepObs   = .TRUE.
   Use_SatobObs   = .TRUE.
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
   check_max_iv   = .TRUE.
   use_obs_errfac = .FALSE.
   put_rand_seed  = .FALSE.
   omb_set_rand   = .FALSE.
   omb_add_noise  = .FALSE.

   max_sound_input = max_sound
   max_synop_input = max_synop
   max_satob_input = max_satob
   max_airep_input = max_airep
   max_satem_input = max_satem
   max_pilot_input = max_pilot
   max_Radar_input = max_Radar
   max_metar_input = max_metar
   max_gpspw_input = max_gpspw
   max_ships_input = max_ships
   max_profiler_input = max_profiler
   max_buoy_input = max_buoy
   max_ssmi_retrieval_input = max_ssmi_retrieval
   max_ssmi_tb_input = max_ssmi_tb
   max_ssmt1_input = max_ssmt1
   max_ssmt2_input = max_ssmt2
   max_qscat_input = max_qscat
   max_gpsref_input = max_gpsref

!--record5
   TIME_WINDOW    = 3.
   PRINT_DETAIL   = 0
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
   calculate_cg_cost_function = .true.
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
   WRITE (6, '(3x,A,A)' ) ' 3DVAR namelist options used are in: ', namelist_file
   IOST = 0

   OPEN ( FILE   = namelist_file, UNIT   = namelist_unit, &
          STATUS = 'OLD' , ACCESS = 'SEQUENTIAL', &
          FORM   = 'FORMATTED', ACTION = 'READ', &
          ERR    = 8000, IOSTAT = IOST )

!----------------------------------------------------------------------------
!  [2.0] Read namelist and close:
!----------------------------------------------------------------------------
   IOST = 0
   READ  ( UNIT = namelist_unit, NML = RECORD1 , ERR = 8000, IOSTAT = IOST)

   if ( print_detail >= 1 ) then
      WRITE(6,'(A,3X,A     )') ' MODEL_TYPE    = ', trim(MODEL_TYPE)
      WRITE(6,'(A,3X,L10   )') ' write_increments = ', write_increments
      WRITE(6,'(A,3X,L5    )') ' Global analysis option=', global         
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD2 , ERR = 8000, IOSTAT = IOST)

   if ( print_detail >= 1 ) then
      WRITE(6,'(A,3X,A6    )') ' ANALYSIS TYPE = ',ANALYSIS_TYPE
      WRITE(6,'(A,3X,A     )') ' ANALYSIS DATE = ',ANALYSIS_DATE
      WRITE(6,'(A,3X,i6    )') ' ANALYSIS ACCU = ',ANALYSIS_ACCU
      WRITE(6,'(A,3X,L10   )') ' W INCREMENTS  = ',W_INCREMENTS
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD3 , ERR = 8000, IOSTAT = IOST)

   if ( print_detail >= 1 ) then
      WRITE(6,'(A,3X,i4    )') ' fg_format     = ',    fg_format
      WRITE(6,'(A,3X,i4    )') ' ob_format     = ',    ob_format
   endif

   READ  ( UNIT = namelist_unit, NML = RECORD4 , ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,3X,A8    )') ' PROCESS_OBS   = ',PROCESS_OBS
      WRITE(6,'(A,3X,I4    )') ' obs_qc_pointer= ',obs_qc_pointer
      WRITE(6,'(A,3X,L10   )') ' Use_SynopObs  =', Use_SynopObs, &
                               ' Use_ShipsObs  =', Use_ShipsObs, &
                               ' Use_MetarObs  =', Use_MetarObs, &
                               ' Use_PilotObs  =', Use_PilotObs, &
                               ' Use_ProfilerObs  =', Use_ProfilerObs, &
                               ' Use_BuoyObs  =', Use_BuoyObs, &
                               ' Use_SoundObs  =', Use_SoundObs, &
                               ' Use_RadarObs  =', Use_RadarObs, &
                               ' Use_Radar_rv  =', Use_Radar_rv, &
                               ' Use_Radar_rf  =', Use_Radar_rf, &
                               ' Use_SatemObs  =', Use_SatemObs, &
                               ' Use_SatobObs  =', Use_SatobObs, &
                               ' Use_AirepObs  =', Use_AirepObs, &
                               ' Use_GpspwObs  =', Use_GpspwObs, &
                               ' Use_GpsrefObs  =', Use_GpsrefObs, &
                               ' Use_SsmiRetrievalObs =', Use_SsmiRetrievalObs, &
                               ' Use_SsmiTbObs =', Use_SsmiTbObs, &
                               ' Use_SSMT1Obs  =', Use_SSMT1Obs, &
                               ' Use_SSMT2Obs  =', Use_SSMT2Obs, &
                               ' use_qscatobs  =', use_qscatobs, &
                               ' check_max_iv  =', check_max_iv, &
                               ' use_obs_errfac=', use_obs_errfac, &
                               ' put_rand_seed =', put_rand_seed, &
                               ' omb_set_rand =', omb_set_rand, &
                               ' omb_add_noise=', omb_add_noise
   end if

   READ  ( UNIT = namelist_unit, NML = RECORD5 , ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,1X,F6.2,A)') ' TIME_WINDOW   = ',TIME_WINDOW, ' hour(s)'
      WRITE(6,'(A,I4       )') ' print_detail  = ',print_detail
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD6 , ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,I4       )') ' max_ext_its   = ',max_ext_its
      WRITE(6,'(A,3X,20E10.2)') ' EPS          = ',EPS
      WRITE(6,'(A,I4       )') ' NTMAX         = ',NTMAX
      WRITE(6,'(A,I4       )') ' NSAVE         = ',NSAVE
      WRITE(6,'(A,L10      )') ' WRITE_SWITCH  = ',write_switch
      WRITE(6,'(A,I4       )') ' WRITE_INTERVAL= ',write_interval
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD7 , ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,I4       )') ' RF_PASSES     = ',RF_PASSES
      WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING1 = ',var_scaling1
      WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING2 = ',var_scaling2
      WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING3 = ',var_scaling3
      WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING4 = ',var_scaling4
      WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING5 = ',var_scaling5
      WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING1 = ',len_scaling1
      WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING2 = ',len_scaling2
      WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING3 = ',len_scaling3
      WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING4 = ',len_scaling4
      WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING5 = ',len_scaling5
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD8 , ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A, L10)') ' def_sub_domain = ', def_sub_domain
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD10, ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,*)               ' Testing_3DVAR = ',Testing_3DVAR
      WRITE(6,*)               ' Test_Inverse  = ',test_transforms
      WRITE(6,*)               ' Test_Statisti = ',test_statistics
      WRITE(6,*)               ' Interpolate_S = ',interpolate_stats
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD11, ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,3X,I4    )') ' Minimisation_Option = ',Minimisation_Option 
      WRITE(6,'(A,3X,L10   )') ' calculate_cg_cost_function = ',calculate_cg_cost_function
      WRITE(6,'(A,3X,L10   )') ' Write_Outer_Loop   = ',write_outer_loop
      WRITE(6,'(A,3X,L10   )') ' Lat_Stats_Option   = ',lat_stats_option
      WRITE(6,'(A,3X,I4    )') ' Balance_Type  = ',balance_type
      WRITE(6,'(A,3X,I4    )') ' CV_Options    = ',cv_options
      WRITE(6,'(A,3X,3F9.5 )') ' as1           = ',as1(1:3), &
                               ' as2           = ',as2(1:3), &
                               ' as3           = ',as3(1:3), &
                               ' as4           = ',as4(1:3), &
                               ' as5           = ',as5(1:3)
      WRITE(6,'(A,1PE8.2   )') ' set_omb_rand_fac = ',set_omb_rand_fac
      WRITE(6,'(A,i16   )')    ' seed_array1 = ', seed_array1
      WRITE(6,'(A,i16   )')    ' seed_array2 = ', seed_array2
      WRITE(6,'(A,3X,I4    )') ' sfc_assi_optn = ',sfc_assi_options
      WRITE(6,'(A,3X,I4    )') ' CV_Options_Hum= ',cv_options_hum
      WRITE(6,'(A,3X,I4    )') ' check_rh      = ',check_rh
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD12, ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,*)               ' Balance_Type  = ',balance_type
   end if
   
   READ  ( UNIT = namelist_unit, NML = RECORD13, ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,'(A,I4       )') ' vert_corr     = ',vert_corr
      WRITE(6,'(A,I4       )') ' vertical_ip   = ',vertical_ip
      WRITE(6,'(A,I4       )') ' vert_evalue   = ',vert_evalue
      WRITE(6,'(A,5f10.3)')    ' Vert var limit= ',max_vert_var1, &
                                                   max_vert_var2, &
                                                   max_vert_var3, &
                                                   max_vert_var4, &
                                                   max_vert_var5
   end if
   
   READ  ( UNIT = namelist_unit, NML = pseudo_ob_nl, ERR = 8000, IOSTAT = IOST)
   
   if ( print_detail >= 1 ) then
      WRITE(6,*)               ' num_pseudo = ',   num_pseudo
      IF ( num_pseudo > 0 ) THEN
         WRITE(6,*)               ' pseudo_x   = ', pseudo_x
         WRITE(6,*)               ' pseudo_y   = ', pseudo_y
         WRITE(6,*)               ' pseudo_z   = ', pseudo_z
         WRITE(6,*)               ' pseudo_val = ', pseudo_val
         WRITE(6,*)               ' pseudo_err = ', pseudo_err
         WRITE(6,'(A,3x,A3)')     ' pseudo_var = ', pseudo_var
      end if
   end if
           
   if ( num_pseudo > 0 ) then
     WRITE(6,'(/5X,a/)') &
     '*** Single OBS Test:: Turn off all the OBS switches ***'  
     Use_SynopObs         = .FALSE.
     Use_ShipsObs         = .FALSE.
     Use_MetarObs         = .FALSE.
     Use_SoundObs         = .FALSE.
     Use_PilotObs         = .FALSE.
     Use_AirepObs         = .FALSE.
     Use_SatobObs         = .FALSE.
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
     check_max_iv   = .FALSE.
   endif

   CLOSE (namelist_unit)

   len = len_trim(ANALYSIS_DATE)

   if ( len < 24 ) then
      tmp_date(1:len) = ANALYSIS_DATE(1:len)
      ANALYSIS_DATE = tmp_date
   end if
      
!------------------------------------------------------------------------------
!  [3.0] Error traps:
!------------------------------------------------------------------------------

   IF ( cv_options < 1 .OR. cv_options > 4 ) THEN
      WRITE(0,'(A,I3)')' DA_Read_Namelist: Invalid cv_options = ', cv_options
      STOP
   END IF
   
   IF ( cv_options_hum < 1 .OR. cv_options_hum > 3 ) THEN
      WRITE(0,'(A,I3)')' DA_Read_Namelist: Invalid cv_options_hum = ', &
                       cv_options_hum
      STOP
   END IF

   IF ( sfc_assi_options < 1 .OR. sfc_assi_options > 2 ) THEN
      WRITE(0,'(A,I3)')' DA_Read_Namelist: Invalid sfc_assi_option = ', &
                       sfc_assi_options
      STOP
   END IF

   IF (Use_SsmiRetrievalObs .OR. Use_SsmiTbObs) THEN
     OPEN ( UNIT   = ssmi_iunit,     &
          FORM   = 'FORMATTED',  &
          ACCESS = 'SEQUENTIAL', &
          IOSTAT =  iost,     &
          STATUS = 'OLD')
     CLOSE(ssmi_iunit)

     IF (iost /= 0) THEN
         WRITE (0,'(/,A,I3,A,/)') &
       ' INPUT FILE UNIT ',ssmi_iunit, &
       ' FOR SSMI OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         Use_SsmiRetrievalObs=.false.
         Use_SsmiTbObs=.false.

     ENDIF
     
   END IF 

   
   IF ( cv_options == 1 ) THEN
      IF ( cv_options_hum == 2 ) THEN
         WRITE(0,'(/,A,/,A,/)')' DA_Setup_Background_Errors: Can only have', &
                               ' cv_option_hum=2 with cv_option=2.'
         STOP
      END IF
   
      IF ( vert_corr == 2 ) THEN
         WRITE (0,'(A,A)') ' DA_Read_Namelist: Invalid cv_options /', &
                           ' vert_corr combination. Set vert_corr=1'
         vert_corr = 1
      END IF
      
   END IF

   IF ( vert_corr == 2 ) THEN
      IF ( vertical_ip < 0 .OR. vertical_ip > 2 ) THEN
         WRITE (0,'(A,I3)') ' DA_Read_Namelist: Invalid vertical_ip = ', &
                           vertical_ip
      END IF
   END IF

   IF ( 0.5 * REAL(rf_passes) /= REAL(rf_passes / 2) ) THEN
      WRITE(6,'(A,I4,A)')' DA_Read_Namelist: rf_passes = ', &
                         rf_passes, ' .Should be even.'
      rf_passes = INT( REAL( rf_passes / 2 ) )
      WRITE(6,'(A,I4)') 'Resetting rf_passes = ', rf_passes
   END IF

   if ( analysis_type == 'randomcv' ) then
      ntmax = 0
      write(6,*)' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

!----------------------------------------------------------------------------
!  [4.0] Print namelist:
!----------------------------------------------------------------------------
   
   if ( print_detail >= 1 ) then
   WRITE(6,'(A,3X,A     )') ' MODEL_TYPE    = ', trim(MODEL_TYPE)
   WRITE(6,'(A,3X,L10   )') ' write_increments = ', write_increments
   WRITE(6,'(A,3X,A6    )') ' ANALYSIS TYPE = ',ANALYSIS_TYPE
   WRITE(6,'(A,3X,A     )') ' ANALYSIS DATE = ',ANALYSIS_DATE
   WRITE(6,'(A,3X,i6    )') ' ANALYSIS ACCU = ',ANALYSIS_ACCU
   WRITE(6,'(A,3X,L10   )') ' W INCREMENTS  = ',W_INCREMENTS
   WRITE(6,'(A,3X,i4    )') ' fg_format     = ',    fg_format
   WRITE(6,'(A,3X,i4    )') ' ob_format     = ',    ob_format
   WRITE(6,'(A,3X,A8    )') ' PROCESS_OBS   = ',PROCESS_OBS
   WRITE(6,'(A,3X,I4    )') ' obs_qc_pointer= ',obs_qc_pointer
   WRITE(6,'(A,3X,L10   )') ' Use_SynopObs  =', Use_SynopObs, &
                            ' Use_ShipsObs  =', Use_ShipsObs, &
                            ' Use_MetarObs  =', Use_MetarObs, &
                            ' Use_PilotObs  =', Use_PilotObs, &
                            ' Use_ProfilerObs  =', Use_ProfilerObs, &
                            ' Use_BuoyObs  =', Use_BuoyObs, &
                            ' Use_SoundObs  =', Use_SoundObs, &
                            ' Use_RadarObs  =', Use_RadarObs, &
                            ' Use_Radar_rv  =', Use_Radar_rv, &
                            ' Use_Radar_rf  =', Use_Radar_rf, &
                            ' Use_SatemObs  =', Use_SatemObs, &
                            ' Use_SatobObs  =', Use_SatobObs, &
                            ' Use_AirepObs  =', Use_AirepObs, &
                            ' Use_GpspwObs  =', Use_GpspwObs, &
                            ' Use_GpsrefObs  =', Use_GpsrefObs, &
                            ' Use_SsmiRetrievalObs =', Use_SsmiRetrievalObs, &
                            ' Use_SsmiTbObs =', Use_SsmiTbObs, &
                            ' Use_SSMT1Obs  =', Use_SSMT1Obs, &
                            ' Use_SSMT2Obs  =', Use_SSMT2Obs, &
                            ' use_qscatobs  =', use_qscatobs, &
                            ' check_max_iv  =', check_max_iv, &
                            ' use_obs_errfac=', use_obs_errfac, &
                            ' put_rand_seed =', put_rand_seed, &
                            ' omb_set_rand =', omb_set_rand, &
                            ' omb_add_noise=', omb_add_noise
   WRITE(6,'(A,1X,F6.2,A)') ' TIME_WINDOW   = ',TIME_WINDOW, ' hour(s)'
   WRITE(6,'(A,I4       )') ' print_detail  = ',print_detail
   WRITE(6,'(A,I4       )') ' max_ext_its   = ',max_ext_its
   WRITE(6,'(A,3X,20E10.2)') ' EPS          = ',EPS
   WRITE(6,'(A,I4       )') ' NTMAX         = ',NTMAX
   WRITE(6,'(A,I4       )') ' NSAVE         = ',NSAVE
   WRITE(6,'(A,L10      )') ' WRITE_SWITCH  = ',write_switch
   WRITE(6,'(A,I4       )') ' WRITE_INTERVAL= ',write_interval
   WRITE(6,'(A,I4       )') ' RF_PASSES     = ',RF_PASSES
   WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING1 = ',var_scaling1
   WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING2 = ',var_scaling2
   WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING3 = ',var_scaling3
   WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING4 = ',var_scaling4
   WRITE(6,'(A,1PE8.2   )') ' RF_VAR_SCALING5 = ',var_scaling5
   WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING1 = ',len_scaling1
   WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING2 = ',len_scaling2
   WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING3 = ',len_scaling3
   WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING4 = ',len_scaling4
   WRITE(6,'(A,1PE8.2   )') ' RF_LEN_SCALING5 = ',len_scaling5
   WRITE(6,'(A,3X,L10   )') ' Minimise _Option = ',Minimisation_Option 
   WRITE(6,'(A,3X,L10   )') ' In_CG_COST_FUNCTION_NEEDED = ',calculate_cg_cost_function
   WRITE(6,'(A,3X,L10   )') ' Outer_Loop_Write   = ',write_outer_loop
   WRITE(6,'(A,3X,L10   )') ' Lat_Stats_Option   = ',lat_stats_option
   WRITE(6,'(A,3X,L10   )') ' Testing_3DVAR = ',Testing_3DVAR
   WRITE(6,'(A,3X,L10   )') ' Test_Inverse  = ',test_transforms
   WRITE(6,'(A,3X,L10   )') ' Test_Statisti = ',test_statistics
   WRITE(6,'(A,3X,L10   )') ' Interpolate_S = ',interpolate_stats
   WRITE(6,'(A,3X,I10   )') ' Balance_Type  = ',balance_type
   WRITE(6,'(A,3X,I10   )') ' CV_Options    = ',cv_options
   WRITE(6,'(A,3X,3F9.5 )') ' as1           = ',as1(1:3), &
                            ' as2           = ',as2(1:3), &
                            ' as3           = ',as3(1:3), &
                            ' as4           = ',as4(1:3), &
                            ' as5           = ',as5(1:3)
   WRITE(6,'(A,1PE8.2   )') ' set_omb_rand_fac = ',set_omb_rand_fac
   WRITE(6,'(A,i16   )')    ' seed_array1 = ', seed_array1
   WRITE(6,'(A,i16   )')    ' seed_array2 = ', seed_array2
   WRITE(6,*)               ' sfc_assi_optn = ',sfc_assi_options
   WRITE(6,*)               ' CV_Options_Hum= ',cv_options_hum
   WRITE(6,*)               ' check_rh      = ',check_rh
   WRITE(6,'(A,I4       )') ' vert_corr     = ',vert_corr
   WRITE(6,'(A,I4       )') ' vertical_ip   = ',vertical_ip
   WRITE(6,'(A,I4       )') ' vert_evalue   = ',vert_evalue
   WRITE(6,'(A,5f10.3)')    ' Vert var limit= ',max_vert_var1, &
                                                max_vert_var2, &
                                                max_vert_var3, &
                                                max_vert_var4, &
                                                max_vert_var5

   WRITE(6,*)               ' num_pseudo = ',   num_pseudo
   IF ( num_pseudo > 0 ) THEN
      WRITE(6,*)               ' pseudo_x   = ', pseudo_x
      WRITE(6,*)               ' pseudo_y   = ', pseudo_y
      WRITE(6,*)               ' pseudo_z   = ', pseudo_z
      WRITE(6,*)               ' pseudo_val = ', pseudo_val
      WRITE(6,*)               ' pseudo_err = ', pseudo_err
      WRITE(6,'(A,3x,A3)')     ' pseudo_var = ', pseudo_var
   end if
   
   end if
   
   WRITE(6,'(/)')

   RETURN

!----------------------------------------------------------------------------
!  [4.0] Report error:
!----------------------------------------------------------------------------

8000  WRITE (0,'(/,A,A,A,I3,/)') ' ERROR READING NAMELIST FILE ', &
                                 TRIM (namelist_file),' IOSTAT = ', IOST

   STOP                       ' in DA_Read_Namelist.f90'

END SUBROUTINE DA_Read_Namelist

