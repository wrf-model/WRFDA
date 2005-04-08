u!/bin/csh -f
#IBM:
# @ job_type   = parallel
## @ environment = COPY_ALL
# @ environment = MP_SHARED_MEMORY=true
# @ job_name   = wrf3dvar
# @ output     = wrf3dvar.out
# @ error      = wrf3dvar.err
# @ node       = 4
# @ network.MPI    = css0,shared,ip
# @ tasks_per_node = 8
# @ node_usage = not_shared
# @ checkpoint = no
# @ wall_clock_limit = 00:30:00
# NCEP IBM=dev
# NCAR IBM(bluesky)=com_rg8:
# @ class      =  com_rg8
# @ queue
#
#FSL JET (Alpha/Linux):
#PBS -V -A sfmlidar
#PBS -lnodes=4:comp -lwalltime=1000
#Uncomment for JET: source /usr/local/bin/setup-mpi.csh
#-----------------------------------------------------------------------
# Script DA_Run_3DVAR.csh
#
# Purpose: Top level script for running 3DVAR system
#
# Method:  1) Set up links to run directory.
#          2) Run 3DVAR in run directory.
#
# History: 11/16/99  Original version. Dale Barker
#          12/01/99  Modifications to defaults and names. Dale Barker
#          10/14/03  Modifications to defaults for WRF 3DVAR. Wei Huang
#
#-----------------------------------------------------------------------

#unlimit

 setenv MP_SHARED_MEMORY yes

 echo ""
 echo "Running script DA_Run_3DVAR"
 echo "---------------------------"
 echo ""

#-----------------------------------------------------------------------
# [1.0] Set up input/output file environment variables:
#-----------------------------------------------------------------------

#Specify job details here:

 set DA_OB_FORMAT	= 2		# Observation format: 1=BUFR, 2=ASCII "little_r"
 set DA_CV_OPTIONS	= 4		# 2 -> MM5, 3 -> WRF 4 -> Global
 set DA_GLOBAL		= .TRUE.	# true for global option
 set TRUNC		= T63           # Triangular truncation number 

 setenv DA_ANALYSIS_DATE	2004-10-27_06:00:00.0000  # Specify date in this format.
 setenv WEST_EAST_GRID_NUMBER	193                       # Number of gridpoints in x(i) dim.
 setenv SOUTH_NORTH_GRID_NUMBER	98                        # Number of gridpoints in y(j) dim.
 setenv VERTICAL_GRID_NUMBER	31                        # Number of vertical levels.
 setenv GRID_DISTANCE		190000                    # Grid resolution (m).

#Specify directories/files here:

 setenv WRF_DIR	/ptmp/n10450/wrfvar
 setenv DAT_DIR	/ptmp/n10450/kma_data
 setenv RUN_DIR /ptmp/n10450/test_kma_global

 setenv DA_FIRST_GUESS  ${DAT_DIR}/wrf_3dvar_input
 setenv DA_BACK_ERRORS  ${DAT_DIR}/be
 setenv DA_OBSERVATIONS	${DAT_DIR}/obs_gts-2004102712-full 

 setenv DA_SSMI		${DAT_DIR}/ssmi.dat                 # SSM/I radiances (ignore if not using).
 setenv DA_RADAR	${DAT_DIR}/radar.dat                # Radar data (ignore if not using).

############################################################################

 if ( ! -d $RUN_DIR ) then
  mkdir -p $RUN_DIR
 endif

 cd $RUN_DIR

#rm -rf $RUN_DIR/*
 cp $WRF_DIR/run/LANDUSE.TBL .
 cp $WRF_DIR/main/da_3dvar.exe  da_3dvar.exe
#cp $WRF_DIR/main/kma2netcdf.exe .                 
#cp $WRF_DIR/main/netcdf2kma.exe .                 

#Check settings:

  if ( ! $?DA_FIRST_GUESS ) then
    echo "DA_Run_3DVAR error: DA_FIRST_GUESS must be specified"
    exit 1
 endif

 if (( ! $?DA_OBSERVATIONS ) && (! $?DA_SSMI) ) then
    echo "DA_Run_3DVAR error: DA_OBSERVATIONS or DA_SSMI must be specified"
    exit 1
 endif

  if ( ! $?DA_BACK_ERRORS ) then
    echo "DA_Run_3DVAR error: DA_BACK_ERRORS must be specified"
    exit 1
 endif

 if(${DA_FG_FORMAT} == 1) then
    ln -sf $DA_FIRST_GUESS              wrf_3dvar_input
 else if(${DA_FG_FORMAT} == 2) then
    ln -sf $DA_FIRST_GUESS              fort.41
 else
    echo "DA_FG_FORMAT error: Do not know how to handle DA_FG_FORMAT = ${DA_FG_FORMAT}"
    exit 1
 endif
 ln -sf $DA_BACK_ERRORS		fort.3${DA_CV_OPTIONS}
 ln -sf $DA_OBSERVATIONS	fort.9${DA_OB_FORMAT}

 if ( -e $DA_SSMI) then
    ln -sf $DA_SSMI	fort.93
    set DA_USE_SSMIRETRIEVALOBS = .TRUE.
 endif

 echo "First Guess Input File:      $DA_FIRST_GUESS"
 echo "Background Error Input File: $DA_BACK_ERRORS"
 echo "Observation Input File:      $DA_OBSERVATIONS"

#-----------------------------------------------------------------------
# [2.0] Set up details of analysis algorithm:
#-----------------------------------------------------------------------

 if ( ! $?DA_MODEL_TYPE )   set DA_MODEL_TYPE = WRF
 if ( ! $?DA_WRITE_INCREMENTS ) set DA_WRITE_INCREMENTS = .FALSE.
 if ( ! $?DA_ANALYSIS_TYPE ) set DA_ANALYSIS_TYPE = 3D-VAR
 if ( ! $?DA_ANALYSIS_DATE ) set DA_ANALYSIS_DATE = 2002-08-29_00:00:00.0000
 if ( ! $?DA_ANALYSIS_ACCU ) set DA_ANALYSIS_ACCU = 900
 if ( ! $?DA_FG_FORMAT )     set DA_FG_FORMAT = 2
 if ( ! $?DA_OB_FORMAT )     set DA_OB_FORMAT = 2
 if ( ! $?DA_NUM_FGAT_TIME ) set DA_NUM_FGAT_TIME = 1
 if ( ! $?DA_PROCESS_OBS )   set DA_PROCESS_OBS = YES
 if ( ! $?DA_QC_POINTER )    set DA_QC_POINTER = 0
 if ( ! $?DA_USE_SYNOPOBS )  set DA_USE_SYNOPOBS = .TRUE. 
 if ( ! $?DA_USE_SHIPSOBS )  set DA_USE_SHIPSOBS = .TRUE. 
 if ( ! $?DA_USE_METAROBS )  set DA_USE_METAROBS = .TRUE. 
 if ( ! $?DA_USE_PILOTOBS )  set DA_USE_PILOTOBS = .TRUE.
 if ( ! $?DA_USE_SOUNDOBS )  set DA_USE_SOUNDOBS = .TRUE.
 if ( ! $?DA_USE_SATEMOBS )  set DA_USE_SATEMOBS = .TRUE.
 if ( ! $?DA_USE_SATOBOBS )  set DA_USE_SATOBOBS = .TRUE. 
 if ( ! $?DA_USE_AIREPOBS )  set DA_USE_AIREPOBS = .TRUE.
 if ( ! $?DA_USE_GPSPWOBS )  set DA_USE_GPSPWOBS = .TRUE.
 if ( ! $?DA_USE_RADAROBS )  set DA_USE_RADAROBS = .FALSE.
 if ( ! $?DA_Use_Radar_rv )  set DA_Use_Radar_rv = .FALSE.
 if ( ! $?DA_Use_Radar_rf )  set DA_Use_Radar_rf = .FALSE.
 if ( ! $?DA_USE_GPSREFOBS )  set DA_USE_GPSREFOBS = .FALSE.
 if ( ! $?DA_USE_PROFILEROBS )  set DA_USE_PROFILEROBS = .TRUE.
 if ( ! $?DA_USE_BUOYOBS     )  set DA_USE_BUOYOBS     = .TRUE.
 if ( ! $?DA_USE_SSMIRETRIEVALOBS  )  set DA_USE_SSMIRETRIEVALOBS = .FALSE.
 if ( ! $?DA_USE_SSMITBOBS ) set DA_USE_SSMITBOBS = .FALSE.
 if ( ! $?DA_USE_SSMT1OBS ) set DA_USE_SSMT1OBS = .FALSE.  
 if ( ! $?DA_USE_SSMT2OBS ) set DA_USE_SSMT2OBS = .FALSE.
 if ( ! $?DA_USE_QSCATOBS ) set DA_USE_QSCATOBS = .FALSE.
 if ( ! $?DA_Check_Max_IV )  set DA_Check_Max_IV        = .TRUE.
 if ( ! $?DA_USE_OBS_ERRFAC ) set DA_USE_OBS_ERRFAC = .FALSE.
 if ( ! $?DA_PUT_RAND_SEED ) set DA_PUT_RAND_SEED = .FALSE.
 if ( ! $?DA_OMB_SET_RAND ) set DA_OMB_SET_RAND = .FALSE.
 if ( ! $?DA_OMB_ADD_NOISE ) set DA_OMB_ADD_NOISE = .FALSE.
 if ( ! $?DA_TIME_WINDOW )   set DA_TIME_WINDOW = 3.
 if ( ! $?DA_PRINT_DETAIL )  set DA_PRINT_DETAIL = 0
 if ( ! $?DA_MAX_EXT_ITS )   set DA_MAX_EXT_ITS = 1
 if ( ! $?DA_W_INCREMENTS ) set DA_W_INCREMENTS = .TRUE.
 if ( ! $?DA_DT_CLOUD_MODEL ) set DA_DT_CLOUD_MODEL = .FALSE.
 if ( ! $?DA_EPS )           set DA_EPS = "1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02,"
 if ( ! $?DA_NTMAX )         set DA_NTMAX = 100
 if ( ! $?DA_WRITE_SWITCH )  set DA_WRITE_SWITCH   = .FALSE.
 if ( ! $?DA_WRITE_INTERVAL )set DA_WRITE_INTERVAL = 5
 if ( ! $?DA_WRITE_QCW )     set DA_WRITE_QCW = .FALSE.
 if ( ! $?DA_WRITE_QRN )     set DA_WRITE_QRN = .FALSE.
 if ( ! $?DA_WRITE_QCI )     set DA_WRITE_QCI = .FALSE.
 if ( ! $?DA_WRITE_QSN )     set DA_WRITE_QSN = .FALSE.
 if ( ! $?DA_WRITE_QGR )     set DA_WRITE_QGR = .FALSE.
 if ( ! $?DA_RF_PASSES )     set DA_RF_PASSES = 4
 if ( ! $?DA_VAR_SCALING1 )  set DA_VAR_SCALING1 = 1.5
 if ( ! $?DA_VAR_SCALING2 )  set DA_VAR_SCALING2 = 1.5
 if ( ! $?DA_VAR_SCALING3 )  set DA_VAR_SCALING3 = 1.5
 if ( ! $?DA_VAR_SCALING4 )  set DA_VAR_SCALING4 = 1.5
 if ( ! $?DA_VAR_SCALING5 )  set DA_VAR_SCALING5 = 1.5
 if ( ! $?DA_LEN_SCALING1 )  set DA_LEN_SCALING1 = 0.5
 if ( ! $?DA_LEN_SCALING2 )  set DA_LEN_SCALING2 = 0.5
 if ( ! $?DA_LEN_SCALING3 )  set DA_LEN_SCALING3 = 0.5
 if ( ! $?DA_LEN_SCALING4 )  set DA_LEN_SCALING4 = 0.5
 if ( ! $?DA_LEN_SCALING5 )  set DA_LEN_SCALING5 = 0.5
 if ( ! $?DA_DEF_SUB_DOMAIN )set DA_DEF_SUB_DOMAIN = .FALSE.
 if ( ! $?DA_X_START_SUB_DOMAIN )set DA_X_START_SUB_DOMAIN = 55.0
 if ( ! $?DA_Y_START_SUB_DOMAIN )set DA_Y_START_SUB_DOMAIN = 35.0
 if ( ! $?DA_X_END_SUB_DOMAIN )set DA_X_END_SUB_DOMAIN = 80.0
 if ( ! $?DA_Y_END_SUB_DOMAIN )set DA_Y_END_SUB_DOMAIN = 60.0
 if ( ! $?DA_TESTING_3DVAR ) set DA_TESTING_3DVAR = .FALSE.
 if ( ! $?DA_TEST_TRANSFORMS )  set DA_TEST_TRANSFORMS = .FALSE.
 if ( ! $?DA_TEST_STATISTICS )  set DA_TEST_STATISTICS = .FALSE. 
 if ( ! $?DA_INTERPOLATE_STATS )  set DA_INTERPOLATE_STATS = .TRUE.
 if ( ! $?DA_MINIMISATION_OPTION) set DA_MINIMISATION_OPTION = 2
 if ( ! $?DA_WRITE_OUTER_LOOP) set DA_WRITE_OUTER_LOOP = .FALSE.
 if ( ! $?DA_LAT_STATS_OPTION) set DA_LAT_STATS_OPTION = .FALSE.
 if ( ! $?DA_CALCULATE_CG_COST_FUNCTION) set DA_CALCULATE_CG_COST_FUNCTION = .FALSE.
 if ( ! $?DA_CV_OPTIONS )    set DA_CV_OPTIONS = 2
 if ( ! $?DA_CV_OPTIONS_HUM ) set DA_CV_OPTIONS_HUM = 1
 if ( ! $?DA_CHECK_RH )      set DA_CHECK_RH = 2
 if ( ! $?DA_as1        )    set DA_as1 = "0.25, 0.75, 1.5"
 if ( ! $?DA_as2        )    set DA_as2 = "0.25, 0.75, 1.5"
 if ( ! $?DA_as3        )    set DA_as3 = "0.25, 0.75, 1.5"
 if ( ! $?DA_as4        )    set DA_as4 = "0.25, 0.75, 1.5"
 if ( ! $?DA_as5        )    set DA_as5 = "0.25, 0.75, 1.5"
 if ( ! $?DA_SFC_ASSI_OPTIONS )  set DA_SFC_ASSI_OPTIONS = 1
 if ( ! $?DA_SET_OMB_RAND_FAC )  set DA_SET_OMB_RAND_FAC = 1.0
 if ( ! $?DA_SEED_ARRAY1 )   set DA_SEED_ARRAY1 = 0
 if ( ! $?DA_SEED_ARRAY2 )   set DA_SEED_ARRAY2 = 0
 if ( ! $?DA_BALANCE_TYPE )  set DA_BALANCE_TYPE = 1
 if ( ! $?DA_VERT_CORR )     set DA_VERT_CORR = 2
 if ( ! $?DA_VERTICAL_IP )   set DA_VERTICAL_IP = 0
 if ( ! $?DA_VERT_EVALUE )   set DA_VERT_EVALUE = 1
 if ( ! $?DA_MAX_VERT_VAR1 ) set DA_MAX_VERT_VAR1 = 99.0
 if ( ! $?DA_MAX_VERT_VAR2 ) set DA_MAX_VERT_VAR2 = 99.0
 if ( ! $?DA_MAX_VERT_VAR3 ) set DA_MAX_VERT_VAR3 = 99.0
 if ( ! $?DA_MAX_VERT_VAR4 ) set DA_MAX_VERT_VAR4 = 99.0
 if ( ! $?DA_MAX_VERT_VAR5 ) set DA_MAX_VERT_VAR5 = 99.0
 if ( ! $?DA_NUM_PSEUDO ) set DA_NUM_PSEUDO = 0
 if ( ! $?DA_PSEUDO_X ) set DA_PSEUDO_X = 165.0
 if ( ! $?DA_PSEUDO_Y ) set DA_PSEUDO_Y =  65.0
 if ( ! $?DA_PSEUDO_Z ) set DA_PSEUDO_Z =  15.0
 if ( ! $?DA_PSEUDO_VAL ) set DA_PSEUDO_VAL = 1.0
 if ( ! $?DA_PSEUDO_ERR ) set DA_PSEUDO_ERR = 1.0
 if ( ! $?DA_PSEUDO_VAR ) set DA_PSEUDO_VAR = u

cat >! namelist.3dvar << EOF
&record1
 MODEL_TYPE = '$DA_MODEL_TYPE',
 WRITE_INCREMENTS = $DA_WRITE_INCREMENTS ,
 GLOBAL           = $DA_GLOBAL           /

&record2
 ANALYSIS_TYPE = '$DA_ANALYSIS_TYPE',
 ANALYSIS_DATE = '$DA_ANALYSIS_DATE',
 ANALYSIS_ACCU =  $DA_ANALYSIS_ACCU,
 W_INCREMENTS  = $DA_W_INCREMENTS,
 DT_CLOUD_MODEL = $DA_DT_CLOUD_MODEL,
 WRITE_QCW      = $DA_WRITE_QCW,
 WRITE_QRN      = $DA_WRITE_QRN,
 WRITE_QCI      = $DA_WRITE_QCI,
 WRITE_QSN      = $DA_WRITE_QSN,
 WRITE_QGR      = $DA_WRITE_QGR /

&record3
 fg_format = $DA_FG_FORMAT,
 ob_format = $DA_OB_FORMAT,
 num_fgat_time = $DA_NUM_FGAT_TIME /

&record4
 PROCESS_OBS    = '$DA_PROCESS_OBS',
 obs_qc_pointer = $DA_QC_POINTER,
 Use_SynopObs   = $DA_USE_SYNOPOBS,
 Use_ShipsObs   = $DA_USE_SHIPSOBS,
 Use_MetarObs   = $DA_USE_METAROBS,
 Use_PilotObs   = $DA_USE_PILOTOBS,
 Use_SoundObs   = $DA_USE_SOUNDOBS,
 Use_SatemObs   = $DA_USE_SATEMOBS,
 Use_SatobObs   = $DA_USE_SATOBOBS,
 Use_AirepObs   = $DA_USE_AIREPOBS,
 Use_GpspwObs   = $DA_USE_GPSPWOBS,
 Use_GpsrefObs   = $DA_USE_GPSREFOBS,
 Use_ProfilerObs = $DA_USE_PROFILEROBS, 
 Use_BuoyObs     = $DA_USE_BUOYOBS,
 Use_BogusObs   = $DA_USE_BOGUSOBS,
 Use_SsmiRetrievalObs = $DA_USE_SSMIRETRIEVALOBS,
 Use_SsmiTbObs  = $DA_USE_SSMITBOBS,
 use_ssmt1obs   = $DA_USE_SSMT1OBS,
 use_ssmt2obs   = $DA_USE_SSMT2OBS,
 use_qscatobs   = $DA_USE_QSCATOBS,
 use_radarobs   = $DA_USE_RADAROBS,
 Use_Radar_rv   = $DA_Use_Radar_rv,
 Use_Radar_rf   = $DA_Use_Radar_rf,
 check_max_iv   = $DA_Check_Max_IV,
 use_obs_errfac = $DA_USE_OBS_ERRFAC,
 put_rand_seed  = $DA_PUT_RAND_SEED,
 omb_set_rand   = $DA_OMB_SET_RAND,
 omb_add_noise  = $DA_OMB_ADD_NOISE /

&record5
 TIME_WINDOW    = $DA_TIME_WINDOW,
 PRINT_DETAIL   = $DA_PRINT_DETAIL /

&record6
 max_ext_its    = $DA_MAX_EXT_ITS,
 EPS            = $DA_EPS,
 NTMAX          = $DA_NTMAX,
 NSAVE          = 4,
 WRITE_SWITCH   = $DA_WRITE_SWITCH,
 WRITE_INTERVAL = $DA_WRITE_INTERVAL /

&record7
 RF_PASSES      = $DA_RF_PASSES,
 VAR_SCALING1   = $DA_VAR_SCALING1,
 VAR_SCALING2   = $DA_VAR_SCALING2,
 VAR_SCALING3   = $DA_VAR_SCALING3,
 VAR_SCALING4   = $DA_VAR_SCALING4,
 VAR_SCALING5   = $DA_VAR_SCALING5,
 LEN_SCALING1   = $DA_LEN_SCALING1,
 LEN_SCALING2   = $DA_LEN_SCALING2,
 LEN_SCALING3   = $DA_LEN_SCALING3,
 LEN_SCALING4   = $DA_LEN_SCALING4,
 LEN_SCALING5   = $DA_LEN_SCALING5 /

&record8
 def_sub_domain = $DA_DEF_SUB_DOMAIN,
 x_start_sub_domain = $DA_X_START_SUB_DOMAIN,
 y_start_sub_domain = $DA_Y_START_SUB_DOMAIN,
 x_end_sub_domain   = $DA_X_END_SUB_DOMAIN,
 y_end_sub_domain   = $DA_Y_END_SUB_DOMAIN /

&record10
 Testing_3DVAR  = $DA_TESTING_3DVAR,
 Test_Transforms = $DA_TEST_TRANSFORMS,
 Test_Statistics = $DA_TEST_STATISTICS,
 Interpolate_Stats = $DA_INTERPOLATE_STATS /
 
&record11
 minimisation_option = $DA_MINIMISATION_OPTION,
 write_outer_loop    = $DA_WRITE_OUTER_LOOP,
 lat_stats_option    = $DA_LAT_STATS_OPTION,
 calculate_cg_cost_function = $DA_CALCULATE_CG_COST_FUNCTION,
 cv_options     = $DA_CV_OPTIONS,
 cv_options_hum = $DA_CV_OPTIONS_HUM,
 check_rh       = $DA_CHECK_RH,
 as1            = $DA_as1,
 as2            = $DA_as2,
 as3            = $DA_as3,
 as4            = $DA_as4,
 as5            = $DA_as5,
sfc_assi_options = $DA_SFC_ASSI_OPTIONS,
 set_omb_rand_fac = $DA_SET_OMB_RAND_FAC,
 seed_array1    = $DA_SEED_ARRAY1,
 seed_array2    = $DA_SEED_ARRAY2 /
 
&record12
 balance_type   = $DA_BALANCE_TYPE /
 
&record13
 vert_corr      = $DA_VERT_CORR,
 vertical_ip    = $DA_VERTICAL_IP,
 vert_evalue    = $DA_VERT_EVALUE,
 max_vert_var1  = $DA_MAX_VERT_VAR1,
 max_vert_var2  = $DA_MAX_VERT_VAR2,
 max_vert_var3  = $DA_MAX_VERT_VAR3,
 max_vert_var4  = $DA_MAX_VERT_VAR4,
 max_vert_var5  = $DA_MAX_VERT_VAR5 /
 
&pseudo_ob_nl
 num_pseudo     = $DA_NUM_PSEUDO, 
 pseudo_x       = $DA_PSEUDO_X,
 pseudo_y       = $DA_PSEUDO_Y,
 pseudo_z       = $DA_PSEUDO_Z,
 pseudo_val     = $DA_PSEUDO_VAL,
 pseudo_err     = $DA_PSEUDO_ERR,
 pseudo_var     = '$DA_PSEUDO_VAR' /

EOF

#set date
 set DA_CY = `echo $DA_ANALYSIS_DATE | cut -c1-4`
 set DA_MM = `echo $DA_ANALYSIS_DATE | cut -c6-7`
 set DA_DD = `echo $DA_ANALYSIS_DATE | cut -c9-10`
 set DA_HH = `echo $DA_ANALYSIS_DATE | cut -c12-13`

cat >! namelist.input << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = 12,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $DA_CY, $DA_CY, $DA_CY,
 start_month                         = $DA_MM, $DA_MM, $DA_MM,
 start_day                           = $DA_DD, $DA_DD, $DA_DD,
 start_hour                          = $DA_HH, $DA_HH, $DA_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $DA_CY, $DA_CY, $DA_CY,
 end_month                           = $DA_MM, $DA_MM, $DA_MM,
 end_day                             = $DA_DD, $DA_DD, $DA_DD,
 end_hour                            = $DA_HH, $DA_HH, $DA_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 21600,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 180,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 10
 /

 &domains
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1, 1, 1,
 e_we                                = $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER, $WEST_EAST_GRID_NUMBER,
 s_sn                                = 1, 1, 1,
 e_sn                                = $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER,
 s_vert                              = 1, 1, 1,
 e_vert                              = $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER,
 dx                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 dy                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 grid_id                             = 1,     2,     3,
 level                               = 1,     1,     2,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    30,
 j_parent_start                      = 0,     20,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0,
 nproc_y                             = 1
 /

 &physics
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 10,    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 num_soil_layers                     = 4,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = 0,
 diff_opt                            = 0,
 km_opt                              = 1,
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.01,   0.01,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 smdiv                               = 0.1,    0.1,    0.1,
 emdiv                               = 0.01,   0.01,   0.01,
 epssm                               = 0.1,    0.1,    0.1
 non_hydrostatic                     = .true., .true., .true.,
 time_step_sound                     = 4,      4,      4,
 h_mom_adv_order                     = 5,      5,      5,
 v_mom_adv_order                     = 3,      3,      3,
 h_sca_adv_order                     = 5,      5,      5,
 v_sca_adv_order                     = 3,      3,      3,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 periodic_x                          = .false.,.false.,.false.,
 symmetric_xs                        = .false.,.false.,.false.,
 symmetric_xe                        = .false.,.false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 symmetric_ys                        = .false.,.false.,.false.,
 symmetric_ye                        = .false.,.false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
 nested                              = .false., .true., .true.,
 real_data_init_type                 = $DA_FG_FORMAT
/
 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF

#-----------------------------------------------------------------------
# [1.0] Run KMA2NETCDF   Procedure
 echo "Running KMA2NETCDF ------"
#-----------------------------------------------------------------------
#Uncomment for particular machine:
#DEC, SGI, Linux/PC multiPE:
#
##  This fiel is for T213
##   ln -sf ${IN_DIR}/g21_modl_glewv.${DA_CY}${DA_MM}${DA_DD}${DA_HH} fort.1
##  This fiel is for T63
##   ln -sf ${IN_DIR}/T63_out.${DA_CY}${DA_MM}${DA_DD}${DA_HH} fort.1
#    ln -sf /mmmtmp2/hcshin/O-B/T63_out.${DA_CY}${DA_MM}${DA_DD}${DA_HH} fort.1
## Following file on unit 3 is for for T213 only
## Since for T63 the topo data is read from history file itself (fort.1)
##   ln -sf ${IN_DIR}/ghm_modl_mtn.dat               fort.3
#    ln -sf ${IN_DIR}/w2gconv_name-T63.${DA_HH}Z     fort.95
##   ./kma2netcdf.exe >&! kma2netcdf.out
#  cp wrf_3dvar_output ${DA_FIRST_GUESS}
#  cp fort.61          ${OUT_DIR}/W2GE_DATA
#  cp fort.610         ${OUT_DIR}/k2n0.ASC
#  cp rsl.out.0000     rsl.out.kma2netcdf
#  echo "KMA2NETCDF    completed"
#-----------------------------------------------------------------------
# [2.0] Run 3DVAR:
 echo "Running  3DVAR  ------"
#-----------------------------------------------------------------------
#DEC, SGI, Linux/PC multiPE:
#  ./da_3dvar.exe >&! da_3dvar.out
#cp wrf_3dvar_output ANL_NETCDF_DATA
#cp fort.71          ANL_INCREMENTS
#cp fort.710         FORMATTED_ANL_INCREMENTS
#cp fort.12          DAProg_3DVAR.statistics
#cp fort.81          DAProg_3DVAR.cost_fn
#cp fort.82          DAProg_3DVAR.grad_fn
#BIG : mpirun -np 4 ./da_3dvar.exe
#    mpirun -np 8 ./da_3dvar.exe
#IBM (llsubmit):
  poe ./da_3dvar.exe
#AFWA: setenv LOADL_INTERACTIVE_CLASS 1
#AFWA: poe ./da_3dvar.exe -euilib us -hostfile host.afwa -procs 15
#echo "3DVAR    completed"
#-----------------------------------------------------------------------
#Create job script for cray x1.
cat >! run_3dvar.sh << EOF
#!/bin/sh
# PBSPRO SETUP
#PBS -lmppe=4,walltime=1:00:00,mem=10Gb
####PBS -lmppssp=416,walltime=2:00:00,mem=400Gb
####PBS -lmppe=4
#PBS -V
#PBS -j oe
                                                                                
 cd \$PBS_O_WORKDIR

NPROC=4

set -x

. /opt/modules/modules/init/sh

module purge
module load modules
module use /opt/modulefiles
module load PrgEnv.53.newest apptools

FILENV=.assign
export FILENV
assign -R
assign -Y on f:namelist.input
assign -Y on f:namelist.3dvar

rm -f rsl.* show* wrf_3dvar_output

export X1_STACK_SIZE=1200000000
export X1_HEAP_SIZE=12000000000

date

aprun -p16m:16m -c core=0 -n \$NPROC da_3dvar.exe

date

 mv rsl.out.0000 p\${NPROC}_rsl.out.0000
 mv rsl.error.0000 p\${NPROC}_rsl.error.0000
EOF

qsub run_3dvar.sh

exit
#-----------------------------------------------------------------------
# [3.0] Run NETCDF2KMA   Procedure
#echo "Running  NETCDF2KMA  ------"
#-----------------------------------------------------------------------
#ln -sf ${IN_DIR}/analysis_date         fort.94
#ln -sf ${IN_DIR}/gau2gwv213_namelist   fort.95
# This file is for T213 which holds gaussian topography
#   ln -sf ${IN_DIR}/ghm_modl_mtn.dat           fort.3
# This file is for T63 which holds gaussian topography
#ln -sf ${IN_DIR}/T63_TOPO_gaus.2004051906    fort.3
#ln -sf ${OUT_DIR}/ANL_NETCDF_DATA wrf_3dvar_input
#ln -sf ${OUT_DIR}/ANL_INCREMENTS             fort.101  # INC at equal lat/lon
#ln -sf ${IN_DIR}/wg6_modl_gleln.2004051906   fort.102  # FG at gaussian lat/lon
#ln -sf ${OUT_DIR}/n2k.out         fort.901    #shc1
#ln -sf ${OUT_DIR}/lt2.out         fort.902    #shc1
#ln -sf ${OUT_DIR}/gwv.out         fort.903    #shc1
#ln -sf ${OUT_DIR}/n2F.out         fort.904    #shc1
#ln -sf ${OUT_DIR}/n2k1.ASC        fort.881    #shc1
#ln -sf ${OUT_DIR}/n2k2.ASC        fort.882    #shc1
#  ./netcdf2kma.exe >&! netcdf2kma.out
#cp fort.11 ${OUT_DIR}/KMA_HIST_DATA
#cp fort.12 ${OUT_DIR}/W2GG_DATA
#cp rsl.out.0000     rsl.out.netcdf2kma
#echo "NETCDF2KMA    completed"
# exit (0)
