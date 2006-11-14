#! /bin/csh -f
#IBM:
# @ job_type   = parallel
## @ environment = COPY_ALL
# @ environment = MP_SHARED_MEMORY=true
# @ job_name   = wrfvar
# @ output     = wrfvar.out
# @ error      = wrfvar.err
# @ node       = 1
## @ network.MPI    = css0,shared,us
# @ network.MPI    = css0,shared,ip
# @ tasks_per_node = 16
# @ node_usage = not_shared
# @ checkpoint = no
# @ wall_clock_limit = 01:30:00
# NCEP IBM=dev
# NCAR IBM(bluesky)=com_rg8:
# NCAR IBM(blackforest)=com_reg:
# NCAR IBM(blackforest_nighthawk)=com_nh:
# @ class      =  com_nh
## @ class      =  com_reg
# @ queue
#
#FSL JET (Alpha/Linux):
#PBS -V -A sfmlidar
#PBS -lnodes=4:comp -lwalltime=1000
#Uncomment for JET: source /usr/local/bin/setup-mpi.csh
#-----------------------------------------------------------------------
# Script DA_Run_WRF-Var.csh
#
# Purpose: Top level script for running the WRF-Var system
#
# Method:  1) Set up links to run directory.
#          2) Run WRF-Var in run directory.
#
# History: 11/16/99  Original version. Dale Barker
#          12/01/99  Modifications to defaults and names. Dale Barker
#          10/14/03  Modifications to defaults for WRF 3DVAR. Wei Huang
#          06/06/05  Modifications for Polar & Geo AMV's      Syed RH Rizvi
#          07/15/05  Tidy up prior to WRF V2.1 release, rename DA_Run_WRF-Var.csh. Dale Barker
#
#-----------------------------------------------------------------------

#set echo

 unlimit

 setenv MP_SHARED_MEMORY yes

 echo ""
 echo "Running script DA_Run_WRF-Var"
 echo "-----------------------------"
 echo ""

#-----------------------------------------------------------------------
# USER: Define non-default job via environment variables: 
#-----------------------------------------------------------------------

#e.g.: setenv DAT_DIR /users/dmbarker/data overrides the default below.
setenv WRFVAR_DIR ${HOME}/code_development/WRF_V2.1/wrfvar.devel.eotd.start
setenv WRFVAR_DIR ${HOME}/code_development/WRF_V2.1/wrfvar.devel.eotd
#set DA_TEST_TRANSFORMS = .TRUE.
setenv PERIODIC_X .true.
setenv DA_ENSDIM 1
setenv DA_ALPHA_TRUNCATION 95
setenv DA_ALPHA_CORR_TYPE 3
setenv DA_ALPHA_CORR_SCALE 1500.0
setenv DA_ID wrfvar.sondes.alpha1.je1.1
setenv DA_ALPHA_STD_DEV 1.0
setenv DA_JB_FACTOR 1.0
setenv DA_JE_FACTOR 1.0
setenv DAT_DIR    /ocotillo1/${user}/data/KMA_T63_input_2004102712
#setenv DA_OBSERVATIONS ${DAT_DIR}/obs_gts.3dvar.2004102712.singlet

##########################################################################
#USER: DO NOT MAKE CHANGES BELOW (if you do, you're on your own!) 
##########################################################################

#-----------------------------------------------------------------------
# [1.0] Specify default environment variables:
#-----------------------------------------------------------------------

 if ( ! $?START_DATE )              setenv START_DATE 2004102712       # Analysis date.
 if ( ! $?DA_CV_OPTIONS )           setenv DA_CV_OPTIONS 4             # Background error statistics: 2=NCAR, 3=NCEP.

#Default directories/files:
 if ( ! $?SRC_DIR )    setenv SRC_DIR    ${HOME}/code_development
 if ( ! $?WRFVAR_DIR ) setenv WRFVAR_DIR ${SRC_DIR}/wrfvar
 if ( ! $?DAT_DIR )    setenv DAT_DIR    /ocotillo1/${user}/data/KMA_T63_input_2004102712
 if ( ! $?DA_ID )      setenv DA_ID wrfvar
 if ( ! $?RUN_DIR )    setenv RUN_DIR    ${DAT_DIR}
 if ( ! -d $RUN_DIR )  mkdir $RUN_DIR

 if ( ! $?DA_FIRST_GUESS ) setenv DA_FIRST_GUESS ${DAT_DIR}/wrfinput_d01.${START_DATE} # wrfvar "first guess" input.
 if ( ! $?DA_OBSERVATIONS ) setenv DA_OBSERVATIONS ${DAT_DIR}/obs_gts.3dvar.${START_DATE} # wrfvar observation input.
 if ( ! $?DA_BACK_ERRORS ) setenv DA_BACK_ERRORS ${DAT_DIR}/be.cv_${DA_CV_OPTIONS}    # wrfvar background errors.
 if ( ! $?DA_SSMI ) setenv DA_SSMI ${DAT_DIR}/ssmi.dat         # SSM/I radiances (ignore if not using).
 if ( ! $?DA_RADAR) setenv DA_RADAR ${DAT_DIR}/radar.dat       # Radar data (ignore if not using).

#Default WRF namelist variables:
 if ( ! $?NUM_PROCS ) setenv NUM_PROCS 1                          # Number of processors to run on.
 if ( ! $?WEST_EAST_GRID_NUMBER )   setenv WEST_EAST_GRID_NUMBER 193   # X grid dimension.
 if ( ! $?SOUTH_NORTH_GRID_NUMBER ) setenv SOUTH_NORTH_GRID_NUMBER 98  # Y grid dimension.
 if ( ! $?VERTICAL_GRID_NUMBER ) setenv VERTICAL_GRID_NUMBER 31   # Z grid dimension.
 if ( ! $?GRID_DISTANCE ) setenv GRID_DISTANCE 190000             # Grid resolution (m).
 if ( ! $?DA_SF_SURFACE_PHYSICS ) setenv DA_SF_SURFACE_PHYSICS 1  #(1=Thermal diffusion, 2=Noah LSM).
 if ( $DA_SF_SURFACE_PHYSICS == 1 ) setenv DA_NUM_SOIL_LAYERS 5   # (Thermal diffusion surface physics).
 if ( $DA_SF_SURFACE_PHYSICS == 2 ) setenv DA_NUM_SOIL_LAYERS 4   # (Noah LSM surface physics).

#Supported default WRF-Var namelist variables:
 if ( ! $?DA_FG_FORMAT )  setenv DA_FG_FORMAT 3                   # First guess format: 1=WRF, 2=MM5, 3=KMA
 if ( ! $?DA_OB_FORMAT )  setenv DA_OB_FORMAT 2                   # Observation format: 1=BUFR, 2=ASCII "little_r"
 if ( ! $?DA_GLOBAL )     setenv DA_GLOBAL .TRUE.                 # Regional/global domain.
 if ( ! $?NPROC_X )       setenv NPROC_X 0                        # Regional, always set NPROC_X to 0, Global, always 1
 if (   $DA_GLOBAL == ".TRUE.") setenv NPROC_X 1
 if ( ! $?DA_MODEL_TYPE ) setenv DA_MODEL_TYPE WRF                # WRF, MM5 or KMA.
 if ( ! $?DA_WRITE_INCREMENTS ) setenv DA_WRITE_INCREMENTS .TRUE.# Optionally write increments.
 if ( ! $?DA_NUM_FGAT_TIME ) setenv DA_NUM_FGAT_TIME 1            # Number of FGAT ob windows.
 if ( ! $?DA_ENSDIM )        setenv DA_ENSDIM 0                   # Ensemble size.
 if ( ! $?DA_ALPHA_TRUNCATION ) setenv DA_ALPHA_TRUNCATION 0      # Number of FGAT ob windows.
 if ( ! $?DA_ALPHA_CORR_TYPE )  setenv DA_ALPHA_CORR_TYPE 3       # Alpha correlation type.
 if ( ! $?DA_ALPHA_CORR_SCALE ) setenv DA_ALPHA_CORR_SCALE 1500.0 # Alpha correlation scale (km).
 if ( ! $?DA_ALPHA_STD_DEV    ) setenv DA_ALPHA_STD_DEV    1.0    # Alpha standard deviation.
 if ( ! $?DA_JE_FACTOR )        setenv DA_JE_FACTOR    1.0        # Je factor.
 if ( ! $?DA_USE_SYNOPOBS )  set DA_USE_SYNOPOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SHIPSOBS )  set DA_USE_SHIPSOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_METAROBS )  set DA_USE_METAROBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_PILOTOBS )  set DA_USE_PILOTOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SOUNDOBS )  set DA_USE_SOUNDOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SATEMOBS )  set DA_USE_SATEMOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_GEO_AMV  )  set DA_USE_GEO_AMV =  .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_POLAR_AMV ) set DA_USE_POLAR_AMV = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_AIREPOBS )  set DA_USE_AIREPOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_GPSPWOBS )  set DA_USE_GPSPWOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_USE_RADAROBS )  set DA_USE_RADAROBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_Use_Radar_rv )  set DA_Use_Radar_rv = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_Use_Radar_rf )  set DA_Use_Radar_rf = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_BOGUSOBS )  set DA_USE_BOGUSOBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_GPSREFOBS ) set DA_USE_GPSREFOBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_PROFILEROBS )set DA_USE_PROFILEROBS = .FALSE.     # Assimilate this observation type.
 if ( ! $?DA_USE_BUOYOBS )   set DA_USE_BUOYOBS = .FALSE.          # Assimilate this observation type.
 if ( ! $?DA_USE_SSMIRETRIEVALOBS )set DA_USE_SSMIRETRIEVALOBS = .FALSE. # Assimilate this observation type.
 if ( ! $?DA_USE_SSMITBOBS ) set DA_USE_SSMITBOBS = .FALSE.       # Assimilate this observation type.
 if ( ! $?DA_USE_SSMT1OBS )  set DA_USE_SSMT1OBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_SSMT2OBS )  set DA_USE_SSMT2OBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_QSCATOBS )  set DA_USE_QSCATOBS = .FALSE.         # Assimilate this observation type.
 if ( ! $?DA_Check_Max_IV )  set DA_Check_Max_IV = .TRUE.         # Perform O-B > 5sigma_o QC if true.
 if ( ! $?DA_MAX_EXT_ITS )   set DA_MAX_EXT_ITS = 1               # Maximum number of "WRF-Var outer loops".
 if ( ! $?DA_EPS )           set DA_EPS = "1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02," # Convergence criteria.
 if ( ! $?DA_NTMAX )         set DA_NTMAX = 100                   # Maximum number of inner loop iterations.
 if ( ! $?DA_RF_PASSES )     set DA_RF_PASSES = 6                 # Number of recursive filter passes.
 if ( ! $?DA_TESTING_3DVAR ) set DA_TESTING_3DVAR = .FALSE.       # Test WRF-Var code.
 if ( ! $?DA_TEST_TRANSFORMS )  set DA_TEST_TRANSFORMS = .FALSE.  # Test WRF-Var transforms.
 if ( ! $?DA_TEST_STATISTICS )  set DA_TEST_STATISTICS = .FALSE.  # Test WRF-Var statistics.
 if ( ! $?DA_INTERPOLATE_STATS )set DA_INTERPOLATE_STATS = .TRUE. # True if statistics computed on different domain.
 if ( ! $?DA_MINIMISATION_OPTION) set DA_MINIMISATION_OPTION = 2  # 1=Quasi-Newton, 2=Conjugate gradient.
 if ( ! $?DA_CALCULATE_CG_COST_FUNCTION) set DA_CALCULATE_CG_COST_FUNCTION = .TRUE. # True if want CG diagnostic output. 
 if ( ! $?DA_CV_OPTIONS_HUM ) set DA_CV_OPTIONS_HUM = 3           # Moist control variable (1-3).
 if ( ! $?DA_CHECK_RH )      set DA_CHECK_RH = 1                  # Physical check on humidity (0-2).
 if ( ! $?DA_MAX_VERT_VAR1 ) set DA_MAX_VERT_VAR1 = 99.0          # CV1 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR2 ) set DA_MAX_VERT_VAR2 = 99.0          # CV2 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR3 ) set DA_MAX_VERT_VAR3 = 99.0          # CV3 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR4 ) set DA_MAX_VERT_VAR4 = 99.0          # CV4 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR5 ) set DA_MAX_VERT_VAR5 = 99.0          # CV5 variance truncation.

#Not supported default WRF-Var namelist variables (overwrite at your own risk):
 if ( ! $?DA_ANALYSIS_TYPE ) set DA_ANALYSIS_TYPE = 3D-VAR
 if ( ! $?DA_ANALYSIS_ACCU ) set DA_ANALYSIS_ACCU = 900
 if ( ! $?DA_PROCESS_OBS )   set DA_PROCESS_OBS = YES
 if ( ! $?DA_QC_POINTER )    set DA_QC_POINTER = 0
 if ( ! $?DA_USE_OBS_ERRFAC ) set DA_USE_OBS_ERRFAC = .FALSE.
 if ( ! $?DA_PUT_RAND_SEED ) set DA_PUT_RAND_SEED = .FALSE.
 if ( ! $?DA_OMB_SET_RAND ) set DA_OMB_SET_RAND = .FALSE.
 if ( ! $?DA_OMB_ADD_NOISE ) set DA_OMB_ADD_NOISE = .FALSE.
 if ( ! $?DA_TIME_WINDOW )   set DA_TIME_WINDOW = 3.      
 if ( ! $?DA_PRINT_DETAIL )  set DA_PRINT_DETAIL = 0
 if ( ! $?DA_WRITE_SWITCH )  set DA_WRITE_SWITCH = .FALSE.
 if ( ! $?DA_WRITE_INTERVAL )set DA_WRITE_INTERVAL = 5
 if ( ! $?DA_WRITE_QCW )     set DA_WRITE_QCW = .FALSE.
 if ( ! $?DA_WRITE_QRN )     set DA_WRITE_QRN = .FALSE.
 if ( ! $?DA_WRITE_QCI )     set DA_WRITE_QCI = .FALSE.
 if ( ! $?DA_WRITE_QSN )     set DA_WRITE_QSN = .FALSE.
 if ( ! $?DA_WRITE_QGR )     set DA_WRITE_QGR = .FALSE.
 if ( ! $?DA_VAR_SCALING1 )  set DA_VAR_SCALING1 = 1.0
 if ( ! $?DA_VAR_SCALING2 )  set DA_VAR_SCALING2 = 1.0
 if ( ! $?DA_VAR_SCALING3 )  set DA_VAR_SCALING3 = 1.0
 if ( ! $?DA_VAR_SCALING4 )  set DA_VAR_SCALING4 = 1.0
 if ( ! $?DA_VAR_SCALING5 )  set DA_VAR_SCALING5 = 1.0
 if ( ! $?DA_LEN_SCALING1 )  set DA_LEN_SCALING1 = 1.0
 if ( ! $?DA_LEN_SCALING2 )  set DA_LEN_SCALING2 = 1.0
 if ( ! $?DA_LEN_SCALING3 )  set DA_LEN_SCALING3 = 1.0
 if ( ! $?DA_LEN_SCALING4 )  set DA_LEN_SCALING4 = 1.0
 if ( ! $?DA_LEN_SCALING5 )  set DA_LEN_SCALING5 = 1.0
 if ( ! $?DA_POWER_TRUNCATION ) set DA_POWER_TRUNCATION = 1.0
 if ( ! $?DA_JB_FACTOR )        setenv DA_JB_FACTOR    1.0        # Je factor.
 if ( ! $?DA_JE_FACTOR )        setenv DA_JE_FACTOR    1.0        # Je factor.
 if ( ! $?DA_DEF_SUB_DOMAIN )set DA_DEF_SUB_DOMAIN = .FALSE.
 if ( ! $?DA_X_START_SUB_DOMAIN )set DA_X_START_SUB_DOMAIN = 55.0
 if ( ! $?DA_Y_START_SUB_DOMAIN )set DA_Y_START_SUB_DOMAIN = 35.0
 if ( ! $?DA_X_END_SUB_DOMAIN )set DA_X_END_SUB_DOMAIN = 80.0
 if ( ! $?DA_Y_END_SUB_DOMAIN )set DA_Y_END_SUB_DOMAIN = 60.0
 if ( ! $?DA_WRITE_OUTER_LOOP) set DA_WRITE_OUTER_LOOP = .FALSE.
 if ( ! $?DA_LAT_STATS_OPTION) set DA_LAT_STATS_OPTION = .FALSE.
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
 if ( ! $?DA_NUM_PSEUDO ) set DA_NUM_PSEUDO = 0
 if ( ! $?DA_PSEUDO_X ) set DA_PSEUDO_X = 165.0
 if ( ! $?DA_PSEUDO_Y ) set DA_PSEUDO_Y =  65.0
 if ( ! $?DA_PSEUDO_Z ) set DA_PSEUDO_Z =  15.0
 if ( ! $?DA_PSEUDO_VAL ) set DA_PSEUDO_VAL = 1.0
 if ( ! $?DA_PSEUDO_ERR ) set DA_PSEUDO_ERR = 1.0
 if ( ! $?DA_PSEUDO_VAR ) set DA_PSEUDO_VAR = u

 set DA_CY = `echo $START_DATE | cut -c1-4`
 set DA_MM = `echo $START_DATE | cut -c5-6`
 set DA_DD = `echo $START_DATE | cut -c7-8`
 set DA_HH = `echo $START_DATE | cut -c9-10`
 setenv DA_ANALYSIS_DATE ${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00.0000

#-----------------------------------------------------------------------
# [2.0] Perform sanity checks:
#-----------------------------------------------------------------------

 if ( ! -e $DA_FIRST_GUESS ) then
    echo "Error: First Guess file does not exist:"
    echo  $DA_FIRST_GUESS
    exit 1
 endif

 if ( ! -e $DA_OBSERVATIONS ) then
    echo "Error: Observation file does not exist:"
    echo  $DA_OBSERVATIONS
    exit 1
 endif

 if ( ! -e $DA_BACK_ERRORS ) then
    echo "Error: Background Error file does not exist:"
    echo  $DA_BACK_ERRORS
    exit 1
 endif

#-----------------------------------------------------------------------
# [3.0] Prepare for assimilation:
#-----------------------------------------------------------------------

# rm -rf ${RUN_DIR}/${DA_ID} >&! /dev/null
 if ( ! -d ${RUN_DIR}/${DA_ID} ) mkdir ${RUN_DIR}/${DA_ID} >&! /dev/null
 cd $RUN_DIR/${DA_ID}

 cp $WRFVAR_DIR/run/LANDUSE.TBL .
 ln -sf $WRFVAR_DIR/main/wrfvar.exe  wrfvar.exe

 ln -sf $DA_FIRST_GUESS		wrf_3dvar_input
 ln -sf $DA_BACK_ERRORS		fort.3${DA_CV_OPTIONS}
 ln -sf $DA_OBSERVATIONS	fort.9${DA_OB_FORMAT}

 if ( -e $DA_SSMI) then
    ln -sf $DA_SSMI	fort.93
    set DA_USE_SSMIRETRIEVALOBS = .TRUE.
 endif

 echo "First Guess Input File:      $DA_FIRST_GUESS"
 echo "Background Error Input File: $DA_BACK_ERRORS"
 echo "Observation Input File:      $DA_OBSERVATIONS"

#Create WRF-Var namelist file:

cat >! namelist.3dvar << EOF
&record1
 MODEL_TYPE = '$DA_MODEL_TYPE',
 WRITE_INCREMENTS = $DA_WRITE_INCREMENTS ,
 GLOBAL           = $DA_GLOBAL,
 PRINT_DETAIL   = $DA_PRINT_DETAIL /

&record2
 ANALYSIS_TYPE = '$DA_ANALYSIS_TYPE',
 ANALYSIS_DATE = '$DA_ANALYSIS_DATE',
 ANALYSIS_ACCU =  $DA_ANALYSIS_ACCU /

&record3
 fg_format = $DA_FG_FORMAT,
 ob_format = $DA_OB_FORMAT,
 num_fgat_time = $DA_NUM_FGAT_TIME /

&ens_info
 ensdim =           ${DA_ENSDIM},
 alpha_truncation = ${DA_ALPHA_TRUNCATION},
 alpha_corr_type  = ${DA_ALPHA_CORR_TYPE},
 alpha_corr_scale = ${DA_ALPHA_CORR_SCALE},
 alpha_std_dev    = ${DA_ALPHA_STD_DEV} /

&record4
 PROCESS_OBS    = '$DA_PROCESS_OBS',
 obs_qc_pointer = $DA_QC_POINTER,
 Use_SynopObs   = $DA_USE_SYNOPOBS,
 Use_ShipsObs   = $DA_USE_SHIPSOBS,
 Use_MetarObs   = $DA_USE_METAROBS,
 Use_PilotObs   = $DA_USE_PILOTOBS,
 Use_SoundObs   = $DA_USE_SOUNDOBS,
 Use_SatemObs   = $DA_USE_SATEMOBS,
 Use_GeoAMVObs  = $DA_USE_GEO_AMV,
 Use_PolarAMVObs = $DA_USE_POLAR_AMV,
 Use_AirepObs   = $DA_USE_AIREPOBS,
 Use_GpspwObs   = $DA_USE_GPSPWOBS,
 Use_GpsrefObs   = $DA_USE_GPSREFOBS,
 Use_ProfilerObs = $DA_USE_PROFILEROBS, 
 Use_BuoyObs     = $DA_USE_BUOYOBS,
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
 /

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
 LEN_SCALING5   = $DA_LEN_SCALING5,
 power_truncation = $DA_POWER_TRUNCATION,
 jb_factor        = ${DA_JB_FACTOR},
 je_factor        = ${DA_JE_FACTOR} /

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

#Create WRF V2.1 namelist.input file:

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
 e_sn                                = $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER, $SOUTH_NORTH_GRID_NUMBER
 s_vert                              = 1, 1, 1,
 e_vert                              = $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER, $VERTICAL_GRID_NUMBER,
 dx                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 dy                                  = $GRID_DISTANCE, $GRID_DISTANCE, $GRID_DISTANCE,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    30,
 j_parent_start                      = 0,     20,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0,
 nproc_y                             = $NPROC_X
 /

 &physics
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 10,    10,    10,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = ${DA_SF_SURFACE_PHYSICS},     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 surface_input_source                = 1,
 mp_zero_out                         = 0,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = ${DA_ENSDIM},
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
 periodic_x                          = ${PERIODIC_X} ,.false.,.false.,
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

#-------------------------------------------------------------------
#Run WRF-Var:
#-------------------------------------------------------------------

#PC-Linux:
#if ( $NUM_PROCS > 1  )then
# cp $WRFVAR_DIR/run/nodes .
#   mpirun -np ${NUM_PROCS} -machinefile nodes ./wrfvar.exe >&! /dev/null
#else
#   mpirun -np 1 ./wrfvar.exe >&! /dev/null #Assumes compile in DM mode.
#endif

./wrfvar.exe >&! wrfvar.out

#IBM (llsubmit):
#poe ./wrfvar.exe
#mpirun -np ${NUM_PROCS} ./wrfvar.exe

cp fort.12 DAProg_WRF-Var.statistics >&! /dev/null
cp fort.81 DAProg_WRF-Var.cost_fn >&! /dev/null
cp fort.82 DAProg_WRF_Var.grad_fn >&! /dev/null

echo "WRF-Var completed"

exit (0)
