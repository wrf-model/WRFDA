#! /bin/csh 
# @ job_type   = parallel
# @ environment = COPY_ALL;MP_EUILIB=us
# @ job_name   = wrfvar
# @ output     = wrfvar.out
# @ error      = wrfvar.err
# @ network.MPI    = csss,not_shared,us
## @ network.MPI    = csss,shared,ip
# @ node_usage = not_shared
# @ checkpoint = no
# @ wall_clock_limit = 06:00:00
# @ node       = 1
# @ total_tasks    =  5
# @ class      =  com_pr8
# @ queue
#
#-----------------------------------------------------------------------
# Script DA_Run_WRF-4DVar_parallel.csh
#
# Purpose: Top level script for running the WRF-4DVar system on multi-processors
#
# Method:  1) Set up links to run directory.
#          2) Run WRF-4DVar in run directory.
#
# History: 07/26/06  Original version. Xin Zhang
#
#-----------------------------------------------------------------------

#set echo on

#unlimit


 echo ""
 echo "Running script DA_Run_WRF-4DVar"
 echo "-----------------------------"
 echo ""

echo '#-----------------------------------------------------------------------'
echo '# USER: Define non-default job parameters via environment variables: '
echo '#-----------------------------------------------------------------------'

setenv START_DATE               2005071603       # Analysis date : YYYYMMDDHH
setenv WEST_EAST_GRID_NUMBER    31
setenv SOUTH_NORTH_GRID_NUMBER  25
setenv VERTICAL_GRID_NUMBER     17
setenv RESOLUTION               135              # resolution: km
setenv DA_TIME_STEP             600              # Model integrate time step : second
setenv TRAJECTORY_SAVE_FREQ     10               # The frequency of the trajectory will be saved : Minutes
setenv CASE_NAME                haitang          # Case name and resolution
setenv EXPE_NAME                var4d05          # Case name and resolution
setenv DA_NTMAX                 50               # Maximum number of inner loop iterations.
setenv VARFRACTION              5                # fraction of processors devoted to var (denominator) 
setenv DA_TIME_WINDOW           3                # Data assimilation time window : Hours

setenv DA_VAR_SCALING1          1.0
setenv DA_VAR_SCALING2          1.0
setenv DA_VAR_SCALING3          1.0
setenv DA_VAR_SCALING4          1.0
setenv DA_VAR_SCALING5          1.0
setenv DA_LEN_SCALING1          0.5 # 1.0
setenv DA_LEN_SCALING2          0.5 # 1.0
setenv DA_LEN_SCALING3          0.5 # 1.0
setenv DA_LEN_SCALING4          0.5 # 1.0
setenv DA_LEN_SCALING5          0.5 # 1.0

setenv DA_JCDFI_ON_OFF          .FALSE.            # JcDFI ON/OFF
setenv DA_JCDFI_TAUC            10800.            # JcDFI Tauc : seconds
setenv DA_JCDFI_GAMA            10.0              # Weighting given to JcDFI term
setenv DA_JCDFI_ERROR_WIND      3.0               # JcDFI error for horizontal wind : m/s
setenv DA_JCDFI_ERROR_T         1.0               # JcDFI error for temperature : K
setenv DA_JCDFI_ERROR_Q         0.001             # JcDFI error for moisture : kg/kg
setenv DA_JCDFI_ERROR_MU        1000.             # JcDFI error for pert. pressure : Pa

setenv ROOT_DIR /ptmp/huangx/wrf             # common reference src, data, etc
setenv USER_DIR /ptmp/xinzhang/wrfp1          # user data and results
setenv RES_DIR $ROOT_DIR                     # results

set GRID_DISTANCE=`expr $RESOLUTION \* 1000`                                      # Model horizontal resolution : meter
set INTERVAL_SECONDS=`expr $DA_TIME_WINDOW \* 3600`                               # Time Window in Seconds
set DA_NUM_FGAT_TIME=`expr $DA_TIME_WINDOW \+ 1`                                  # Number of FGAT ob windows.
setenv RUN_DIR $USER_DIR/${EXPE_NAME}${RESOLUTION}km$START_DATE                  # The directory in which you wish your case run
setenv BE_NAME gen_be_nmc_${CASE_NAME}${RESOLUTION}_${WEST_EAST_GRID_NUMBER}x${SOUTH_NORTH_GRID_NUMBER}x${VERTICAL_GRID_NUMBER}.dat                                             # The BE file 

# setup the directory for storing results (5 levels here below)
setenv RES_DIR $RES_DIR/$CASE_NAME
 if (  -d ${RES_DIR} ) then
    echo "The RES_DIR directory: $RES_DIR has already been used"
 else
    mkdir -p ${RES_DIR}
 endif
setenv RES_DIR $RES_DIR/${RESOLUTION}km
 if (  -d ${RES_DIR} ) then
    echo "The RES_DIR directory: $RES_DIR has already been used"
 else
    mkdir -p ${RES_DIR}
 endif
setenv RES_DIR $RES_DIR/results
 if (  -d ${RES_DIR} ) then
    echo "The RES_DIR directory: $RES_DIR has already been used"
 else
    mkdir -p ${RES_DIR}
 endif
setenv RES_DIR $RES_DIR/$EXPE_NAME
 if (  -d ${RES_DIR} ) then
    echo "The RES_DIR directory: $RES_DIR has already been used"
 else
    mkdir -p ${RES_DIR}
 endif
setenv RES_DIR $RES_DIR/$START_DATE
 if (  -d ${RES_DIR}) then
    echo '------------------------ ATTENTION -----------------------------------------'
    echo "The RES_DIR directory: $RES_DIR has already been used"
    echo 'please assign another dirctory'
    echo '----------------------------------------------------------------------------'
    exit (1)
 else
    mkdir -p ${RES_DIR}
 endif

 @ dummy = ($TRAJECTORY_SAVE_FREQ * 60 ) % $DA_TIME_STEP
 if ($dummy != 0) then
    echo '------------------------ ATTENTION -----------------------------------------'
    echo ' Please adjust DA_TIME_STEP or TRAJECTORY_SAVE_FREQ to make sure'
    echo ' (TRAJECTORY_SAVE_FREQ * *60 / DA_TIME_STEP ) is a integer '
    echo '----------------------------------------------------------------------------'
    exit (1)
 endif

 if (  -d ${RUN_DIR}) then
    echo '------------------------ ATTENTION -----------------------------------------'
    echo "The RUN_DIR directory: $RUN_DIR  is  already exist "
    echo 'please assign another dirctory'
    echo '----------------------------------------------------------------------------'
    exit (1) 
 else
    mkdir -p ${RUN_DIR} 
    cd ${RUN_DIR}
 endif

echo -n start whole darn thing
date

##########################################################################
#USER: DO NOT MAKE CHANGES BELOW (if you do, you're on your own!) 
##########################################################################
echo '#-----------------------------------------------------------------------'
echo '# [1.0] Specify default environment variables:'
echo '#-----------------------------------------------------------------------'

 set DA_CY = `echo $START_DATE | cut -c1-4`
 set DA_MM = `echo $START_DATE | cut -c5-6`
 set DA_DD = `echo $START_DATE | cut -c7-8`
 set DA_HH = `echo $START_DATE | cut -c9-10`

#Default directories/files:
 if ( ! $?EXE_DIR )             setenv EXE_DIR          ${ROOT_DIR}/bin
 if ( ! $?MODEL_DAT_DIR )       setenv MODEL_DAT_DIR    ${ROOT_DIR}/modeldata
 if ( ! $?BE_DIR )              setenv BE_DIR           ${ROOT_DIR}/bedata
 if ( ! $?CASE_DIR )            setenv CASE_DIR         ${ROOT_DIR}/${CASE_NAME}/${RESOLUTION}km
 if ( ! $?DA_CV_OPTIONS )       setenv DA_CV_OPTIONS    5                          # Background error model. 
 if ( ! $?DA_FIRST_GUESS ) setenv DA_FIRST_GUESS ${CASE_DIR}/fgsdata/wrffgs_d01.${DA_CY}${DA_MM}${DA_DD}${DA_HH} # wrfvar "first guess" input.
 if ( ! $?DA_BDY ) setenv DA_BDY ${CASE_DIR}/bdydata/wrfbdy_d01.${DA_CY}${DA_MM}${DA_DD}${DA_HH} # wrfvar "Boundary" input.
 if ( ! $?DA_BACK_ERRORS ) setenv DA_BACK_ERRORS ${BE_DIR}/${BE_NAME}    # wrfvar background errors.
 if ( ! $?DA_SSMI ) setenv DA_SSMI ${CASE_DIR}/obs_ssmi_retrieval.3dvar         # SSM/I radiances (ignore if not using).
 if ( ! $?DA_RADAR) setenv DA_RADAR ${CASE_DIR}/ob/radar.dat       # Radar data (ignore if not using).

#Default WRF namelist variables:
 if ( ! $?NUM_PROCS ) setenv NUM_PROCS 1                          # Number of processors to run on.
 if ( ! $?WEST_EAST_GRID_NUMBER )   setenv WEST_EAST_GRID_NUMBER 165   # X grid dimension.
 if ( ! $?SOUTH_NORTH_GRID_NUMBER ) setenv SOUTH_NORTH_GRID_NUMBER 217 # Y grid dimension.
 if ( ! $?VERTICAL_GRID_NUMBER ) setenv VERTICAL_GRID_NUMBER 31   # Z grid dimension.
 if ( ! $?GRID_DISTANCE ) setenv GRID_DISTANCE 60000              # Grid resolution (m).
 if ( ! $?DA_SF_SURFACE_PHYSICS ) setenv DA_SF_SURFACE_PHYSICS 2  #(1=Thermal diffusion, 2=Noah LSM).
 if ( $DA_SF_SURFACE_PHYSICS == 1 ) setenv DA_NUM_SOIL_LAYERS 5   # (Thermal diffusion surface physics).
 if ( $DA_SF_SURFACE_PHYSICS == 2 ) setenv DA_NUM_SOIL_LAYERS 4   # (Noah LSM surface physics).

#Supported default WRF-Var namelist variables:
 if ( ! $?DA_FG_FORMAT )  setenv DA_FG_FORMAT 1                   # First guess format: 1=WRF, 2=MM5, 3=KMA
 if ( ! $?DA_OB_FORMAT )  setenv DA_OB_FORMAT 2                   # Observation format: 1=BUFR, 2=ASCII "little_r"
 if ( ! $?DA_GLOBAL )     setenv DA_GLOBAL .FALSE.                # Regional/global domain.
 if ( ! $?NPROC_X )       setenv NPROC_X 0                        # Regional, always set NPROC_X to 0, Global, always 1
 if (   $DA_GLOBAL == ".TRUE.") setenv NPROC_X 1
 if ( ! $?DA_MODEL_TYPE ) setenv DA_MODEL_TYPE WRF                # WRF, MM5 or KMA.
 if ( ! $?DA_WRITE_INCREMENTS ) setenv DA_WRITE_INCREMENTS .TRUE. # Optionally write increments.
 if ( ! $?LVAR4D )        setenv LVAR4D .TRUE.                    # 
 if ( ! $?DA_NUM_FGAT_TIME ) setenv DA_NUM_FGAT_TIME 7            # Number of FGAT ob windows.
 if ( ! $?DA_USE_SYNOPOBS )  set DA_USE_SYNOPOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SHIPSOBS )  set DA_USE_SHIPSOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_METAROBS )  set DA_USE_METAROBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_PILOTOBS )  set DA_USE_PILOTOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SOUNDOBS )  set DA_USE_SOUNDOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_SATEMOBS )  set DA_USE_SATEMOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_GEO_AMV  )  set DA_USE_GEO_AMV =  .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_POLAR_AMV ) set DA_USE_POLAR_AMV = .TRUE.        # Assimilate this observation type.
 if ( ! $?DA_USE_AIREPOBS )  set DA_USE_AIREPOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_GPSPWOBS )  set DA_USE_GPSPWOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_RADAROBS )  set DA_USE_RADAROBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_Use_Radar_rv )  set DA_Use_Radar_rv = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_Use_Radar_rf )  set DA_Use_Radar_rf = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_BOGUSOBS )  set DA_USE_BOGUSOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_USE_GPSREFOBS ) set DA_USE_GPSREFOBS = .TRUE.        # Assimilate this observation type.
 if ( ! $?DA_USE_PROFILEROBS )set DA_USE_PROFILEROBS = .TRUE.     # Assimilate this observation type.
 if ( ! $?DA_USE_BUOYOBS )   set DA_USE_BUOYOBS = .TRUE.          # Assimilate this observation type.
 if ( ! $?DA_USE_SSMIRETRIEVALOBS )set DA_USE_SSMIRETRIEVALOBS = .FALSE. # Assimilate this observation type.
 if ( ! $?DA_USE_SSMITBOBS ) set DA_USE_SSMITBOBS = .FALSE.       # Assimilate this observation type.
 if ( ! $?DA_USE_SSMT1OBS )  set DA_USE_SSMT1OBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_SSMT2OBS )  set DA_USE_SSMT2OBS = .FALSE.        # Assimilate this observation type.
 if ( ! $?DA_USE_QSCATOBS )  set DA_USE_QSCATOBS = .TRUE.         # Assimilate this observation type.
 if ( ! $?DA_Check_Max_IV )  set DA_Check_Max_IV = .TRUE.         # Perform O-B > 5sigma_o QC if true.
 if ( ! $?DA_MAX_EXT_ITS )   set DA_MAX_EXT_ITS = 1               # Maximum number of "WRF-Var outer loops".
 if ( ! $?DA_EPS )           set DA_EPS = "1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02, 1.E-02," # Convergence criteria.
 if ( ! $?DA_NTMAX )         set DA_NTMAX =   2                   # Maximum number of inner loop iterations.
 if ( ! $?DA_DEBUG_LEVEL )   set DA_DEBUG_LEVEL =    0            # The level of debug 0, 100 , 150 , 200
 if ( ! $?DA_RF_PASSES )     set DA_RF_PASSES = 6                 # Number of recursive filter passes.
 if ( ! $?DA_TESTING_3DVAR ) set DA_TESTING_3DVAR = .FALSE.       # Test WRF-Var code.
 if ( ! $?DA_TEST_TRANSFORMS )  set DA_TEST_TRANSFORMS = .FALSE.  # Test WRF-Var transforms.
 if ( ! $?DA_TEST_STATISTICS )  set DA_TEST_STATISTICS = .FALSE.  # Test WRF-Var statistics.
 if ( ! $?DA_INTERPOLATE_STATS )set DA_INTERPOLATE_STATS = .TRUE. # True if statistics computed on different domain.
 if ( ! $?DA_MINIMISATION_OPTION) set DA_MINIMISATION_OPTION = 1  # 1=Quasi-Newton, 2=Conjugate gradient.
 if ( ! $?DA_CALCULATE_CG_COST_FUNCTION) set DA_CALCULATE_CG_COST_FUNCTION = .FALSE. # if want CG diagnostic output. 
 if ( ! $?DA_CV_OPTIONS_HUM ) set DA_CV_OPTIONS_HUM = 1           # Moist control variable (1-3).
 if ( ! $?DA_CHECK_RH )      set DA_CHECK_RH = 0                  # Physical check on humidity (0-2).
 if ( ! $?DA_MAX_VERT_VAR1 ) set DA_MAX_VERT_VAR1 = 99.0          # CV1 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR2 ) set DA_MAX_VERT_VAR2 = 99.0          # CV2 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR3 ) set DA_MAX_VERT_VAR3 = 99.0          # CV3 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR4 ) set DA_MAX_VERT_VAR4 = 99.0          # CV4 variance truncation.
 if ( ! $?DA_MAX_VERT_VAR5 ) set DA_MAX_VERT_VAR5 =  0.0          # CV5 variance truncation.

#Not supported default WRF-Var namelist variables (overwrite at your own risk):
 if ( ! $?DA_ANALYSIS_TYPE ) set DA_ANALYSIS_TYPE = 3D-VAR
 if ( ! $?DA_ANALYSIS_ACCU ) set DA_ANALYSIS_ACCU = 900
 if ( ! $?DA_PROCESS_OBS )   set DA_PROCESS_OBS = YES
 if ( ! $?DA_QC_POINTER )    set DA_QC_POINTER = 0
 if ( ! $?DA_USE_OBS_ERRFAC ) set DA_USE_OBS_ERRFAC = .FALSE.
 if ( ! $?DA_PUT_RAND_SEED ) set DA_PUT_RAND_SEED = .FALSE.
 if ( ! $?DA_OMB_SET_RAND ) set DA_OMB_SET_RAND = .FALSE.
 if ( ! $?DA_OMB_ADD_NOISE ) set DA_OMB_ADD_NOISE = .FALSE.
 if ( ! $?DA_TIME_WINDOW )   set DA_TIME_WINDOW = 7.      
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
 if ( ! $?DA_DEF_SUB_DOMAIN )set DA_DEF_SUB_DOMAIN = .FALSE.
 if ( ! $?DA_X_START_SUB_DOMAIN )set DA_X_START_SUB_DOMAIN = 55.0
 if ( ! $?DA_Y_START_SUB_DOMAIN )set DA_Y_START_SUB_DOMAIN = 35.0
 if ( ! $?DA_X_END_SUB_DOMAIN )set DA_X_END_SUB_DOMAIN = 80.0
 if ( ! $?DA_Y_END_SUB_DOMAIN )set DA_Y_END_SUB_DOMAIN = 60.0
 if ( ! $?DA_WRITE_OUTER_LOOP) set DA_WRITE_OUTER_LOOP = .FALSE.
 if ( ! $?DA_LAT_STATS_OPTION) set DA_LAT_STATS_OPTION = .FALSE.
 if ( ! $?DA_as1        )    set DA_as1 = "0.25, 1.00, 1.5"
 if ( ! $?DA_as2        )    set DA_as2 = "0.25, 1.00, 1.5"
 if ( ! $?DA_as3        )    set DA_as3 = "0.35, 1.00, 1.5"
 if ( ! $?DA_as4        )    set DA_as4 = "0.10, 1.75, 1.5"
 if ( ! $?DA_as5        )    set DA_as5 = "0.35, 1.00, 1.5"
 if ( ! $?DA_SFC_ASSI_OPTIONS )  set DA_SFC_ASSI_OPTIONS = 1
 if ( ! $?DA_SET_OMB_RAND_FAC )  set DA_SET_OMB_RAND_FAC = 1.0
 if ( ! $?DA_SEED_ARRAY1 )   set DA_SEED_ARRAY1 = 0
 if ( ! $?DA_SEED_ARRAY2 )   set DA_SEED_ARRAY2 = 0
 if ( ! $?DA_BALANCE_TYPE )  set DA_BALANCE_TYPE = 1
 if ( ! $?DA_VERT_CORR )     set DA_VERT_CORR = 2
 if ( ! $?DA_VERTICAL_IP )   set DA_VERTICAL_IP = 0
 if ( ! $?DA_VERT_EVALUE )   set DA_VERT_EVALUE = 1
 if ( ! $?DA_NUM_PSEUDO ) set DA_NUM_PSEUDO = 0
 if ( ! $?DA_PSEUDO_X ) set DA_PSEUDO_X =   1.0
 if ( ! $?DA_PSEUDO_Y ) set DA_PSEUDO_Y =   1.0
 if ( ! $?DA_PSEUDO_Z ) set DA_PSEUDO_Z =   1.0
 if ( ! $?DA_PSEUDO_VAL ) set DA_PSEUDO_VAL = 1.0
 if ( ! $?DA_PSEUDO_ERR ) set DA_PSEUDO_ERR = 1.0
 if ( ! $?DA_PSEUDO_VAR ) set DA_PSEUDO_VAR = t
 if ( ! $?DA_JCDFI_ON_OFF) set DA_JCDFI_ON_OFF = .TRUE.            # JcDFI ON/OFF
 if ( ! $?DA_JCDFI_TAUC) set DA_JCDFI_TAUC = 10800.            # JcDFI Tauc : seconds
 if ( ! $?DA_JCDFI_GAMA ) set DA_JCDFI_GAMA = 1.0               # Weighting given to JcDFI term
 if ( ! $?DA_JCDFI_ERROR_WIND) set DA_JCDFI_ERROR_WIND = 3.0               # JcDFI error for horizontal wind : m/s
 if ( ! $?DA_JCDFI_ERROR_T) set DA_JCDFI_ERROR_T = 1.0               # JcDFI error for temperature : K
 if ( ! $?DA_JCDFI_ERROR_Q) set DA_JCDFI_ERROR_Q = 0.001             # JcDFI error for moisture : kg/kg
 if ( ! $?DA_JCDFI_ERROR_MU) set DA_JCDFI_ERROR_MU = 1000.             # JcDFI error for pert. pressure : Pa

 setenv DA_ANALYSIS_DATE ${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00.0000

echo '#-----------------------------------------------------------------------'
echo '# [2.0] Perform sanity checks:'
echo '#-----------------------------------------------------------------------'


 if ( ! -e $DA_FIRST_GUESS ) then
    echo "Error: First Guess file does not exist:"
    echo  $DA_FIRST_GUESS
    exit 1
 endif

 if ( ! -e $DA_BDY ) then
    echo "Error: Boundary file does not exist:"
    echo  $DA_BDY
    exit 1
 endif

 if ( ! -e $DA_BACK_ERRORS ) then
    echo "Error: Background Error file does not exist:"
    echo  $DA_BACK_ERRORS
    exit 1
 endif

echo '#-----------------------------------------------------------------------'
echo '# [3.0] Prepare for assimilation: establish the linkages'
echo '#-----------------------------------------------------------------------'

 ln -sf $MODEL_DAT_DIR/LANDUSE.TBL .
 ln -sf $MODEL_DAT_DIR/GENPARM.TBL .
 ln -sf $MODEL_DAT_DIR/RRTM_DATA_DBL   RRTM_DATA
 ln -sf $MODEL_DAT_DIR/SOILPARM.TBL .
 ln -sf $MODEL_DAT_DIR/VEGPARM.TBL  .
 ln -sf $MODEL_DAT_DIR/gribmap.txt  .

 ln -sf $EXE_DIR/wrfplus.exe  wrf.exe
 ln -sf $EXE_DIR/4dvar.exe  da_3dvar.exe
 ln -sf $EXE_DIR/geth_new.exe geth_new.exe

 ln -sf $DA_FIRST_GUESS		wrf_3dvar_input
 ln -sf $DA_BDY                 wrfbdy_d01
 ln -sf $DA_BACK_ERRORS		fort.3${DA_CV_OPTIONS}

 foreach num ( 00 01 02 03 04 05 06)

        set NEW_DATE = `geth_new.exe ${DA_CY}-${DA_MM}-${DA_DD}-${DA_HH} ${num}`
        set NEW_CY = `echo $NEW_DATE | cut -c1-4`
        set NEW_MM = `echo $NEW_DATE | cut -c6-7`
        set NEW_DD = `echo $NEW_DATE | cut -c9-10`
        set NEW_HH = `echo $NEW_DATE | cut -c12-13`

        set DA_OBSERVATIONS = ${CASE_DIR}/../obsdata/obs_1h/obs_gts.3dvar_d01_${NEW_CY}${NEW_MM}${NEW_DD}${NEW_HH}

        if ( $num == 00 ) then
             set DA_OBSERVATIONS = ${CASE_DIR}/../obsdata/obs_30m/obs_gts.3dvar_d01_${NEW_CY}${NEW_MM}${NEW_DD}${NEW_HH}+
        endif

        if ( $num == 06 ) then
             set DA_OBSERVATIONS = ${CASE_DIR}/../obsdata/obs_30m/obs_gts.3dvar_d01_${NEW_CY}${NEW_MM}${NEW_DD}${NEW_HH}-
        endif


        ln -sf ${DA_OBSERVATIONS}		fgat_ob.${num}

        if ( ! -e ${DA_OBSERVATIONS} ) then
           echo "Error: Observation file does not exist:"
           echo  $DA_OBSERVATIONS
           exit 1
        endif

        echo "Observation Input File:      $DA_OBSERVATIONS"
 end

# if ( -e $DA_SSMI) then
#    ln -sf $DA_SSMI	fort.93
#    set DA_USE_SSMIRETRIEVALOBS = .TRUE.
# endif

 echo "First Guess Input File:      $DA_FIRST_GUESS"
 echo "Boundary Input File:         $DA_BDY"
 echo "Background Error Input File: $DA_BACK_ERRORS"

echo '#-----------------------------------------------------------------------'
echo '# [4.0] Create namelist files    '
echo '#-----------------------------------------------------------------------'

echo '#Create WRF-Var namelist file: '

 set NEW_DATE = `geth_new.exe ${DA_CY}-${DA_MM}-${DA_DD}-${DA_HH} ${DA_TIME_WINDOW}`
 set NEW_CY = `echo $NEW_DATE | cut -c1-4`
 set NEW_MM = `echo $NEW_DATE | cut -c6-7`
 set NEW_DD = `echo $NEW_DATE | cut -c9-10`
 set NEW_HH = `echo $NEW_DATE | cut -c12-13`

cat >! namelist.3dvar << EOF
&record1
 MODEL_TYPE = '$DA_MODEL_TYPE',
 WRITE_INCREMENTS = $DA_WRITE_INCREMENTS ,
 lvar4d           = $LVAR4D,
 PRINT_DETAIL   = $DA_PRINT_DETAIL /

&record2
 ANALYSIS_TYPE = '$DA_ANALYSIS_TYPE',
 ANALYSIS_DATE = '$DA_ANALYSIS_DATE',
 ANALYSIS_ACCU =  $DA_ANALYSIS_ACCU /

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
 Use_GeoAMVObs  = $DA_USE_GEO_AMV,
 Use_PolarAMVObs = $DA_USE_POLAR_AMV,
 Use_AirepObs   = $DA_USE_AIREPOBS,
 Use_GpspwObs   = $DA_USE_GPSPWOBS,
 Use_GpsrefObs   = $DA_USE_GPSREFOBS,
 Use_ProfilerObs = $DA_USE_PROFILEROBS, 
 Use_BuoyObs     = $DA_USE_BUOYOBS,
 Use_BogusObs    = $DA_USE_BOGUSOBS,
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
 TIME_WINDOW    = $DA_TIME_WINDOW /

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
 cv_options     = $DA_CV_OPTIONS,
 cv_options_hum = $DA_CV_OPTIONS_HUM,
 calculate_cg_cost_function = $DA_CALCULATE_CG_COST_FUNCTION,
 check_rh       = $DA_CHECK_RH,
 as1            = $DA_as1,
 as2            = $DA_as2,
 as3            = $DA_as3,
 as4            = $DA_as4,
 as5            = $DA_as5,
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

echo '#Create WRF 4dVar namelist.var4dnl  input file:'

cat >! namelist.var4dnl << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = $DA_TIME_WINDOW,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $DA_CY, $DA_CY, $DA_CY,
 start_month                         = $DA_MM, $DA_MM, $DA_MM,
 start_day                           = $DA_DD, $DA_DD, $DA_DD,
 start_hour                          = $DA_HH, $DA_HH, $DA_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $NEW_CY, $NEW_CY, $NEW_CY,
 end_month                           = $NEW_MM, $NEW_MM, $NEW_MM,
 end_day                             = $NEW_DD, $NEW_DD, $NEW_DD,
 end_hour                            = $NEW_HH, $NEW_HH, $NEW_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = $INTERVAL_SECONDS,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 9999,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2,
 io_form_restart                     = 2,
 io_form_input                       = 2,
 io_form_boundary                    = 2,
 debug_level                         = $DA_DEBUG_LEVEL,
 write_input                         = .true.,
 inputout_interval                   = 60,
 input_outname                       = 'wrf_3dvar_input_d<domain>_<date>',
 auxhist2_inname                     = 'auxhist2_d<domain>_<date>',
 auxhist2_interval                   = $TRAJECTORY_SAVE_FREQ,
 frames_per_auxhist2                 = 1,
 /

 &domains
 time_step                           = $DA_TIME_STEP,
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
 /

 &physics
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 15,    10,    10,
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
 mp_zero_out                         = 2,
 mp_zero_out_thresh                  = 1.e-8,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 2,
 rk_ord                              = 3,
 w_damping                           = 1,
 diff_opt                            = 1,
 km_opt                              = 4,
 damp_opt                            = 0,
 base_temp                           = 290.
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.0,   0.01,   0.01
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
 jcdfi_onoff                         = $DA_JCDFI_ON_OFF,
 jcdfi_tauc                          = $DA_JCDFI_TAUC,
 jcdfi_gama                          = $DA_JCDFI_GAMA,
 jcdfi_error_wind                    = $DA_JCDFI_ERROR_WIND,
 jcdfi_error_t                       = $DA_JCDFI_ERROR_T,
 jcdfi_error_q                       = $DA_JCDFI_ERROR_Q,
 jcdfi_error_mu                      = $DA_JCDFI_ERROR_MU,
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

echo '#Create WRF 4dVar namelist.var4dtl  input file:'

cat >! namelist.var4dtl << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = $DA_TIME_WINDOW,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $DA_CY, $DA_CY, $DA_CY,
 start_month                         = $DA_MM, $DA_MM, $DA_MM,
 start_day                           = $DA_DD, $DA_DD, $DA_DD,
 start_hour                          = $DA_HH, $DA_HH, $DA_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $NEW_CY, $NEW_CY, $NEW_CY,
 end_month                           = $NEW_MM, $NEW_MM, $NEW_MM,
 end_day                             = $NEW_DD, $NEW_DD, $NEW_DD,
 end_hour                            = $NEW_HH, $NEW_HH, $NEW_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = $INTERVAL_SECONDS,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 9999,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2,
 io_form_restart                     = 2,
 io_form_input                       = 2,
 io_form_boundary                    = 2,
 debug_level                         = $DA_DEBUG_LEVEL,
 write_input                         = .true.,
 inputout_interval                   = 60,
 input_outname                       = 'tl<date>',
 auxinput2_inname                    = 'auxhist2_d<domain>_<date>'
 auxinput2_interval                  = $TRAJECTORY_SAVE_FREQ,
 frames_per_auxhist2                 = 1,
 /

 &domains
 time_step                           = $DA_TIME_STEP,
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
 /

 &physics
 mp_physics                          = 0,     3,     3,
 ra_lw_physics                       = 0,     1,     1,
 ra_sw_physics                       = 0,     1,     1,
 radt                                = 00,    10,    10,
 sf_sfclay_physics                   = 0,     1,     1,
 sf_surface_physics                  = 0,     1,     1,
 bl_pbl_physics                      = 0,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 0,     1,     0,
 cudt                                = 0,     5,     5,
 isfflx                              = 0,
 ifsnow                              = 0,
 icloud                              = 0,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 surface_input_source                = 1,
 mp_zero_out                         = 2,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 202,
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
 jcdfi_onoff                         = $DA_JCDFI_ON_OFF,
 jcdfi_tauc                          = $DA_JCDFI_TAUC,
 jcdfi_gama                          = $DA_JCDFI_GAMA,
 jcdfi_error_wind                    = $DA_JCDFI_ERROR_WIND,
 jcdfi_error_t                       = $DA_JCDFI_ERROR_T,
 jcdfi_error_q                       = $DA_JCDFI_ERROR_Q,
 jcdfi_error_mu                      = $DA_JCDFI_ERROR_MU,
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


 &var_test_control
 /


 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF

echo '#Create WRF 4dVar namelist.var4dad  input file:'

cat >! namelist.var4dad << EOF
 &time_control
 run_days                            = 0,
 run_hours                           = $DA_TIME_WINDOW,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = $DA_CY, $DA_CY, $DA_CY,
 start_month                         = $DA_MM, $DA_MM, $DA_MM,
 start_day                           = $DA_DD, $DA_DD, $DA_DD,
 start_hour                          = $DA_HH, $DA_HH, $DA_HH,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = $NEW_CY, $NEW_CY, $NEW_CY,
 end_month                           = $NEW_MM, $NEW_MM, $NEW_MM,
 end_day                             = $NEW_DD, $NEW_DD, $NEW_DD,
 end_hour                            = $NEW_HH, $NEW_HH, $NEW_HH,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = $INTERVAL_SECONDS,
 input_from_file                     = .true.,.false.,.false.,
 history_interval                    = 9999,  60,   60,
 frames_per_outfile                  = 1000, 1000, 1000,
 restart                             = .false.,
 restart_interval                    = 5000,
 io_form_history                     = 2,
 io_form_restart                     = 2,
 io_form_input                       = 2,
 io_form_boundary                    = 2,
 debug_level                         = $DA_DEBUG_LEVEL,
 write_input                         = .true.,
 inputout_begin_y                    = 0,
 inputout_begin_mo                   = 0,
 inputout_begin_d                    = 0,
 inputout_begin_h                    = 0,
 inputout_begin_m                    = 0,
 inputout_begin_s                    = 0,
 inputout_interval                   = 60,
 input_outname                       = 'wrf_3dvar_input_d<domain>_<date>',
 auxinput2_inname                    = 'auxhist2_d<domain>_<date>',
 auxinput2_interval                  = $TRAJECTORY_SAVE_FREQ,
 auxinput3_inname                    = 'auxinput3_d<domain>_<date>',
 auxinput3_interval                  = 60
 /

 &domains
 time_step                           = $DA_TIME_STEP,
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
 /

 &physics
 mp_physics                          = 0,     3,     3,
 ra_lw_physics                       = 0,     1,     1,
 ra_sw_physics                       = 0,     1,     1,
 radt                                = 00,    10,    10,
 sf_sfclay_physics                   = 0,     1,     1,
 sf_surface_physics                  = 0,     1,     1,
 bl_pbl_physics                      = 0,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 0,     1,     0,
 cudt                                = 0,     5,     5,
 isfflx                              = 0,
 ifsnow                              = 0,
 icloud                              = 0,
 num_soil_layers                     = ${DA_NUM_SOIL_LAYERS},
 surface_input_source                = 1,
 mp_zero_out                         = 2,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 /

 &dynamics
 dyn_opt                             = 302,
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
 jcdfi_onoff                         = $DA_JCDFI_ON_OFF,
 jcdfi_tauc                          = $DA_JCDFI_TAUC,
 jcdfi_gama                          = $DA_JCDFI_GAMA,
 jcdfi_error_wind                    = $DA_JCDFI_ERROR_WIND,
 jcdfi_error_t                       = $DA_JCDFI_ERROR_T,
 jcdfi_error_q                       = $DA_JCDFI_ERROR_Q,
 jcdfi_error_mu                      = $DA_JCDFI_ERROR_MU,
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


 &var_test_control
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /

EOF

 set NEW_DATE = `geth_new.exe ${DA_CY}-${DA_MM}-${DA_DD}-${DA_HH} -3`
 set NEW_CY = `echo $NEW_DATE | cut -c1-4`
 set NEW_MM = `echo $NEW_DATE | cut -c6-7`
 set NEW_DD = `echo $NEW_DATE | cut -c9-10`
 set NEW_HH = `echo $NEW_DATE | cut -c12-13`

echo '#-----------------------------------------------------------------------'
echo '# [5.0] Create runvar shell files    '
echo '#-----------------------------------------------------------------------'

 foreach num ( 01 02 03 04 05 06 )

   set NEW_DATE = `geth_new.exe ${DA_CY}-${DA_MM}-${DA_DD}-${DA_HH} ${num}`
   if ( $num == 01 ) then
       set CY01 = `echo $NEW_DATE | cut -c1-4`
       set MM01 = `echo $NEW_DATE | cut -c6-7`
       set DD01 = `echo $NEW_DATE | cut -c9-10`
       set HH01 = `echo $NEW_DATE | cut -c12-13`
   endif
   if ( $num == 02 ) then
       set CY02 = `echo $NEW_DATE | cut -c1-4`
       set MM02 = `echo $NEW_DATE | cut -c6-7`
       set DD02 = `echo $NEW_DATE | cut -c9-10`
       set HH02 = `echo $NEW_DATE | cut -c12-13`
   endif
   if ( $num == 03 ) then
       set CY03 = `echo $NEW_DATE | cut -c1-4`
       set MM03 = `echo $NEW_DATE | cut -c6-7`
       set DD03 = `echo $NEW_DATE | cut -c9-10`
       set HH03 = `echo $NEW_DATE | cut -c12-13`
   endif
   if ( $num == 04 ) then
       set CY04 = `echo $NEW_DATE | cut -c1-4`
       set MM04 = `echo $NEW_DATE | cut -c6-7`
       set DD04 = `echo $NEW_DATE | cut -c9-10`
       set HH04 = `echo $NEW_DATE | cut -c12-13`
   endif
   if ( $num == 05 ) then
       set CY05 = `echo $NEW_DATE | cut -c1-4`
       set MM05 = `echo $NEW_DATE | cut -c6-7`
       set DD05 = `echo $NEW_DATE | cut -c9-10`
       set HH05 = `echo $NEW_DATE | cut -c12-13`
   endif
   if ( $num == 06 ) then
       set CY06 = `echo $NEW_DATE | cut -c1-4`
       set MM06 = `echo $NEW_DATE | cut -c6-7`
       set DD06 = `echo $NEW_DATE | cut -c9-10`
       set HH06 = `echo $NEW_DATE | cut -c12-13`
   endif
 end

echo '#Create WRF 4dVar runvar4dnl.csh :'

set arg1 = '$1'
set arg2 = '$2'

cat >! runvar4dnl.csh << EOF
#!/bin/csh -f

 set echo

echo begin runvar4dnl.csh

 if ( "$arg1" == "pre" && "$arg2" == "monitor" ) then
   cp namelist.var4dnl namelist.input

   if ( -e wrf_3dvar_output ) then
     mv wrf_3dvar_output wrfinput_d01
   else
     cp wrf_3dvar_input wrfinput_d01
   endif

 endif

 if ( "$arg1" == "post" && "$arg2" == "monitor" ) then
   if ( ! -d nl ) mkdir nl
   /bin/rm -f nl/namelist.input
   /bin/cp namelist.input nl

  cp wrfinput_d01                            fgat_fg.01
  mv wrf_3dvar_input_d01_${CY01}-${MM01}-${DD01}_${HH01}:00:00 fgat_fg.02
  mv wrf_3dvar_input_d01_${CY02}-${MM02}-${DD02}_${HH02}:00:00 fgat_fg.03
  mv wrf_3dvar_input_d01_${CY03}-${MM03}-${DD03}_${HH03}:00:00 fgat_fg.04
  mv wrf_3dvar_input_d01_${CY04}-${MM04}-${DD04}_${HH04}:00:00 fgat_fg.05
  mv wrf_3dvar_input_d01_${CY05}-${MM05}-${DD05}_${HH05}:00:00 fgat_fg.06
  mv wrf_3dvar_input_d01_${CY06}-${MM06}-${DD06}_${HH06}:00:00 fgat_fg.07
 endif

echo end runvar4dnl.csh

EOF


echo '#Create WRF 4dVar runvar4dtl.csh :'

 set NEW_DATE = `geth_new.exe ${DA_CY}-${DA_MM}-${DA_DD}-${DA_HH} ${DA_TIME_WINDOW}`
 set NEW_CY = `echo $NEW_DATE | cut -c1-4`
 set NEW_MM = `echo $NEW_DATE | cut -c6-7`
 set NEW_DD = `echo $NEW_DATE | cut -c9-10`
 set NEW_HH = `echo $NEW_DATE | cut -c12-13`

cat >! runvar4dtl.csh << EOF
#!/bin/csh -f

 set echo

echo begin runvar4dtl.csh

 if ( "$arg1" == "pre" && "$arg2" == "monitor" ) then

   cp tl00 wrfinput_d01
   cp namelist.var4dtl namelist.input

 endif

 if ( "$arg1" == "post" && "$arg2" == "monitor" ) then

   if ( ! -d tl ) mkdir tl
   /bin/cp namelist.input tl

  mv tl${CY01}-${MM01}-${DD01}_${HH01}:00:00 tl01
  mv tl${CY02}-${MM02}-${DD02}_${HH02}:00:00 tl02
  mv tl${CY03}-${MM03}-${DD03}_${HH03}:00:00 tl03
  mv tl${CY04}-${MM04}-${DD04}_${HH04}:00:00 tl04
  mv tl${CY05}-${MM05}-${DD05}_${HH05}:00:00 tl05
  mv tl${CY06}-${MM06}-${DD06}_${HH06}:00:00 tl06

  mv auxhist3_d01_${NEW_CY}-${NEW_MM}-${NEW_DD}_${NEW_HH}:00:00 tldf

  cp namelist.var4dnl namelist.input

 endif

echo end runvar4dtl.csh

EOF

echo '#Create WRF 4dVar runvar4dad.csh :'

cat >! runvar4dad.csh << EOF
#!/bin/csh -f

 set echo

 echo beginning runvar4dad.csh

 if ( "$arg1" == "pre" && "$arg2" == "monitor" ) then

 cp namelist.var4dad namelist.input

 cp wrf_3dvar_input wrfinput_d01

# rm tl0*

 mv af06 auxinput3_d01_${CY06}-${MM06}-${DD06}_${HH06}:00:00
 mv af05 auxinput3_d01_${CY05}-${MM05}-${DD05}_${HH05}:00:00
 mv af04 auxinput3_d01_${CY04}-${MM04}-${DD04}_${HH04}:00:00
 mv af03 auxinput3_d01_${CY03}-${MM03}-${DD03}_${HH03}:00:00
 mv af02 auxinput3_d01_${CY02}-${MM02}-${DD02}_${HH02}:00:00
 mv af01 auxinput3_d01_${CY02}-${MM01}-${DD01}_${HH01}:00:00
 mv af00 auxinput3_d01_${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00
 mv afdf auxinput3_d01_dfi

 endif

 if ( "$arg1" == "post" && "$arg2" == "monitor" ) then

   if ( ! -d ad ) mkdir ad
   /bin/cp namelist.input ad

   mv wrf_3dvar_input_d01_${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00 gr00

   cp namelist.var4dnl namelist.input

 endif

echo end of runvar4dad.csh

EOF

chmod +x runvar*.csh

echo '#-------------------------------------------------------------------'
echo '# [6.0] Run WRF-Var:'
echo '#-------------------------------------------------------------------'

setenv MP_PGMMODEL mpmd
setenv MP_CMDFILE poe.cmdfile

# determine number of processors from load-leveler

set ppp = ($LOADL_PROCESSOR_LIST)
set np=$#ppp

# construct the poe.cmdfile giving 1/4 of np to var and other half to model

if ( $np < 2 ) then
  echo must have at least two processors to run 4dvar
  exit
endif
@ num_a = $np / $VARFRACTION
if ( $num_a < 1 ) set num_a = 1

/bin/rm poe.cmdfile
set i=0
while ( $i < $num_a )
  echo "env BIND_TASKS=no da_3dvar.exe" >> poe.cmdfile
  @ i += 1
end
@ i = $num_a
while ( $i < $np )
  echo "env BIND_TASKS=no wrf.exe" >> poe.cmdfile
  @ i += 1
end

 touch stdout.ad stdout.tl stdout.nl
 cp namelist.var4dnl namelist.input

 poe

echo -n end whole darn thing
date

mv fort.12 DAProg_WRF-Var.statistics >&! /dev/null
mv fort.81 DAProg_WRF-Var.cost_fn >&! /dev/null
mv fort.82 DAProg_WRF-Var.grad_fn >&! /dev/null
mv fort.47 obsrelated
mv fort.48 jo
mv fort.50 obsagain
mv fort.60 qc

echo "WRF-4DVar completed"

echo '#-------------------------------------------------------------------'
echo '# [7.0] Save results:'
echo '#-------------------------------------------------------------------'

cp namelist.* $RES_DIR/

mv run*out* rsl.* var.* wrf_3dvar_output ${RES_DIR}/
mv DAProg_WRF-Var.statistics obsrelated jo obsagain qc DAProg_WRF-Var.cost_fn DAProg_WRF-Var.grad_fn ${RES_DIR}/
mv inc1 ANALYSIS_INCREMENT tl* gr00 ${RES_DIR}/

rm  wrf_3dvar_input wrfout* wrfinput_d01 fgat_fg* aux*

exit (0)
