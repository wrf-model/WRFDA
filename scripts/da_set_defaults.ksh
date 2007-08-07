#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_set_namelist_defaults.ksh
#
# Purpose: This scripts sets the environment variables used within the 
# entire scripts system to values corresponding to a "standard case".
# The standard case currently used is the con200 application.
# The namelist parameters specified here is that sub-set of the entire
# range of parameters for all namelists that we have found necessary
# to change through experience of the applications tested so far. As
# new applications are tests, additional environment valiables may
# be added. 

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

#Decide which stages to run (run if true):

export RUN_RESTORE_DATA_GRIB=${RUN_RESTORE_DATA_GRIB:-false}
export RUN_RESTORE_DATA_RTOBS=${RUN_RESTORE_DATA_RTOBS:-false}
export RUN_WPS=${RUN_WPS:-false}
export RUN_WRFSI=${RUN_WRFSI:-false}
export RUN_REAL=${RUN_REAL:-false}
export RUN_OBSPROC=${RUN_OBSPROC:-false}
export RUN_WRFVAR=${RUN_WRFVAR:-false}
export RUN_UPDATE_BC=${RUN_UPDATE_BC:-false}
export RUN_WRF=${RUN_WRF:-false}

#Experiment details:
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}                            # Domain name.
export EXPT=${EXPT:-test}                              # Experiment name.
export SOLVER=${SOLVER:-em}
export CLEAN=${CLEAN:-false}
export CYCLING=${CYCLING:-false}                       # Cold start (false), cycle (true).
export FIRST=${FIRST:-true}                            # Cold start (false), cycle (true).
export CHECK_SVNVERSION=${CHECK_SVNVERSION:-true}
export UPDATE_CYCLING=${UPDATE_CYCLING:-false}  # Combination of cold start and cycling runs for AFWa Projects: cold start (for 00,12) cycling (for 06,18)
export FG_TYPE=${FG_TYPE:-fnl}

#Scheduling:
export PROJECT_ID=${PROJECT_ID:-48500053}
export QUEUE=${QUEUE:-regular}
export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors for WRF-Var/WRF.
export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
export LSF_EXCLUSIVE=${LSF_EXCLUSIVE:--x}
export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-10} # minutes
export LL_PTILE=${LL_PTILE:-1} # minutes
export PREV_JOBID=${PREV_JOBID:-test}
export POE=${POE:-false}
export HOSTS=${HOSTS:-${HOME}/hosts}
if test -f $HOSTS; then
   export RUN_CMD=${RUN_CMD:-mpirun -machinefile $HOSTS -np $NUM_PROCS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}            # Directory containing codes.
export OBSPROC_DIR=${OBSPROC_DIR:-$REL_DIR/3DVAR_OBSPROC} # Observation preprocessor dir.
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}  # WRF-Var code directory.
export BUILD_DIR=${BUILD_DIR:-$WRFVAR_DIR/build}  # WRF-Var executable location.
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}           # WPS directory.
export WRFSI_DIR=${WRFSI_DIR:-$REL_DIR/wrfsi}     # WRF SI directory.
export WRF_BC_DIR=${WRF_BC_DIR:-$REL_DIR/WRF_BC}  # Update_bc dir.
export WPB_DIR=${WPB_DIR:-$REL_DIR/wpb}           # Perturbed LBC dir.
export WRF_DIR=${WRF_DIR:-$REL_DIR/WRFV2}         # WRF directory.
export WRF_NL_DIR=${WRF_NL_DIR:-$REL_DIR/wrf_nl}  # WRF namelist directory?
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus} # WRF TL/ADJ directory.
export DAT_DIR=${DAT_DIR:-$HOME/data}             # Top-level data directory.
export MSS_GRIB_DIR=${MSS_GRIB_DIR:-mss:/DSS/DS083.2/data}
export GRIB_DIR=${GRIB_DIR:-$DAT_DIR/${FG_TYPE}}         # GRIB input data dir.
export MSS_RTOBS_DIR=${MSS_RTOBS_DIR:-mss:/BRESCH/RT/DATA}
export RTOBS_DIR=${RTOBS_DIR:-$DAT_DIR/rtobs}     # Real-time observation directory.
export OB_DIR=${OB_DIR:-$REG_DIR/ob}              # Observation data dir.
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}       # Region-specific data dir.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}         # Experiment-specific data dir.
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run}           # Run dir.
export WORK_DIR=${WORK_DIR:-$RUN_DIR/working}     # Temporary working dir.
export RC_DIR=${RC_DIR:-$REG_DIR/rc}              # Reconfiguration directory
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}              # Forecast directory
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
export SCRIPT=${SCRIPT:-$SCRIPTS_DIR/da_run_wrfvar.ksh}

#Time info:
export DATE=${DATE:-2003010100}                   # Current date.
export INITIAL_DATE=${INITIAL_DATE:-2003010100}   # Start date of test period
export FINAL_DATE=${FINAL_DATE:-2003012800}       # Final date of test period.
export LBC_FREQ=${LBC_FREQ:-06}
let LBC_FREQ_SS=$LBC_FREQ*3600
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}                # Assimilation frequency.
export OBS_FREQ=${OBS_FREQ:-12}
export WINDOW_START=${WINDOW_START:-0}                 # Start ob window difference (hrs).
export WINDOW_END=${WINDOW_END:-0}                     # End ob window difference (hrs).
export LONG_FCST_TIME_1=${LONG_FCST_TIME_1:-99}
export LONG_FCST_TIME_2=${LONG_FCST_TIME_2:-99}
export LONG_FCST_TIME_3=${LONG_FCST_TIME_3:-99}
export LONG_FCST_TIME_4=${LONG_FCST_TIME_4:-99}
export LONG_FCST_RANGE_1=${LONG_FCST_RANGE_1:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_2=${LONG_FCST_RANGE_2:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_3=${LONG_FCST_RANGE_3:-$CYCLE_PERIOD}
export LONG_FCST_RANGE_4=${LONG_FCST_RANGE_4:-$CYCLE_PERIOD}

#Diagnostics:
export OK='<FONT COLOR="green">'
export ERR='<FONT COLOR="red">'
export END='</FONT>'

#WPS:
export RUN_GEOGRID=${RUN_GEOGRID:-true}                 # Run GEOGRID, or not.
export RUN_UNGRIB_METGRID_AFWA=${RUN_UNGRIB_METGRID_AFWA:-false}
export RUN_UNGRIB_METGRID_KMA=${RUN_UNGRIB_METGRID_KMA:-false}
export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}
#export WPS_GEOG_DIR=${WPS_GEOG_DIR:-~wrfhelp/WPS_GEOG} 
export WPS_GEOG_DIR=${WPS_GEOG_DIR:-/mmm/users/wrfhelp/WPS_GEOG} 
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export MAP_PROJ=${MAP_PROJ:-lambert}
export REF_LAT=${REF_LAT:-40.0}
export REF_LON=${REF_LON:--98.0}
export REF_X=${REF_X:-NAN}
export REF_Y=${REF_Y:-NAN}
export TRUELAT1=${TRUELAT1:-30.0}
export TRUELAT2=${TRUELAT2:-60.0}
export STAND_LON=${STAND_LON:--98.0}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}
export GEOG_DATA_RES=${GEOG_DATA_RES:-30s}
export VTABLE_TYPE=${VTABLE_TYPE:-GFS}
export CONSTANTS1=${CONSTANTS1:-*}
export CONSTANTS2=${CONSTANTS2:-*}
export DEBUG_LEVEL=${DEBUG_LEVEL:-0}

#WRF real (not already covered above):
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export NL_FRAMES_PER_OUTFILE=${NL_FRAMES_PER_OUTFILE:-1}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                        " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                        " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                        " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                        " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000"}
export NL_E_VERT=${NL_E_VERT:-28}                   #
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           #
export NL_RADT=${NL_RADT:-30}                #
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_NUM_SOIL_LAYERS=${NL_NUM_SOIL_LAYERS:-5}
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_CUDT=${NL_CUDT:-5}           #(1=, 2=,3=).
export NL_W_DAMPING=${NL_W_DAMPING:-0}            #
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             #
export NL_KM_OPT=${NL_KM_OPT:-1}               #
export NL_BASE_TEMP=${NL_BASE_TEMP:-290.0}               #
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-6}    #
export NL_SPECIFIED=${NL_SPECIFIED:-true}          #

#WRF (not already covered above):
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-true}
export NL_INPUT_FROM_FILE=${NL_INPUT_FROM_FILE:-true}
export NL_INPUT_OUTNAME=${NL_INPUT_OUTNAME:-'wrf_3dvar_input_d<domain>_<date>'}
export NL_INPUTOUT_INTERVAL=${NL_INPUTOUT_INTERVAL:-360}
export NL_INPUTOUT_BEGIN_H=${NL_INPUTOUT_BEGIN_H:-$CYCLE_PERIOD} # Output input format start.
export NL_INPUTOUT_END_H=${NL_INPUTOUT_END_H:-$FCST_RANGE}       # Output input format end.
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}

#OBSPROC (not covered above):
export MAX_OB_RANGE=${MAX_OB_RANGE:-2}             # Maximum difference O, B (hours)
export MAX_NUMBER_OF_OBS=${MAX_NUMBER_OF_OBS:-70000}
export THINING_SATOB=${THINING_SATOB:-.FALSE.}
export THINING_SSMI=${THINING_SSMI:-.FALSE.}
export THINING_QSCAT=${THINING_QSCAT:-.FALSE.}
export PS0=${PS0:-100000.0}
export TS0=${TS0:-300.0}
export TLP=${TLP:-50.0}

#WRF-Var (not covered above):
export NL_ANALYSIS_TYPE=${NL_ANALYSIS_TYPE:-"3D-VAR"}  # Analysis type.
export NL_VAR4D=${NL_VAR4D:-false}
export BE_DIR=${BE_DIR:-$REG_DIR/be}                   # Background error covariance directory.
export OB_DIR=${OB_DIR:-$REG_DIR/ob}                   # Observation directory.
export DA_DIR=${DA_DIR:-$EXP_DIR/da}                   # Forecast directory
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # background errors.
export NL_OB_FORMAT=${NL_OB_FORMAT:-2}   # Observation format: 1=BUFR, 2=ASCII "little_r"
export NL_LATS_STATS_OPTION_FALSE=${NL_LATS_STATS_OPTION_FALSE:-false}
export NL_CV_OPTIONS_HUM=${NL_CV_OPTIONS_HUM:-1} # Humidity control variable.
export NL_CHECK_MAX_IV=${NL_CHECK_MAX_IV:-true} # QC on O-B differences.
export NL_NTMAX=${NL_NTMAX:-100}         # Maximum number of inner loop iterations.
export NL_CHECK_RH=${NL_CHECK_RH:-2}     # RH bounds check.
#From Update_BC:
export PHASE=${PHASE:-false}     # Indicate which phase update_bc is.

#Ensemble parameters:
export NUM_JOBS=${NUM_JOBS:-1}                         # Number of parallel jobs to run.
export NUM_MEMBERS=${NUM_MEMBERS:-1}                   # Number of ensemble members.
export MEM=${MEM:-1}                                   # Ensemble member.
export FILE_TYPE=${FILE_TYPE:-wrf_3dvar_input_d01}     # ETKF input file-type.
export NV=${NV:-15}                                    # Number of ETKF variables.
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'TSLB'", "'TSK'", \
                "'QCLOUD'", "'QRAIN'", "'QVAPOR'", "'U10'", "'V10'", "'T2'", "'Q2'"} # ETKF variable names
export NACCUMT1=${NACCUMT1:-1}                         # ETKF parameter.
export NACCUMT2=${NACCUMT2:--1}                        # ETKF parameter.
export NSTARTACCUM1=${NSTARTACCUM1:-1}                 # ETKF parameter.
export NSTARTACCUM2=${NSTARTACCUM2:-1}                 # ETKF parameter.
export CYCLE_NUMBER=${CYCLE_NUMBER:-0}                 # ETKF parameter.
export TAINFLATINPUT=${TAINFLATINPUT:-1.0}             # ETKF parameter.
export RHOINPUT=${RHOINPUT:-1.0}                       # ETKF parameter.

