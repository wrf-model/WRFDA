#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script.
#
# Description:
#
# Here are a few examples of environment variables you may want to 
# change in da_run_suite_wrapper.ksh:
#
# 1) "export RUN_WRFVAR=true" runs WRFVAR).
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export INITIAL_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################
#set echo 

#Decide which stages to run (run if true):

#export RUN_RESTORE_DATA_NCEP=false
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WRFSI=true
export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
#export RUN_WRF=true

#Experiment details:
#export DUMMY=${DUMMY:-true}
export REGION=t46 # SEA domain of AFWA
export EXPT=noda  # domain check option is set to false!
export CLEAN=false
export CYCLE_PERIOD=6
#export CYCLING=true
#export FIRST=false # the dafult is true!

# LSF settings...
export JOBSUBMIT_TOOL=lsf
export LSF_EXCLUSIVE=" "
export NUM_PROCS=64
export QUEUE=premium  # Options:premium, regular, economy, share
export PROJECT_ID=68000001  # 64000420 for MMM.
export LSF_MAX_RUNTIME=10
export LL_PTILE=16

#Time info:
export INITIAL_DATE=2006100100 
export FINAL_DATE=2006100100  
export LBC_FREQ=03
export CYCLE_PERIOD=6
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=6
export LONG_FCST_RANGE_2=72
export LONG_FCST_RANGE_3=6
export LONG_FCST_RANGE_4=72

#Directories:
export OB_DIR=/rap/datc/data/t46/ob
#export REL_DIR=/rap/datc/demirtas/code
#export DAT_DIR=/rap/datc/data
export DAT_DIR=/ptmp/demirtas/data
export NCEP_DIR=/rap/datc/data
export WPS_GEOG_DIR=/mmm/users/wrfhelp/WPS_GEOG
export WRFVAR_DIR=$REL_DIR/wrfvar

# Namelist variables for 3DVAR_OBSPROC:
#export domain_check_h=.false. # default is  .true.,
#export TS0=290		      # From AFWA's t46.namelist.3dvar_obs
#export NL_E_WE=212       # From AFWA's t46.namelist.3dvar_obs
#export NL_E_SN=162       # From AFWA's t46.namelist.3dvar_obs
#export MAP_PROJ=lambert  # From AFWA's t46.namelist.3dvar_obs
#export REF_LAT=37.74     # From AFWA's t46.namelist.3dvar_obs
#export REF_LON=129.51    # From AFWA's t46.namelist.3dvar_obs
#export TRUELAT1=60.0     # From AFWA's t46.namelist.3dvar_obs
#export TRUELAT2=30.0     # From AFWA's t46.namelist.3dvar_obs
#export STAND_LON=129.51  # From AFWA's t46.namelist.3dvar_obs 
#export NL_DX=15000       # From AFWA's t46.namelist.3dvar_obs (DIS    =   15.00,) 
#DIS    =  ${NL_DX_KM} defined by da_run_obsproc.ksh
#export PREPBUFR_OUTPUT_FILENAME = 'obs_gts.3dvar',

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export DEBUG_LEVEL_UNGRIB=20
export NL_E_WE=162  # used also for WRF namelis.input 
export NL_E_SN=212  # used also for WRF namelis.input 
export REF_LAT=37.74
export REF_LON=129.51
export TRUELAT1=60.0
export TRUELAT2=30.0
export STAND_LON=129.51
export GEOG_DATA_RES=5m # note that da_run_wps.ksh has "30s" as default setting.
export NL_DX=15000  # used also for WRF namelis.input 
export NL_DY=15000  # used also for WRF namelis.input 

#WRF:
#export NL_HISTORY_INTERVAL=180
#export NL_INPUTOUT_INTERVAL=180 
export NL_HISTORY_INTERVAL=720   # Every 12 hours to save disk space
export NL_INPUTOUT_INTERVAL=360  # Every 6 hours to save disk space
export NL_TIME_STEP=90
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.995, 0.992, 0.983, 0.975, "\ 
                                      " 0.961, 0.949, 0.932, 0.917, 0.897, "\
                                      " 0.878, 0.855, 0.832, 0.806, 0.778, "\
                                      " 0.749, 0.718, 0.687, 0.654, 0.623, "\
                                      " 0.590, 0.559, 0.526, 0.495, 0.462, "\
                                      " 0.431, 0.398, 0.367, 0.334, 0.304, "\
                                      " 0.272, 0.244, 0.213, 0.187, 0.158, "\
                                      " 0.134, 0.107, 0.085, 0.060, 0.040, "\
                                      " 0.018, 0.000 "}
export NL_E_VERT=42
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_CU_PHYSICS=1
export NL_MP_ZERO_OUT=2
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0 
export NL_FEEDBACK=0 
export NL_INPUTOUT_BEGIN_H=6   #our previous setting: 3       # Output input format start.
export NL_INPUTOUT_END_H=6     #our previous setting: 9       # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6  # AFWA'S original is 6,     (recommend switch to 0, Dave 31 May 2007)
export NL_ADJUST_HEIGHTS=true     # AFWA'S original is .true. (recommend switch to .false., Dave 31 May 2007)
export NL_SPECIFIED=true
export NL_NIO_TASKS_PER_GROUP=0
export NL_NIO_GROUPS=1

#WRF-Var:
export NL_ANALYSIS_TYPE=QC-OBS
export NL_WRITE_FILTERED_OBS=.true.                  # It is "false" in AFWA's namelist and in Registry.wrfvar.
#export DA_BACK_ERRORS=/rap/datc/data/t46/be         # There are two be.dat files, be careful!

export NL_USE_PROFILEROBS=false   # (AFWA's original)
export NL_USE_QSCATOBS=false      # (AFWA's original)
export NL_USE_AMSUAOBS=true       # (AFWA's original) 
export NL_USE_AIRSRETOBS=false    # (AFWA's original)
export NL_USE_OBS_ERRFAC=true     # (AFWA's original)

export NL_CV_OPTIONS_HUM=1         #  (AFWA's original)
####export NL_MAXVERT_VAR5=0.0     #(AFWA's original) Dale is going to check with them.

export NL_RTMINIT_NSENSOR="1  1"       # (Registry.wrfvar) 
export NL_RTMINIT_PLATFORM="30 -1"      # (Registry.wrfvar)
export NL_RTMINIT_SATID="30 -1.0"      # (Registry.wrfvar)
export NL_RTMINIT_SENSOR="30 -1.0"     # (Registry.wrfvar)

export NL_BIASCORR=false               # (Registry.wrfvar)

export NL_WRITE_IV_RAD_ASCII=false     # (Registry.wrfvar) 
export NL_WRITE_OA_RAD_ASCII=false     # (Registry.wrfvar)
export NL_ERROR_FACTOR_RAD=false       # (Registry.wrfvar)

export NL_RTM_OPTION=2                 # (AFWA's original) CRTM.


# The following setting is specifically for producing "filtered_obs" from WRF-VAR runs
#export DA_BACK_ERRORS=${DAT_DIR}/${REGION}/be.dat
#export NL_ANALYSIS_TYPE=QC-OBS
#export NL_CHECK_MAX_IV=.true.
#export NL_NTMAX=0
#export NL_WRITE_INCREMENTS=.false. #(I think this is default. If so, remove this one)

export RUN_CMD=" "
$WRFVAR_DIR/scripts/da_run_suite.ksh

exit 0

