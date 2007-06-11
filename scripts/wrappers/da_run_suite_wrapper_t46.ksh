#!/bin/ksh
#########################################################################
set echo 

#Decide which stages to run (run if true):
export RUN_WPS=true
export RUN_REAL=true
export RUN_WRF=true

#Experiment details:
export REGION=t46
export EXPT=noda
export CYCLE_PERIOD=6
#export CYCLING=true
#export FIRST=false

#Scheduling:
export RUN_CMD=" "
export LSF_EXCLUSIVE=" "
#export LL_PTILE=16

#Time info:
export INITIAL_DATE=2006100100
export FINAL_DATE=2006100100
#export LONG_FCST_TIME_1=00
#export LONG_FCST_RANGE_1=72
#export LONG_FCST_TIME_2=12
#export LONG_FCST_RANGE_2=72
export LBC_FREQ=6    # Actual: 3

#Directories:
###smoke:
export REL_DIR=/smoke/dmbarker/code/trunk
export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC         # Which version?
export WRFVAR_DIR=$REL_DIR/wrfvar_afwa_2.2        # r2522.
export WPS_DIR=$REL_DIR/wps_r237                  # r237 AFWA release.
export WRF_BC_DIR=$REL_DIR/WRF_BC                 # Which version?
export WRF_DIR=$REL_DIR/wrf_r2234                 # 
export DAT_DIR=/smoke/dmbarker/data
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/ob
export WPS_GEOG_DIR=~wrfhelp/WPS_GEOG

#WPS (namelist.wps):
#export RUN_GEOGRID=false
export DEBUG_LEVEL_UNGRIB=20
export NL_E_WE=162
export NL_E_SN=212
#export NL_E_WE=54 # 15km: 162
#export NL_E_SN=71 # 15km: 212
export REF_LAT=37.74
export REF_LON=129.51
export TRUELAT1=60.0
export TRUELAT2=30.0
export STAND_LON=129.51
export GEOG_DATA_RES=5m
export NL_DX=15000
#export NL_DX=45000 #15km: 15000
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_TIME_STEP=90
#export NL_TIME_STEP=240 #15km: 90
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
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0

#WRF (not already covered above):
export NL_INPUTOUT_BEGIN_H=6   #our previous setting: 3       # Output input format start.
export NL_INPUTOUT_END_H=6     #our previous setting: 9       # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6  # AFWA'S original is 6,     (recommend switch to 0, Dave 31 May 2007)

#export NL_FEEDBACK=0            # Not in 2234? 
#export NL_ADJUST_HEIGHTS=true   # Not in 2234?
#export NL_NIO_TASKS_PER_GROUP=0 # Not in 2234?
#export NL_NIO_GROUPS=1          # Not in 2234?

#OBSPROC (not covered above):
#export TS0=265.

#WRF-Var:
#export NL_WRITE_INCREMENTS=.false.
#export NL_USE_SSMIRETRIEVALOBS=.false.
#export NL_VAR_SCALING4=0.01
#export NL_CALCULATE_CG_COST_FN=.true.
#export NL_USE_GPSREFOBS=.true.
#export NL_USE_AIRSRETOBS=.false.

#Continuous job 
export CONTJOB=n
export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
export MACHINE=smoke

#$WRFVAR_DIR/scripts/da_run_job.${MACHINE}.ksh
$SCRIPT

exit 0

