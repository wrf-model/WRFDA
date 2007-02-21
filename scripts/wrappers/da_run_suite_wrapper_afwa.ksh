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
export RUN_RESTORE_DATA_GRIB=false
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WRFSI=true
#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_WRFVAR=true
#export RUN_UPDATE_BC=true
export RUN_WRF=true

#Experiment details:
#export DUMMY=${DUMMY:-true}
export REGION=t44
export EXPT=noda
#export FCYCLE_HOUR=00
export CLEAN=${CLEAN:-false}
#export CYCLING=${CYCLING:-true}
#export FIRST=false

export LSF_EXCLUSIVE=" "
export NUM_PROCS=64
export QUEUE=premium
#export QUEUE=economy
#export PROJECT_ID=68000001
#export PROJECT_ID=64000420
export LSF_MAX_RUNTIME=350
export LL_PTILE=16
#export SUBMIT="bsub -a mpich_gm -n $NUM_PROCS -o $EXPT.out -e $EXPT.err -q $JOB_QUEUE -P $PROJECT_ID -W $WALL_CLOCK_TIME" 
#export RUN_CMD=mpirun.lsf

#Time info:
export INITIAL_DATE=2006102512
export FINAL_DATE=2006102718
export LBC_FREQ=03
export CYCLE_PERIOD=6
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=24
export LONG_FCST_RANGE_2=72
export LONG_FCST_RANGE_3=24
export LONG_FCST_RANGE_4=72

#Directories:
#bluevista:
export REL_DIR=/homebv/demirtas/trunk_rsl_xlf_bluevista
export DAT_DIR=/rap/datc/data
export NCEP_DIR=/rap/datc/data
export WPS_GEOG_DIR=/mmm/users/wrfhelp/WPS_GEOG

#smoke:
#export REL_DIR=/smoke/dmbarker/code/trunk/Vtable.AGRWRF
#export DAT_DIR=/smoke/dmbarker/data
#export WPS_GEOG_DIR=/smoke/dmbarker/data/geog

export WRFVAR_DIR=$REL_DIR/wrfvar

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export DEBUG_LEVEL_UNGRIB=200
export NL_E_WE=301
export NL_E_SN=238
export REF_LAT=31.4
export REF_LON=52.55
export TRUELAT1=60.0
export TRUELAT2=30.0
export STAND_LON=52.55
export NL_DX=15000
export NL_DY=15000

#WRF:
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
export NL_CU_PHYSICS=3
export NL_MP_ZERO_OUT=2
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0 # What does this mean Jimy?
export NL_FEEDBACK=0 
export NL_INPUTOUT_INTERVAL=180 

#WRF-Var:
#export NL_CHECK_MAX_IV=.false.

export SCRIPT=$WRFVAR_DIR/scripts/new/da_run_suite.ksh
export MACHINE=bluevista
$WRFVAR_DIR/scripts/new/da_run_job.${MACHINE}.ksh

export RUN_CMD=" "
$WRFVAR_DIR/scripts/new/da_run_suite.ksh

exit 0

