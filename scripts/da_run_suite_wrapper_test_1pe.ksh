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
export RUN_RESTORE_DATA_NCEP=false
export RUN_RESTORE_DATA_RTOBS=false
export RUN_WRFSI=false
export RUN_WPS=false
export RUN_REAL=false
export RUN_OBSPROC=false
export RUN_WRFVAR=true
export RUN_UPDATE_BC=true
export RUN_WRF=true
export CHECK_SVNVERSION=false

#Experiment details:
export DUMMY=${DUMMY:-false}
export REGION=cwb
export EXPT=test
export CLEAN=${CLEAN:-false}
export CYCLING=${CYCLING:-true}
export NL_INPUTOUT_BEGIN_H=0
export NL_NTMAX=5
export NL_VAR4D=true
#export NL_TRACE_UNIT=0
export NL_DEBUG_LEVEL=0
export NL_VAR4D_COUPLING=1 # disk linear
export WINDOW_START=0
export WINDOW_END=6
export NL_RUN_HOURS=6
if test $NL_VAR4D != true ; then
   export NL_NUM_FGAT_TIME=1
else
   export NL_NUM_FGAT_TIME=`expr $WINDOW_END \+ 1`
fi
#export FIRST=false

export LSF_EXCLUSIVE=" "
export NUM_PROCS=1
#export QUEUE=premium
export QUEUE=share
#export PROJECT_ID=68000001
export PROJECT_ID=64000400
export LSF_MAX_RUNTIME=180
export LL_PTILE=16
export SUBMIT="none" 
export RUN_CMD=" "

#Time info:
export INITIAL_DATE=2005071600
export FINAL_DATE=2005071600
#Uncomment for actual runs: export LBC_FREQ=03
export CYCLE_PERIOD=6
export LONG_FCST_TIME_1=00
export LONG_FCST_TIME_2=06
export LONG_FCST_TIME_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_1=06
export LONG_FCST_RANGE_2=06
export LONG_FCST_RANGE_3=06
export LONG_FCST_RANGE_4=06

#Directories:
#bluevista:
export REL_DIR=$HOME/4DVAR_Optimization
export DAT_DIR=/ptmp/hender/4DVAR_Optimization/case_data
export EXP_DIR=/ptmp/hender/4DVAR_Optimization/$REGION/$EXPT
export WRFVAR_DIR=$REL_DIR/wrfvar
export WRFPLUS_DIR=$REL_DIR/wrfplus
export WRF_DIR=$REL_DIR/wrf
export WRF_BC_DIR=$WRFVAR_DIR/build
export REG_DIR=$DAT_DIR/$REGION
export OB_DIR=$REG_DIR/135km/ob
export RC_DIR=$REG_DIR/135km/rc
export BE_DIR=$REG_DIR/135km/be
#export NCEP_DIR=/mmm/users/dmbarker/data/ncep
#export DAT_DIR=/ptmp/dmbarker/data
#export WPS_GEOG_DIR=/mmm/users/wrfhelp/WPS_GEOG
#smoke:
#export REL_DIR=/smoke/dmbarker/code/trunk
#export DAT_DIR=/smoke/dmbarker/data
#export WPS_GEOG_DIR=/smoke/dmbarker/data/geog

#From WPS (namelist.wps):
#export RUN_GEOGRID=false
export NL_E_WE=31
export NL_E_SN=25
export NL_DX=135000
export NL_DY=135000

#WRF:
export NL_TIME_STEP=600
export NL_E_VERT=17
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

#WRF-Var:
export NL_CHECK_MAX_IV=true
#export NL_TESTING_WRFVAR=true
#export NL_TEST_TRANSFORMS=true

#WRF-plus adjoint optimization:
# TBH:  Hacking in new namelist settings here.
# TBH:  "REMOVE_*" namelist variables are .FALSE. by default
#export NL_REMOVE_RUNGE_KUTTA_LOOR=true
#export NL_REMOVE_SMALL_STEP=true
#export NL_REMOVE_RKTEND_THRU_SS=true
#export NL_REMOVE_RKTEND=true
#export NL_REMOVE_RK_STEP_PREP=true

#JCDF Option & Obs
export NL_JCDFI_USE=false
export NL_JCDFI_TAUC=`expr $WINDOW_END \* 3600`
export NL_JCDFI_GAMA=0.1
export NL_JCDFI_ERROR_WIND=3.0
export NL_JCDFI_ERROR_T=1.0
export NL_JCDFI_ERROR_Q=0.001
export NL_JCDFI_ERROR_MU=1000.

export NL_USE_SYNOPOBS=true
export NL_USE_SHIPSOBS=true
export NL_USE_METAROBS=true
export NL_USE_SOUNDOBS=true
export NL_USE_PILOTOBS=true
export NL_USE_AIREPOBS=true
export NL_USE_GEOAMVOBS=true
export NL_USE_POLARAMVOBS=true
export NL_USE_BOGUSOBS=true
export NL_USE_BUOYOBS=true
export NL_USE_PROFILEROBS=true
export NL_USE_SATEMOBS=true
export NL_USE_GPSPWOBS=true
export NL_USE_GPSREFOBS=true
export NL_USE_QSCATOBS=true

export NL_LEN_SCALING1=0.5
export NL_LEN_SCALING2=0.5
export NL_LEN_SCALING3=0.5
export NL_LEN_SCALING4=0.5
export NL_LEN_SCALING5=0.5


export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

