#!/bin/ksh
#########################################################################
# Script: da_run_multi_increment_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to multi_increment configuration
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

##################################################
#                                                #
# Stage 1:  High resolution model                #
#                                                # 
##################################################

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
export EXPT=4dvar_minc_npe
export CLEAN=${CLEAN:-false}
export CYCLING=${CYCLING:-false}
export NL_INPUTOUT_BEGIN_H=0
export NL_NTMAX=1
export NL_VAR4D=true
export NL_VAR4D_MULTI_INC=1
export NL_VAR4D_COUPLING=1 # disk linear
#export NL_TRACE_UNIT=0
export NL_DEBUG_LEVEL=0
export WINDOW_START=0
export WINDOW_END=6
export NL_RUN_HOURS=6
if test $NL_VAR4D != true ; then
   export NL_NUM_FGAT_TIME=1
else
   export NL_NUM_FGAT_TIME=`expr $WINDOW_END \+ 1`
fi
#export FIRST=false

export NUM_PROCS=32
export NUM_PROCS_VAR=8
export NUM_PROCS_WRF=8
export QUEUE=debug
export PROJECT=64000420
export WALLCLOCK=30
export LL_PTILE=32
if test $NUM_PROCS -gt 1 ; then
   export NL_VAR4D_COUPLING=2 # disk linear
   export LSF_EXCLUSIVE=-I
   export SUBMIT="LSF"
   export RUN_CMD=mpirun.lsf
else
   export LSF_EXCLUSIVE=" "
   export SUBMIT="none"
   export RUN_CMD=" "
fi

#Time info:
export INITIAL_DATE=2005071421
export FINAL_DATE=2005071421
#Uncomment for actual runs: export LBC_FREQ=03
export CYCLE_PERIOD=6
export LONG_FCST_TIME_1=03
export LONG_FCST_TIME_2=09
export LONG_FCST_TIME_3=15
export LONG_FCST_TIME_4=21
export LONG_FCST_RANGE_1=51
export LONG_FCST_RANGE_2=51
export LONG_FCST_RANGE_3=51
export LONG_FCST_RANGE_4=51

#Directories:
#bluevista:
export REL_DIR=$HOME/compare
export DAT_DIR=$REL_DIR/../case_data
export EXP_DIR=/ptmp/$USER/$REGION/$EXPT
export WRFVAR_DIR=$REL_DIR/wrfvar4d
export WRFPLUS_DIR=$REL_DIR/wrfplus
export WRF_DIR=$REL_DIR/wrf
export WRF_BC_DIR=$WRFVAR_DIR
export REG_DIR=$DAT_DIR/$REGION
export OB_DIR=$REG_DIR/45km/ob
export RC_DIR=$REG_DIR/45km/rc
export RC_HIGH_DIR=$REG_DIR/45km/rc
export BE_DIR=$REG_DIR/45km/be

#From WPS (namelist.wps):
#export RUN_GEOGRID=false
export NL_E_WE=91
export NL_E_SN=73
export NL_DX=45000
export NL_DY=45000

#WRF:
export NL_TIME_STEP=240
export NL_NPROC_X=0
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
if [[ $NL_VAR4D_MULTI_INC = 1 ]]; then
   export NL_SFC_ASSI_OPTIONS=2
fi
#export NL_TEST_WRFVAR=true
#export NL_TEST_TRANSFORMS=true

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

##################################################
#                                                #
# Stage 2:  Low resolution model                 #
#                                                # 
##################################################

export OB_DIR_LOW=$REG_DIR/135km/ob
export RC_DIR_LOW=$REG_DIR/135km/rc
export BE_DIR_LOW=$REG_DIR/135km/be

#From WPS (namelist.wps):
export NL_E_WE_LOW=31
export NL_E_SN_LOW=25
export NL_DX_LOW=135000
export NL_DY_LOW=135000

#WRF:
export NL_TIME_STEP_LOW=720

export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0
