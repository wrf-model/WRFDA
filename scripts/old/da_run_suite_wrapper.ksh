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

#########################################################################
#[1] Decide what stages to run:
#########################################################################

export RUN_RESTORE_DATA_NCEP=false
export RUN_RESTORE_DATA_RTOBS=false
export RUN_WRFSI=false
export RUN_WPS=false
export RUN_REAL=false
export RUN_OBSPROC=false
export RUN_WRFVAR=true
export RUN_UPDATE_BC=true
export RUN_WRF=true

export CYCLING=${CYCLING:-true}
export DUMMY=${DUMMY:-false}
export CLEAN=${CLEAN:-true}

#########################################################################
# [2] Override default environment variables here:
#########################################################################

# Job details:
export EXPT=${EXPT:-test}
export REGION=cwb
export NUM_PROCS=${NUM_PROCS:-1}
export INITIAL_DATE=2005071600
export FINAL_DATE=2005071700
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=24
export CYCLE_PERIOD=6
export NL_NTMAX=1
export NL_VAR4D=true 
#export NL_TRACE_UNIT=0
export NL_DEBUG_LEVEL=0
export NL_VAR4D_COUPLING=1 # disk linear
export WINDOW_START=0
export WINDOW_END=6
if test $NL_VAR4D != true ; then
   export NL_NUM_FGAT_TIME=1
else
   export NL_NUM_FGAT_TIME=`expr $WINDOW_END \+ 1`
fi



# Disks, directories:
export REL_DIR=$HOME/trunk
export WRFVAR_DIR=$REL_DIR/wrfvar
export WRFPLUS_DIR=$REL_DIR/wrfplus
export WRF_DIR=$REL_DIR/wrf
export WPS_DIR=$REL_DIR/wps

export DAT_DIR=/mmmtmp/xinzhang/wrf
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=~/willow/$REGION/$EXPT
export OB_DIR=$REG_DIR/ob
export RC_DIR=$REG_DIR/rc
export FC_DIR=$EXP_DIR/fc

# Resolution
export NL_E_WE=31
export NL_E_SN=25
export NL_E_VERT=17
export NL_TIME_STEP=600
export NL_DX=135000
export NL_DY=135000

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
export NL_USE_BOGUSOBS=false
export NL_USE_BUOYOBS=true
export NL_USE_PROFILEROBS=true
export NL_USE_SATEMOBS=true
export NL_USE_GPSPWOBS=true
export NL_USE_GPSREFOBS=true
export NL_USE_QSCATOBS=true



# Uncomment after you've run SI once for this domain!
# export RUN_GRID_GEN=${RUN_GRID_GEN:-false}

# WRF:
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND=6}

export HOSTS=~bray/data/hosts/`hostname`.hosts
export HOSTS=`eval echo $HOSTS`

export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

