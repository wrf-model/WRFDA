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

export RUN_RESTORE_DATA_NCEP=true
export RUN_RESTORE_DATA_RTOBS=true
export RUN_WRFSI=false
export RUN_WPS=true
export RUN_REAL=true
export RUN_OBSPROC=true
export RUN_WRFVAR=false
export RUN_UPDATE_BC=true
export RUN_WRF=true

export CYCLING=${CYCLING:-true}
export DUMMY=${DUMMY:-true}
export CLEAN=${CLEAN:-false}

#########################################################################
# [2] Override default environment variables here:
#########################################################################

# Job details:
export EXPT=${EXPT:-test}
export REGION=con200
export NUM_PROCS=${NUM_PROCS:-8}
export INITIAL_DATE=2003010100
export FINAL_DATE=2003010300
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=24
export CYCLE_PERIOD=6

# Disks, directories:
export REL_DIR=$HOME/trunk
export WRFVAR_DIR=$REL_DIR/wrfvar
export WRF_DIR=$REL_DIR/wrf
export WRF_NL_DIR=$REL_DIR/wrfvar_wrf_nl
export WPS_DIR=$REL_DIR/wps

export DAT_DIR=~/data
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/$EXPT/ob
export RC_DIR=$EXP_DIR/rc
export FC_DIR=$EXP_DIR/fc

# Resolution
export NL_E_WE=45
export NL_E_SN=45
export NL_DX=200000
export NL_DY=200000

# Uncomment after you've run SI once for this domain!
# export RUN_GRID_GEN=${RUN_GRID_GEN:-false}

# WRF:
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND=6}

export HOSTS=~bray/data/hosts/`hostname`.hosts
export HOSTS=`eval echo $HOSTS`

export SCRIPT=da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

