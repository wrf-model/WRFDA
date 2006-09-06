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
# 1) "export RUN_WPS=true" runs the WPS).
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export START_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################

#########################################################################
#[1] Decide what stages to run:
#########################################################################

export RUN_RESTORE_DATA_NCEP=${RUN_RESTORE_DATA_NCEP:-true}
export RUN_WRFSI=${RUN_WRFSI:-true}
export RUN_WPS=${RUN_WPS:-false}
export RUN_REAL=${RUN_REAL:-true}

export RUN_OBSPROC=${RUN_OBSPROC:-true}
export OB_DIR=$REG_DIR/$EXPT/ob

export RUN_WRFVAR=${RUN_WRFVAR:-true}

export RUN_UPDATE_BC=${RUN_UPDATE_BC:-true}
export RUN_WRF=${RUN_WRF:-true}
# export CYCLING=${CYCLING:-true}
export DUMMY=true

#########################################################################
# [2] Override default environment variables here:
#########################################################################

# Disks, directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}

# Job details:
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export NUM_PROCS=${NUM_PROCS:-8}
export INITIAL_DATE=${INITIAL_DATE:-2003010100}
export FINAL_DATE=${FINAL_DATE:-2003010300}
export LONG_FCST_RANGE=${LONG_FCST_RANGE:-24}
export CYCLE_PERIOD=${CYCLE_PERIOD:-6}
# Resolution
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}

# Uncomment after you've run SI once for this domain!
# export RUN_GRID_GEN=${RUN_GRID_GEN:-false}
# export VERTICAL_LEVELS - do this directly in da_run_wrfsi.ksh.

# WRF:
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND=6}

# WRFVAR:

export SCRIPT=da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

