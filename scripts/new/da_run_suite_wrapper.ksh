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

#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_NCEP=true
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WRFSI=true
#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true
export RUN_WRFVAR=true
export RUN_UPDATE_BC=true
export RUN_WRF=true

#Experiment details:
#export DUMMY=${DUMMY:-true}
export EXPT=cs1
export RUN_CMD=" "
#export CLEAN=${CLEAN:-true}
#export CYCLING=${CYCLING:-true}

#Time info:
export INITIAL_DATE=2003010112
export FINAL_DATE=2003010112
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=24
export LONG_FCST_TIME_2=12
export LONG_FCST_RANGE_2=24

#Directories:
export DATA_DISK=/smoke
export REL_DIR=${DATA_DISK}/$USER/code/trunk
export WRFVAR_DIR=$REL_DIR/wrfvar
export WRF_BC_DIR=/smoke/dmbarker/code/WRF_BC
export DAT_DIR=${DATA_DISK}/$USER/data

#From WPS (namelist.wps):
export RUN_GEOGRID=false
export WPS_GEOG_DIR=${DAT_DIR}/geog

export SCRIPT=${WRFVAR_DIR}/scripts/new/da_run_suite.ksh

$RUN_CMD $SCRIPT

exit 0

