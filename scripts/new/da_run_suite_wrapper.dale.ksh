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
#export RUN_RESTORE_DATA_RTOBS=true
#export RUN_WPS=true
#export RUN_REAL=true

#########################################################################
# [2] Override default environment variables here:
#########################################################################

# Job details:
export JOB_QUEUE=share
export WALL_CLOCK_TIME=1


#export EXPT=noobs
#export NUM_PROCS=8
#export INITIAL_DATE=2003010100
#export FINAL_DATE=2003010100
#export LONG_FCST_TIME_1=00
#export LONG_FCST_RANGE_1=72

# Disks, directories:
export REL_DIR=$HOME/code/trunk
export DAT_DIR=$HOME/data3/data
#export DAT_DIR=/mmm/users/dmbarker/data

#WPS:
export WPS_GEOG_DIR=/mmm/users/gill/DATA/GEOG
#export HOSTS=~bray/data/hosts/`hostname`.hosts
#export HOSTS=`eval echo $HOSTS`

#export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
#$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

