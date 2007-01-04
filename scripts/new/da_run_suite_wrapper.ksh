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
export EXPT=cy1
#export CLEAN=${CLEAN:-true}
export NUM_PROCS=2
export RUN_CMD="mpirun -np $NUM_PROCS"
export CYCLING=${CYCLING:-true}
export CYCLE_PERIOD=12
export FIRST=true

#Time info:
export INITIAL_DATE=2003010100
export FINAL_DATE=2003012800
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=24
export LONG_FCST_TIME_2=12
export LONG_FCST_RANGE_2=24

#Directories:
export REL_DIR=/data1/$USER/code/trunk
export WRF_BC_DIR=/data1/dmbarker/code/WRF_BC
export DAT_DIR=/data3/$USER/data
export WRFVAR_DIR=$REL_DIR/WRFVAR
export WPS_DIR=$REL_DIR/WPS
#export EXP_DIR=$HOME/data/con200/noda

#From WPS (namelist.wps):
#export RUN_GEOGRID=true
export WPS_GEOG_DIR=/data1/dmbarker/data/geog
#export GEOG_DATA_RES=10m

#WRF:
#export NL_CU_PHYSICS=2
export NL_DAMPCOEF=0.2
export NL_MP_ZERO_OUT=2

#WRF-Var:
export NL_CHECK_MAX_IV=.false.
export DA_BACK_ERRORS=/palm2/hclin/data/BES/con200_gen_be.ENS.dat
export NL_USE_AIRSRETOBS=.false.
export NL_TRACE_USE=.false.
export NL_QC_RAD=.false.
export NL_TRACE_MEMORY=.false.
export NL_LEN_SCALING1=1.0
export NL_LEN_SCALING2=1.0
export NL_LEN_SCALING3=1.0
export NL_LEN_SCALING4=1.0
export NL_LEN_SCALING5=1.0

${WRFVAR_DIR}/scripts/new/da_run_suite.ksh

exit 0

