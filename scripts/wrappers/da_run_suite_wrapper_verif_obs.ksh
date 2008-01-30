#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper_verif_obs.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite_verif_obs.ksh 
# Author:    Syed RH Rizvi,    NCAR/MMM   
#########################################################################

#Experiment details:
export REGION=amps1
export EXPT=CY2_verify        
export CYCLE_PERIOD=12

#Scheduling:
export PROJECT=64000510       # DATC GAUs.
export QUEUE=debug    #  debug #  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
export NUM_PROCS=8 
export SUBMIT=LSF
export WALLCLOCK=10       

#Time info:

export INITIAL_DATE=2006100200
export FINAL_DATE=2006100212 
export VERIFY_HOUR=24      # 0 for analysis 

#WRF-Var Directory:   
export WRFVAR_DIR=/ptmp/rizvi/trunk           

#DATA Directories:
export DAT_DIR=/ptmp/rizvi/data
export FILTERED_OBS_DIR=/ptmp/rizvi/data/amps1/noda                   
export VERIFICATION_FILE_STRING='wrf_3dvar_input'
export FC_DIR=/ptmp/rizvi/data/amps1/CY2/fc

export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/${EXPT}_H${VERIFY_HOUR}

# Namelist options
export NL_E_WE=165
export NL_E_SN=217
export MAP_PROJ=polar
export REF_LAT=-87.4
export REF_LON=180.0
export TRUELAT1=-71.0
export TRUELAT2=-91.0
export STAND_LON=180.0
export NL_DX=60000
export NL_DY=60000
export NL_E_VERT=31

#Continuous job

export SCRIPTS_DIR=$WRFVAR_DIR/scripts
export SCRIPT=${SCRIPTS_DIR}/da_run_suite_verif_obs.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

