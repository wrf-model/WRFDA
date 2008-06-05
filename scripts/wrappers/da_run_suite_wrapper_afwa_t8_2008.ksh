#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          specific to AFWA Project t8_15km_2008 setting.
#
# This script is an updated version of 
# "da_run_suite_wrapper_afwa_t8.ksh" by Syed Rizvi (ESSL/MMM)
# 
# Current users: Meral Demirtas (RAL/DATC), Syed Rizvi (ESSL/MMM)
# 
#########################################################################

#Decide which stages to run (run if true):
export RUN_WPS=false #true
export RUN_REAL=false #true  
export RUN_WRFVAR=true 
export RUN_UPDATE_BC=true
export RUN_WRF=true
export RUN_OBSPROC=false

#Experiment details:
export REGION=t8_datc
export RUN_UNGRIB_METGRID_AFWA=true  
export METGRID_TABLE_TYPE=AFWA
export FG_TYPE=GFS
export EXPT=qvar         
export CYCLE_PERIOD=6  
export CYCLING=true
export CYCLE_NUMBER=1  #Give this number bigger than 0 integer after first cycling run.
export CLEAN=true

#Scheduling:
export PROJECT=48503001       # 48503001 DATC GAUs for authorized users. 
export QUEUE=regular  #economy   # use "share or debug" queues for:WPS,UPDATE_BC and OBS_PROC 
export NUM_PROCS=64  
export SUBMIT=LSF
export WALLCLOCK=165         
#export PREV_JOBNAME=${EXPT}_c2007082100_18                      # Use when making cycling runs
export JOBNAME=${EXPT}_c2007082200_18
#export SUBMIT_OPTIONS2="#BSUB -w \"done(${PREV_JOBNAME})\""      # Use when making cycling runs
#export SUBMIT_OPTIONS2=" "
# WRF: 48hrs run 64min , 6hrs run 8-9min.
# cycling run for 1 day (4 runs): 156minuntes at least. Let's use 165

#Time info:

export INITIAL_DATE=2007082200
export   FINAL_DATE=2007082218
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=48  
export LONG_FCST_TIME_2=06    # Use 9 for FGAT
export LONG_FCST_RANGE_2=06  
export LONG_FCST_TIME_3=12
export LONG_FCST_RANGE_3=48
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_4=06   # Use 9 for FGAT
export LBC_FREQ=3 

#Directories:
export DAT_DIR=/ptmp/demirtas/data
export REL_DIR=/rap/datc/demirtas/code/bluevista/may_2008
export WPS_DIR=$REL_DIR/WPS               # https://svn-wrf-wps.cgd.ucar.edu/tags/RELEASE-3-0/ 
export WRF_DIR=$REL_DIR/WRFV3		  # https://svn-wrf-model.cgd.ucar.edu/tags/release-v3-0-1/                     
export WRFVAR_DIR=$REL_DIR/WRFVAR         # https://svn-wrf-var.cgd.ucar.edu/branches/DATC  
export OBSPROC_DIR=$WRFVAR_DIR/obsproc    
export WRF_BC_DIR=$WRFVAR_DIR/da/da_update_bc       
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export RC_DIR=$REG_DIR/rc
export RUN_DIR=$EXP_DIR
export OB_DIR=$REG_DIR/ob
export GRIB_DIR=$DAT_DIR/$FG_TYPE
export SCRIPTS_DIR=$WRFVAR_DIR/scripts


#WPS (namelist.wps):
export MAP_PROJ=mercator
export RUN_GEOGRID=false 
export NL_E_WE=418   
export NL_E_SN=280   
export REF_LAT=20.0
export REF_LON=-75.0
export TRUELAT1=20.0
export TRUELAT2=20.0
export STAND_LON=-75.00
export GEOG_DATA_RES=5m
export NL_DX=15000
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_TIME_STEP=90    
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.00000,0.99730,0.99157,0.98506,0.97772, "\
                                      " 0.96943,0.96010,0.94962,0.93789,0.92478, "\
                                      " 0.91020,0.89403,0.87618,0.85657,0.83514, "\
                                      " 0.81183,0.78664,0.75960,0.73078,0.70027, "\
    " 0.66825,0.63491,0.60048,0.56527,0.52957,0.49370,0.45802,0.42281,0.38842, "\
    " 0.35510,0.32311,0.29265,0.26386,0.23686,0.21172,0.18845,0.16706,0.14749, "\
    " 0.12968,0.11355,0.09901,0.08593,0.07423,0.06378,0.05447,0.04621,0.03888, "\
    " 0.03240,0.02668,0.02163,0.01718,0.01328,0.00984,0.00682,0.00418,0.00186, "\
    " 0.00000 "}
export NL_E_VERT=57
export NL_P_TOP_REQUESTED=1000
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0

#WRF (not already covered above):
export NL_HISTORY_INTERVAL=360  
#export NL_INPUTOUT_INTERVAL=360     # Every 6 hours to save disk space
#export NL_INPUTOUT_BEGIN_H=6        # Output input format start.
#export NL_INPUTOUT_END_H=6          # Output input format end.
export NL_FEEDBACK=0            
export NL_ADJUST_HEIGHTS=true          
export NL_MP_ZERO_OUT=0
export NL_PD_MOIST=.true.
export NL_ADJUST_HEIGHTS=.false. #DATC's new setting for AFWA's new as of May 2008
export NL_FORCE_SFC_IN_VINTERP=3 #DATC's new setting for AFWA's new as of May 2008   

#WRF-Var:

export NL_LAT_STATS_OPTION=false       #Note that default is "true". Set "false" for "bin_tyep=1"

#export NL_WRITE_FILTERED_OBS=true.
#export NL_WRITE_INCREMENTS=.false.
export NL_USE_SSMIRETRIEVALOBS=true
#export NL_VAR_SCALING4=0.01  
#export NL_CALCULATE_CG_COST_FN=.true.
export NL_USE_GPSREFOBS=false
export NL_USE_AIRSRETOBS=false
export NL_LAT_STATS_OPTION=.false.
export NL_USE_QSCATOBS=false  # Set to "true" for cycling QSCAT   


# FGAT related settings:

# For 3DVAR (a 7-time-slot FGAT, -3h -2h -1h, analysis_time, +1h, +2h, +3h):

  #export NL_NUM_FGAT_TIME=7
  #export WINDOW_START=-3
  #export WINDOW_END=3

# For WRF provide input for FGAT 

   #export NL_INPUTOUT_INTERVAL=60
   #export NL_INPUTOUT_BEGIN_H=3
   #export NL_INPUTOUT_END_H=9

export SCRIPT=${SCRIPTS_DIR}/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

exit 0

