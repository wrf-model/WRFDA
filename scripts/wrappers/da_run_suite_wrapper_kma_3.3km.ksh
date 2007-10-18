#!/bin/ksh
#########################################################################
# Script: da_run_suite_wrapper.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          specific to KMA Project setting for KMA's 3.3km domain.
#
#
#########################################################################

#Decide which stages to run (run if true):
#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_NDOWN=false
export RUN_WRF=true

#Experiment details:
export REGION=kma_3.33km
export RUN_UNGRIB_METGRID_KMA=true
#export FG_TYPE=GDAPS
export EXPT=noda.test
export CYCLE_PERIOD=12
#export CYCLING=true
#export FIRST=false
#export CLEAN=true

#Scheduling:
#export PROJECT_ID=48500053       # JNT GAUs (1200/month).
export PROJECT_ID=48503001       # DATC GAUs.
export QUEUE=regular  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC 
export NUM_PROCS=64   #64
export RUN_CMD=mpirun.lsf
export LSF_MAX_RUNTIME=240
export LSF_EXCLUSIVE=" "
export LL_PTILE=16
export JOBNAME=${EXPT}

#Time info:
export INITIAL_DATE=2007081000
export   FINAL_DATE=2007081012
export  LONG_FCST_TIME_1=00   
export LONG_FCST_RANGE_1=24  
export  LONG_FCST_TIME_2=03    
export LONG_FCST_RANGE_2=03    
export  LONG_FCST_TIME_3=06    
export LONG_FCST_RANGE_3=03    
export  LONG_FCST_TIME_4=09     
export LONG_FCST_RANGE_4=03    
export  LONG_FCST_TIME_5=12    
export LONG_FCST_RANGE_5=24  
export  LONG_FCST_TIME_6=15    
export LONG_FCST_RANGE_6=03    
export  LONG_FCST_TIME_7=18     
export LONG_FCST_RANGE_7=03    
export  LONG_FCST_TIME_8=21     
export LONG_FCST_RANGE_8=03     

export LBC_FREQ=12 # For 00/12Z KMA runs
#export LBC_FREQ=6  # For 06/18Z KMA runs

#Directories:
export MACHINE=bluevista
export REL_DIR=/rap/datc/demirtas/code/blueice
export DAT_DIR=/ptmp/demirtas/data
export GRIB_DIR=$DAT_DIR/$FG_TYPE
export WPS_GEOG_DIR=/mmm/users/wrfhelp/WPS_GEOG   # bluevista 

export OBSPROC_DIR=$REL_DIR/3DVAR_OBSPROC         # Which version?
export WRFVAR_DIR=$REL_DIR/wrfvar_r2522           # r2522.
export WPS_DIR=$REL_DIR/wps_r237                  # r237 AFWA release.
export WRF_BC_DIR=$REL_DIR/WRF_BC                 # Which version?
export WRF_DIR=$REL_DIR/wrf_r2234_dfi_ndown      #wrf_r2234                 # 
export REG_DIR=$DAT_DIR/$REGION
export EXP_DIR=$REG_DIR/$EXPT
export OB_DIR=$REG_DIR/ob
export SCRIPTS_DIR=/blhome/demirtas/code/scripts_wrfvar_r2522

#ndown related specific settings.
export WRFOUT_DIR=$DAT_DIR/kma_10km/cycling/fc
export FRANGE=3  # KMA_3.3km specific
export WRFOUT_INTERVAL=1

#WPS (namelist.wps):

export RUN_GEOGRID=false
export NL_E_WE=442
export NL_E_SN=388
export REF_LAT=38.00
export REF_LON=126.0
export STAND_LON=126.0
export NL_DX=3333.333
export NL_DY=$NL_DX

#WRF real (not already covered above):
export NL_HISTORY_INTERVAL=720  #30
export NL_TIME_STEP=20
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.993, 0.980, 0.966, 0.950, "\
                                      " 0.933, 0.913, 0.892, 0.869, 0.844, "\
                                      " 0.816, 0.786, 0.753, 0.718, 0.680, "\
                                      " 0.639, 0.596, 0.550, 0.501, 0.451, "\
                                      " 0.398, 0.345, 0.290, 0.236, 0.188, "\
                                      " 0.145, 0.108, 0.075, 0.046, 0.021, "\
                                      " 0.000 "}
export NL_E_VERT=31
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=6
export NL_RADT=10
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_CUDT=0
export NL_MP_ZERO_OUT=0
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0
export NL_CU_PHYSICS=0

#WRF (not already covered above):

#WRF (not already covered above):
export NL_NUM_METGRID_LEVELS=27    #24 used for kma_10km domain but does not work for GFS based 3.3km domain.
export NL_INPUTOUT_BEGIN_H=6          # Output input format start.
#export NL_INPUTOUT_END_H=6           # Output input format end.#
export NL_FORCE_SFC_IN_VINTERP=6  

#export NL_ADJUST_HEIGHTS=true   # Not in 2234?
#export NL_NIO_TASKS_PER_GROUP=0 # Not in 2234?
#export NL_NIO_GROUPS=1          # Not in 2234?

#OBSPROC (not covered above):

#WRF-Var:

export NL_USE_SSMIRETRIEVALOBS=false
export NL_CHECK_RH=1
export NL_INTERPOLATE_STATS=false
export NL_LEN_SCALING1=0.5
export NL_LEN_SCALING2=0.5
export NL_LEN_SCALING3=0.5
export NL_LEN_SCALING4=0.5
export NL_LEN_SCALING5=0.5


export SCRIPT=$SCRIPTS_DIR/da_run_suite.ksh
export SCHEDULER=lsf
$WRFVAR_DIR/scripts/da_run_job.${SCHEDULER}.ksh

echo $INITIAL_DATE $FINAL_DATE  >> log4submittedjobs.txt

exit 0

