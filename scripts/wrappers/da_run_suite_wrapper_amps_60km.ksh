#!/bin/ksh 
#########################################################################
# Script: da_run_suite_wrapper_amps1.ksh
#
# Purpose: Provide user-modifiable interface to da_run_suite.ksh script
#          for AMPS domain 1 (60km).

# Description:
#
# Here are a few examples of environment variables you may want to 
# change in da_run_suite_wrapper.ksh:
#
# 1) "export RUN_WRFVAR=true" runs WRFVAR.
# 2) "export REL_DIR=$HOME/trunk" points to directory
# containing all code (I always include all components as subdirectories
# e.g. $REL_DIR/wrfvar contains the WRFVAR code.
# 3) "export INITIAL_DATE=2003010100" begins the experiment at 00 UTC
# 1 January 2003.
#
# You will see the full list of environment variables, and their default
# values in da_run_suite.ksh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
# 
#########################################################################
set echo 

#export RUN_WPS=true
#export RUN_REAL=true
#export RUN_OBSPROC=true
export RUN_WRF=true
export RUN_WRFVAR=true
export RUN_UPDATE_BC=true
#export CYCLING=true
#export FIRST=true

export EXPT=test
export REGION=amps1

export NUM_PROCS=32                                    #Number of processors for WRF-Var/WRF.
export QUEUE=premium                                     #premium, regular, share
export LSF_MAX_RUNTIME=45                             #premium/regular max=360, share max=180

export INITIAL_DATE=2006100100
export   FINAL_DATE=2006100100
export CYCLE_PERIOD=06

export NL_USE_GPSREFOBS=.true.

export CONTJOB=n

###############################################################
# DEFAULT CONFIGURATIONS and NAMELIST. SHOULD NOT BE MODIFIED #
###############################################################
#Decide which stages to run (run if true):
export RUN_RESTORE_DATA_GRIB=${RUN_RESTORE_DATA_GRIB:-false}
export RUN_RESTORE_DATA_RTOBS=${RUN_RESTORE_DATA_RTOBS:-false}
export RUN_WPS=${RUN_WPS:-false}
export RUN_REAL=${RUN_REAL:-false}
export RUN_OBSPROC=${RUN_OBSPROC:-false}
export RUN_WRFVAR=${RUN_WRFVAR:-false}
export RUN_UPDATE_BC=${RUN_UPDATE_BC:-false}
export RUN_WRF=${RUN_WRF:-false}

#Experiment details:
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-amps1}
export EXPT=${EXPT:-test}                              # Experiment name.
export CLEAN=${CLEAN:-false}
export CYCLING=${CYCLING:-false}                       # Cold start (false), cycle (true).
export FIRST=${FIRST:-false}         

export PROJECT_ID=${PROJECT_ID:-25000026}              #JNT: 68000001, MMM: 64000420
export NUM_PROCS=${NUM_PROCS:-32}                       # Number of processors for WRF-Var/WRF.
export QUEUE=${QUEUE:-regular}                                   
export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-360}         #premimum max=360, regular max=180
export LSF_EXCLUSIVE=" "
export LL_PTILE=${LL_PTILE:-16}

#Time info:
export INITIAL_DATE=${INITIAL_DATE:-2006100100}
export   FINAL_DATE=${FINAL_DATE:-2006102818}
export CYCLE_PERIOD=${CYCLE_PERIOD:-12}                # Assimilation frequency.
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=72
export LONG_FCST_TIME_2=12
export LONG_FCST_RANGE_2=72

#Directories:
export     MACHINE=${MACHINE:-bluevista}
export     REL_DIR=${REL_DIR:-/rap/datc/code}
export  WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar_r2552}
export     WRF_DIR=${WRF_DIR:-$REL_DIR/wrf_r2234}
export     WPS_DIR=${WPS_DIR:-$REL_DIR/wps_r237}
export  WRF_BC_DIR=${WRF_BC_DIR:-$REL_DIR/WRF_BC}
export OBSPROC_DIR=${OBSPROC_DIR:-$REL_DIR/3DVAR_OBSPROC}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
export     DAT_DIR=${DAT_DIR:-/ptmp/$USER/data}
export     REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export     EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export   RTOBS_DIR=${RTOBS_DIR:-$REG_DIR/rtobs}
export      OB_DIR=${OB_DIR:-$REG_DIR/ob}
export    GRIB_DIR=/rap/datc/data/fnl
export WPS_GEOG_DIR=/mmm/users/wrfhelp/WPS_GEOG

#WPS (namelist.wps):
export RUN_GEOGRID=${RUN_GEOGRID:-false}  #true for first time run of wps

#both WPS and OBSPROC 
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

#OBSPROC
export MAX_NUMBER_OF_OBS=140000
export THINING_QSCAT=.TRUE.
export THINING_SATOB=.FALSE.
export  THINING_SSMI=.FALSE.

#OBSPROC and WRF
export NL_P_TOP_REQUESTED=5000.   
export NL_BASE_TEMP=268.

#WRF:
export NL_TIME_STEP=240
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.993, 0.980, 0.966, 0.950, "\ 
                                      " 0.933, 0.913, 0.892, 0.869, 0.844, "\
                                      " 0.816, 0.786, 0.753, 0.718, 0.680, "\
                                      " 0.639, 0.596, 0.550, 0.501, 0.451, "\
                                      " 0.398, 0.345, 0.290, 0.236, 0.188, "\
                                      " 0.145, 0.108, 0.075, 0.046, 0.021, 0.000 "}
export NL_E_VERT=31
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_RADT=10
export NL_SF_SFCLAY_PHYSICS=1
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_BL_PBL_PHYSICS=1
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=4
export NL_MP_ZERO_OUT=0

#WRF-Var:
##from opertional AMPS1 wrfvar2.1 namelist
export NL_WRITE_INCREMENTS=${NL_WRITE_INCREMENTS:-.false.}     # .true. in Kevin's namelist
export NL_USE_SSMIRETRIEVALOBS=${NL_USE_SSMIRETRIEVALOBS:-.false.} # .true. in Kevin's namelist
export NL_USE_GPSREFOBS=${NL_USE_GPSREFOBS:-.false.}
export NL_USE_AIRSRETOBS=${NL_USE_AIRSRETOBS:-.false.}
export NL_VAR_SCALING4=${NL_VAR_SCALING4:-0.01}            # for be.dat procuded by wrfvar2.1
export NL_CALCULATE_CG_COST_FN=.true.
export NL_CV_OPTIONS_HUM=1
export NL_LAT_STATS_OPTION=.false.
export NL_ANALYSIS_TYPE=QC-OBS
export NL_CHECK_MAX_IV=.true.

#Continuous job 
export CONTJOB=${CONTJOB:-n} #Y/N or y/n

#Final confirmatioin
echo "REGION = $REGION & EXPT = $EXPT "
echo "Date = ${INITIAL_DATE} - ${FINAL_DATE} "
echo "CONTJOB = $CONTJOB"

#Run suite
export SCRIPT=${SCRIPTS_DIR}/da_run_suite.ksh

if [ "$CONTJOB" = "Y" ] || [ "$CONTJOB" = "y" ] ; then
  read string1 jobid string3 < bsubjob_${EXPT}.log
  let joblen=${#jobid}-1
  export PREV_JOBID=`print $jobid | cut -c 2-${joblen}` #remove < and > from string2
  echo "PREV_JOBID = ${PREV_JOBID} "

  ${SCRIPTS_DIR}/da_run_job.${MACHINE}-cont.ksh

  read string1 current_jobid string3 < bsubjob_${EXPT}.log
  echo "job ${current_jobid}(depd on ${PREV_JOBID}) submitted for ${REGION}/${EXPT} during ${INITIAL_DATE}-${FINAL_DATE}" >> currentjobs.list
else
  ${SCRIPTS_DIR}/da_run_job.${MACHINE}.ksh
  read string1 current_jobid string3 < bsubjob_${EXPT}.log
  echo "job ${current_jobid} submitted for ${REGION}/${EXPT} during ${INITIAL_DATE}-${FINAL_DATE}" >> currentjobs.list
fi

exit 0
