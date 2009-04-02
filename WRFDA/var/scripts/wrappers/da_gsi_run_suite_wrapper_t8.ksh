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
set echo

#Decide which stages to run (run if true):
#export RUN_RESTORE_DATA_GRIB=true
#export RUN_RESTORE_DATA_RTOBS=true
export RUN_WPS=true
export RUN_REAL=true
#export RUN_OBSPROC=true
#export RUN_ENS_EP=true
#export RUN_WRFVAR=true
export RUN_GSI=true
#export NUM_MEMBERS=10
export RUN_UPDATE_BC=true
export RUN_WRF=true
#export RUN_ENSMEAN=true

#Experiment details:
export REGION=t8
export EXPT=test
#export CLEAN=true
#export CYCLING=true
export CYCLE_PERIOD=06

#Time info
export INITIAL_DATE=2007081500
export FINAL_DATE=2007081600
export FCST_RANGE=12
export LONG_FCST_TIME_1=00
export LONG_FCST_RANGE_1=12
export LONG_FCST_TIME_2=06
export LONG_FCST_RANGE_2=12
export LONG_FCST_TIME_3=12
export LONG_FCST_RANGE_3=12
export LONG_FCST_TIME_4=18
export LONG_FCST_RANGE_4=12

#Directories:
export        REL_DIR=/ptmp/liuz
export        WPS_DIR=$REL_DIR/WRF3.1/trunk_wps
export        WRF_DIR=$REL_DIR/WRF3.1/trunk_wrf
export        WRFVAR_DIR=$REL_DIR/WRF3.1/trunk_wrfvar
export        GSI_DIR=/ptmp/mhu/test_Q1FY09
export        SCRIPTS_DIR=/ptmp/lir/test/wrf_var/wrapper/WRFDA/var/scripts

export        RUN_DIR=/ptmp/lir/test/gsi_run
export        EXP_DIR=$RUN_DIR
export        DAT_DIR=/ptmp/lir/test/data/t8
export        RC_DIR=$DAT_DIR/rc
export        FC_DIR=$DAT_DIR/fc
export        OB_DIR=/ptmp/liuz/t8_2008_45km/ob
export        GRIB_DIR=$DAT_DIR/gfs

#From WPS (namelist.wps):
export RUN_GEOGRID=true
export NL_E_WE=418
export NL_E_SN=280
export MAP_PROJ=mercator
export REF_LAT=20.0
export REF_LON=-75.0
export TRUELAT1=20.0
export TRUELAT2=20.0
export STAND_LON=-75.00
export NL_DX=15000
export NL_DY=15000
export GEOG_DATA_RES=30s
export TRUELAT1=20.0
export TRUELAT2=20.0

#WRF:
export NL_HISTORY_INTERVAL=360
export NL_TIME_STEP=90  # 15 mins should be plenty for 240km resolution.
#export NL_HISTORY_INTERVAL=360 # wrfout files every 6 hours.
export NL_USE_ADAPTIVE_TIME_STEP=false
export NL_E_VERT=57
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.00000, 0.99730, 0.99157, 0.98506, 0.97772, "\
                                      " 0.96943, 0.96010, 0.94962, 0.93789, 0.92478, "\
                                      " 0.91020, 0.89403, 0.87618, 0.85657, 0.83514, "\
                                      " 0.81183, 0.78664, 0.75960, 0.73078, 0.70027, "\
                                      " 0.66825, 0.63491, 0.60048, 0.56527, 0.52957, "\
                                      " 0.49370, 0.45802, 0.42281, 0.38842, 0.35510, "\
                                      " 0.32311, 0.29265, 0.26386, 0.23686, 0.21172, "\
                                      " 0.18845, 0.16706, 0.14749, 0.12968, 0.11355, "\
                                      " 0.09901, 0.08593, 0.07423, 0.06378, 0.05447, "\
                                      " 0.04621, 0.03888, 0.03240, 0.02668, 0.02163, "\
                                      " 0.01718, 0.01328, 0.00984, 0.00682, 0.00418, "\
                                      " 0.00186, 0.00000 "}
export NL_P_TOP_REQUESTED=1000
export NL_FEEDBACK=0
export NL_SMOOTH_OPTION=0
export NL_FORCE_SFC_IN_VINTERP=3
export NL_ADJUST_HEIGHTS=false
export NL_MP_PHYSICS=4
export NL_RADT=30
export NL_SF_SFCLAY_PHYSICS=1
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_BL_PBL_PHYSICS=1
export NL_MP_ZERO_OUT=0
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_KM_OPT=4
export NL_DAMPCOEF=0.01
export NL_TIME_STEP_SOUND=0
export NL_PD_MOIST=TRUE


#GSI

# submit job info
export NUM_PROCS=8
export QUEUE=premium
export WALLCLOCK=360
export PROJECT=64000510

#Continuous job

export SCRIPT=$SCRIPTS_DIR/da_run_suite.ksh
$SCRIPTS_DIR/da_run_job.ksh


