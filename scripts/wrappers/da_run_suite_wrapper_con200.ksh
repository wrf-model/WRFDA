#!/bin/ksh
#-------------------------------------------------
# following command is for BIG to execute mpich2
#mpd &
#mpdboot -n 9 -f $HOME/hosts --ifhn=master

#-------------------------------------------------

export PROJECT=64000510     # DATC GAUs.
export WALLCLOCK=10

export QUEUE=debug  # use "share" queue for:WPS, REAL, UPDATE_BC and OBS_PROC
export NUM_PROCS=1  #64 is for WRF and WRF-VAR # 1 is for WPS, REAL, UPDATE_BC and OBS_PROC

# Define for your run

export REL_DIR=/big/users/rizvi
export WRFVAR_DIR=$REL_DIR/WRFV3_20080319
export DAT_DIR=/big5/wrfhelp/data_working/trunk

# Fiddle with options here

export EXPT=run_cpu$NUM_PROCS
export RUN_DIR=/big6/rizvi/con200/${EXPT}
export DATE=2007010200

export SUBMIT=none


export INITIAL_DATE=2007010200    
export   FINAL_DATE=2007010200    
export DATE=$INITIAL_DATE         

export REGION=con200
export REG_DIR=$DAT_DIR/$REGION

export OB_DIR=${REG_DIR}/jan07/ob
export DA_BACK_ERRORS=${REG_DIR}/be/be.cv_5
export RC_DIR=${REG_DIR}/jan07/rc
export FC_DIR=/big6/rizvi/${REGION}/fc

export NL_E_WE=45
export NL_E_SN=45
export NL_DX=200000
export NL_DY=200000
export NL_E_VERT=28

#export HOSTS=$HOME/hosts
#export HOSTS=`eval echo $HOSTS`

export RUN_WRFVAR=true            

export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh

##----------------------------------------------
##   For Single Obs test  starts
#export runplot_psot=1  # 1 -run psot  2- plot psot
#export RUN_DIR=/big6/rizvi/$REGION/psot      
##----------------------------------------------
## 
##----------------------------------------------
#export NL_NUM_PSEUDO=1
#export NL_PSEUDO_VAR="u"         #  Can be    "u   u    t    t      q"
#export NL_PSEUDO_X=23.0
#export NL_PSEUDO_Y=23.0
#export NL_PSEUDO_Z=14.0
#export NL_PSEUDO_ERR=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
#export NL_PSEUDO_VAL=1.0       #  Should be "1.0 1.0 1.0  1.0  0.001"
##
#if (( $runplot_psot == 1 ));then
#export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_suite.ksh
#else
#export SCRIPT=$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
#fi
#----------------------------------------------
#   For Single Obs test  ends
#----------------------------------------------
#
$WRFVAR_DIR/var/scripts/da_run_job.ksh

exit 0


$WRFVAR_DIR/var/scripts/da_run_job.ksh

