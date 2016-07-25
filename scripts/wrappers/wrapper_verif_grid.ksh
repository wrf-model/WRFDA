#!/bin/ksh -aeux 
#============================================================;
# Purpose:  wrapper for running 
#           $WRFVAR_DIR/scripts/da_verif_grid.ksh
#============================================================;
# Author: Syed RH Rizvi          MMM/NCAR
# Date  : 10/09/2007
#
#============================================================;

export RUN_VERIF_ANAL_STATS=false
export RUN_VERIF_ANAL_PLOTS=true 

#export NUM3D=3
#export VAR3D="Z   T  QVAPOR"
#export VAR3D="Z  U  T  QVAPOR"
#
#export NUM2D=1
#export VAR2D="SLP PSFC U10M V10M T2M Q2M"
#export VAR2D="SLP"


export VERIFY_HOUR=00
#WRF-Var Directory:
export WRFVAR_DIR=/ptmp/rizvi/trunk

export REG_DIR=/ptmp/rizvi/data/ampsrt
export NUM_EXPT=2
export EXP_DIRS="/ptmp/auligne/ampsrt/CTRL /ptmp/auligne/ampsrt/AIRSRET3"

#export EXP_DIRS="/ptmp/auligne/ampsrt/AIRSRET3"

export TOP_HPA_LEVEL_FOR_VERT_PROFILES=50.

export EXP_NAMES='ctrl  airs2'
export CONTROL_EXP_DIR=/ptmp/rizvi/data/ampsrt/gfs_anal
export VERIFICATION_FILE_STRING='wrfout'
export VERIFY_ITS_OWN_ANALYSIS=false

export EXP_LEGENDS='(/"CTRL" , "AIRSRET"/)'
export DESIRED_LEVELS='(/"850","500","200"/)'
export DESIRED_SCORES='(/"RMSE","BIAS", "ABIAS"/)'
export EXP_LINES_COLORS='(/"blue","green", "orange"/)'
export START_DATE=2008120112
export   END_DATE=2008122000

export RUN_DIR=$REG_DIR/verify_gfs_anal_${VERIFY_HOUR}Hr     
export INTERVAL=12
export Verify_Date_Range="01 - 21 Dec 2008 (12-Hourly Cycle)"

export PLOT_WKS=pdf
export CLEAN=true 

mkdir -p $RUN_DIR
export SCRIPTS_DIR=$WRFVAR_DIR/var/scripts
$WRFVAR_DIR/var/scripts/da_verif_grid.ksh
exit

