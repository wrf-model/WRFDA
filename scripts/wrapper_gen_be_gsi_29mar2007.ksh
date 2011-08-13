#! /bin/ksh -aeux
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Author : Syed RH Rizvi, MMM/ESSL/NCAR,  Date:04/15/2009
#
# Purpose: Calculates WRF-ARW background error statistics for GSI
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:

export DAT_DIR=/ptmp/yann
export REGION=data
export EXPT=dowell
export REG_DIR=$DAT_DIR/$REGION/$EXPT

export FC_DIR=$REG_DIR
export RUN_DIR=$REG_DIR
export EXP_DIR=$REG_DIR/holm_gsibe_mar
export WORK_DIR=$EXP_DIR/working

export GEN_BE_DIR=/blhome/yann/stand_alone_holm_be
export WRFVAR_DIR=$GEN_BE_DIR
export SCRIPTS_DIR=$GEN_BE_DIR/scripts
export BUILD_DIR=$GEN_BE_DIR/src

export STAGE0_DIR=$WORK_DIR/stage0
export STAGE1_DIR=$WORK_DIR/stage1
export STAGE2_DIR=$WORK_DIR/stage2

export NUM_PROCS=1
export WALL_CLOCK=30
export PROJECT=64000510
export QUEUE=debug

export CLEAN=false   

export LESS_Q_FROM_TOP=0      # Exclude levels from top for moisture statistics
export LAT_BINS_IN_DEG=5.0    # Lat bins (in deg) for BE stats
export DEBUG=0      

export START_DATE=2007032903
export END_DATE=2007032903
export NUM_LEVELS=34
export NUM_WE=350
export NUM_SN=450

export BE_METHOD=ENS
export NE=30
export FCST_RANGE=12
 
export DOMAIN=01
export FCST_RANGE1=0
export FCST_RANGE2=12
export INTERVAL=2
export STRIDE=1

export RUN_GEN_BE_GSI_STAGE0=false
export RUN_GEN_BE_GSI_STAGE1=false
export RUN_GEN_BE_HOLM_VARIANCE=true
export RUN_GEN_BE_GSI_STAGE2=true

export INTERVAL=12

#-----------------------------------------------------------------------

# Run GSI version of GEN_BE

${GEN_BE_DIR}/scripts/gen_be_gsi.ksh

#-----------------------------------------------------------------------
