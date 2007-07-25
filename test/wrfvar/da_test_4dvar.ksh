#!/bin/ksh

export WINDOW_START=0
export WINDOW_END=6
export NL_RUN_HOURS=6
export NL_VAR4D=true
export NL_INPUTOUT_BEGIN_H=0
export NL_NUM_FGAT_TIME=`expr $WINDOW_END \+ 1`
export FIRST=true

#WRF:
export NL_TIME_STEP=600
export NL_SMOOTH_OPTION=0
export NL_MP_PHYSICS=4
export NL_SF_SURFACE_PHYSICS=2
export NL_NUM_SOIL_LAYERS=4
export NL_CU_PHYSICS=3
export NL_MP_ZERO_OUT=2
export NL_W_DAMPING=1
export NL_DIFF_OPT=1
export NL_DAMPCOEF=0.01

#JCDF Option & Obs
export NL_JCDFI_USE=false
export NL_JCDFI_TAUC=`expr $WINDOW_END \* 3600`
export NL_JCDFI_GAMA=0.1
export NL_JCDFI_ERROR_WIND=3.0
export NL_JCDFI_ERROR_T=1.0
export NL_JCDFI_ERROR_Q=0.001
export NL_JCDFI_ERROR_MU=1000.0

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}

export EXPT=${EXPT:-${ID}_${NUM_PROCS}}

export REL_DIR=$HOME/code/$ID

export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}
export WRFNL_DIR=${WRFNL_DIR:-$REL_DIR/wrfnl}

export CLEAN=${CLEAN:-false}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR
rm -rf $RUN_DIR

$WRFVAR_DIR/scripts/da_run_job.ksh
