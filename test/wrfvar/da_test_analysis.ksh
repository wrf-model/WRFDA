#!/bin/ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}

export EXPT=${EXPT:-${ID}_${NUM_PROCS}}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}

export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export CLEAN=${CLEAN:-false}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR
rm -rf $RUN_DIR

export SCRIPT=$WRFVAR_DIR/scripts/da_run_wrfvar.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh