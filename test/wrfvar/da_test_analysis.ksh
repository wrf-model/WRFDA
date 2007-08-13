#!/bin/ksh

export EXPT=vartest
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export RUN=${RUN:-${ID}_${NUM_PROCS}}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR/$RUN
rm -rf $RUN_DIR

. ./setup.ksh
. $EXP_DIR/setup.ksh

export SCRIPT=$WRFVAR_DIR/scripts/da_run_wrfvar.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh
