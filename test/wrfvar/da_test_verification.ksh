#!/bin/ksh

. ./setup.ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export WRFVAR_DIR=${WRFVAR_DIR:-$HOME/code/$ID/wrfvar}

export REG_DIR=$PWD

export NUM_EXPT=2
export EXP_NAMES="${MACHINE}_${COMPILER}_stable_quick_${NUM_PROCS) ${MACHINE}_${COMPILER}_opt_quick_${NUM_PROCS)"
export EXP_LEGENDS='(/"stable","opt"/)'

export INTERVAL=12
export PLOT_WKS=pdf
export CLEAN=${CLEAN:-false}

export RUN_DIR=$PWD/${MACHINE}_verification

export START_DATE=$INITIAL_DATE
export END_DATE=$FINAL_DATE

$WRFVAR_DIR/scripts/da_verif_plot.ksh > $RUN_DIR/index.html 2>&1
