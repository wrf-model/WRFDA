#!/bin/ksh

# da_test_verification.ksh

. da_test_defaults.ksh

. ./setup.ksh

export REG_DIR=$PWD

export EXP1=${1:-~/data/afwa_2.2/${ID}_quick_${NUM_PROCS}}
export EXP2=${2:-~/data/trunk/${ID}_quick_${NUM_PROCS}}

NAME1=$(basename $EXP1)
NAME2=$(basename $EXP2)

export NUM_EXPT=2
export EXP_DIRS="$EXP1 $EXP2"
export EXP_NAMES="$NAME1 $NAME2"
export EXP_LEGENDS='(/"stable","opt"/)'

export INTERVAL=12
export PLOT_WKS=pdf
export CLEAN=${CLEAN:-false}

export RUN_DIR=$PWD/${MACHINE}_verification

export START_DATE=$INITIAL_DATE
export END_DATE=$FINAL_DATE

$WRFVAR_DIR/scripts/da_verif_plot.ksh > $RUN_DIR/index.html 2>&1
