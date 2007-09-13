#!/bin/ksh

. ./setup.ksh

export REG_DIR=$PWD

export START_DATE=$INITIAL_DATE
export END_DATE=$FINAL_DATE

TUNING_DIR=$PWD/${MACHINE}_tuning
rm -rf $TUNING_DIR; mkdir -p $TUNING_DIR; cd $TUNING_DIR

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export WRFVAR_DIR=${WRFVAR_DIR:-~/code/${ID}/wrfvar}

export UNPERT_ID=${MACHINE}_${COMPILER}_opt
export PERT_ID=${MACHINE}_${COMPILER}_debug

export Y_DIR=$REG_DIR/${UNPERT_ID}_suite
export YP_DIR=$REG_DIR/${PERT_ID}_suite

export RUN_DIR=$TUNING_DIR/desroziers

$WRFVAR_DIR/scripts/da_tune_obs_desroziers.ksh

export EXP_DIR=$Y_DIR
export RUN_DIR=$TUNING_DIR/hollingsworth

$WRFVAR_DIR/scripts/da_tune_obs_hollingsworth.ksh
