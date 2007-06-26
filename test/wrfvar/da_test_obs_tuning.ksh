#!/bin/ksh

. ./region.ksh

export REG_DIR=$PWD

TUNING_DIR=${MACHINE}_tuning
rm -rf $TUNING_DIR; mkdir -p $TUNING_DIR; cd $TUNING_DIR

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export WRFVAR_DIR=${WRFVAR_DIR:-~/code/${ID}/wrfvar}

export UNPERT_ID=${MACHINE}_${COMPILER}_opt
export PERT_ID=${MACHINE}_${COMPILER}_debug

export Y_DIR=$REG_DIR/${UNPERT_ID}_suite_${NUM_PROCS}
export YP_DIR=$REG_DIR/${PERT_ID}_suite_${NUM_PROCS}

$WRFVAR_DIR/scripts/da_tune_obs_desroziers.ksh

export EXPT=${UNPERT_ID}_suite_${NUM_PROCS}

$WRFVAR_DIR/scripts/da_tune_obs_hollingsworth.ksh
