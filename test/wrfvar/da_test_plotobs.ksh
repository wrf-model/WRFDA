#!/bin/ksh

# da_test_plotobs.ksh

. ./setup.ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export EXPT=${ID}

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$PWD/$EXPT/run

mkdir -p $EXP_DIR/plotobs
ncl $WRFVAR_DIR/graphics/ncl/plotobs.ncl > $EXP_DIR/plotobs/plotobs.log 2>&1
