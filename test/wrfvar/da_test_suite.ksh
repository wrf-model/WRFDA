# Define for your run

. ./setup.ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export EXPT=${ID}_suite_${NUM_PROCS}

export REL_DIR=$HOME/code/$ID

export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export NL_NTMAX=${NL_NTMAX:-2}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR/run

export SUBMIT_FLAGS=$SUBMIT_WAIT_FLAG

export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

if $RUN_WRFVAR; then
   mkdir -p $EXP_DIR/plotobs
   ncl $WRFVAR_DIR/graphics/ncl/plotobs.ncl > $EXP_DIR/plotobs/plotobs.log 2>&1
fi

exit 0
