# Define for your run

. ./setup.ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export EXPT=${ID}_suite
export RUN=run

export REL_DIR=$HOME/code/$ID

export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export NL_NTMAX=${NL_NTMAX:-2}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT

export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh
