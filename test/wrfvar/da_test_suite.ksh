#!/bin/ksh

# da_test_suite.ksh

if [[ $# != 1 ]]; then
   echo "Arguments test"
   echo "(one of wrfvar wrfvar_tests wrfvar_verbose ideal wrf wrf_real wrf_ideal quick)"
   exit 1
fi

TEST=$1

. ./setup.ksh
. $CASE/setup.ksh

export REG_DIR=$PWD
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export RUN=${RUN:-run}
export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

if [[ $TYPE == serial ]]; then
   export NUM_PROCS=1
fi

export EXPT=${EXPT:-${ID}_${TEST}_${NUM_PROCS}}
export EXP_DIR=$PWD/$EXPT

if [[ $TEST == wrfvar ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
elif [[ $TEST == wrfvar_tests ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
   export NL_TEST_WRFVAR=true
   export NL_TEST_TRANSFORMS=true
   export NL_TEST_STATISTICS=true
elif [[ $TEST == wrfvar_verbose ]]; then
   export INITIAL_DATE=$DATE
   export FINAL_DATE=$DATE
   export RUN_WRFVAR=true
   export RC_DIR=$PWD/$CASE/rc
   export OB_DIR=$PWD/$CASE/ob
   export NL_NTMAX=${NL_NTMAX:-2}
   export NL_TRACE_USE_DULL=true
   export NL_TRACE_USE_FREQUENT=true
   export NL_TRACE_ALL_PES=true
   export NL_PRINT_DETAIL_AIREP=true
   export NL_PRINT_DETAIL_RADAR=true
   export NL_PRINT_DETAIL_RAD=true
   export NL_PRINT_DETAIL_XA=true
   export NL_PRINT_DETAIL_XB=true
   export NL_PRINT_DETAIL_OBS=false
   export NL_PRINT_DETAIL_F_OBS=true
   export NL_PRINT_DETAIL_MAP=true
   export NL_PRINT_DETAIL_RAD=true
   export NL_PRINT_DETAIL_GRAD=true
   export NL_PRINT_DETAIL_REGRESSION=true
   export NL_PRINT_DETAIL_SPECTRAL=true
   export NL_PRINT_DETAIL_TESTING=true
   export NL_PRINT_DETAIL_PARALLEL=true
   export NL_PRINT_DETAIL_BE=true
   export NL_PRINT_DETAIL_TIMING=true
   export NL_WRITE_QCW=true
   export NL_WRITE_QRN=true
   export NL_WRITE_QCI=true
   export NL_WRITE_QSN=true
   export NL_WRITE_QGR=true
   export NL_WRITE_FILTERED_OBS=true
   export NL_WRITE_FILTERED_RAD=true
   export NL_WRITE_INCREMENTS=true
   export NL_WRITE_IV_RAD_ASCII=true
   export NL_WRITE_OA_RAD_ASCII=true
   export NL_BIASPREP=true
   # Changes the results, so avoid
   #export NL_OMB_ADD_NOISE=true
   # stop hitting bug at line 247 of da_write_iv_rad_ascii where rttov coefficient
   # used in crtm run. Zhiquan has fix in local code.
   export NL_WRITE_PROFILE=false
elif [[ $TEST == wrf ]]; then
   export RUN_WRF=${RUN_WRF:-true}
elif [[ $TEST == wrf_ideal ]]; then
   export RUN_WRF=${RUN_WRF:-true}
   export RUN_IDEAL=${RUN_IDEAL:-true}
elif [[ $TEST == wrf_real ]]; then
   export RUN_WRF=${RUN_WRF:-true}
   export RUN_REAL=${RUN_REAL:-true}
elif [[ $TEST == quick ]]; then
   export RUN_WPS=${RUN_WPS:-true}
   export RUN_REAL=${RUN_REAL:-true}
   export RUN_OBSPROC=${RUN_OBSPROC:-true}
   export RUN_WRFVAR=${RUN_WRFVAR:-true}
   export RUN_UPDATE_BC=${RUN_UPDATE_BC:-true}
   export RUN_WRF=${RUN_WRF:-true}
   export NL_NTMAX=${NL_NTMAX:-2}
else
   echo "Unknown test $TEST"
   exit 1
fi

export SCRIPT=$WRFVAR_DIR/scripts/da_run_suite.ksh
$WRFVAR_DIR/scripts/da_run_job.ksh

