#!/bin/ksh
#########################################################################
# Script: da_run_wpb.ksh
#
# Purpose: Provide an ensemble of WRF lateral boundary condition (wrfbdy)
# files.
#
# Description:
# 1. Run WPS/real (produces wrfinput_d01 files).
# 2. Run WRF-Var in "randomcv" mode (produces ensemble of perturbed
#    wrfinput_d01 files.
# 3. Loop over 1. and 2. for each time of tendency in wrfbdy file out to
#    forecast length (e.g. 3hourly tendency update in a 72hr forecast).
# 4. Run perturb_wrf_bc to provide perturbed wrfbdy files.
#
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

export RUN_DIR=$EXP_DIR/run/$DATE/wpb}

#Save these, as they are overwritten locally:
export DATE_SAVE=$DATE
export END_DATE_SAVE=$END_DATE
export FCST_RANGE_SAVE=$FCST_RANGE
export NL_RUN_HOURS_SAVE=$NL_RUN_HOURS
export RC_DIR_SAVE=$RC_DIR
export NL_ANALYSIS_TYPE_SAVE=$NL_ANALYSIS_TYPE
export RUN_DIR_SAVE=$RUN_DIR

#These are the local values:
export END_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null`
export FCST_RANGE=0
export NL_RUN_HOURS=0
export RC_DIR=$RUN_DIR_SAVE/rc
export NL_ANALYSIS_TYPE="randomcv"
mkdir -p $RC_DIR

if test -f ${RC_DIR_SAVE}/geo_em.d01.nc; then
   cp ${RC_DIR_SAVE}/geo_em.d01.nc ${RC_DIR}
   export RUN_GEOGRID=false
fi

while test $DATE -le $END_DATE; do 
   echo "Producing wrfinput files for $DATE"

#  Run WPS:
   export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/wps/$DATE
   mkdir -p $RUN_DIR

#   $WRFVAR_DIR/scripts/da_trace.ksh da_run_wps $RUN_DIR >&! /dev/null
   ${SCRIPTS_DIR}/da_run_wps.ksh > $RUN_DIR/index.html 2>&1
   RC=$?
   if test $RC != 0; then
      echo `date` "${ERR}Failed with error $RC$END"
      exit 1
   fi
   export RUN_GEOGRID=false # Only need to run it once.

#  Run real:
   export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/real/$DATE
   mkdir -p $RUN_DIR

#   $WRFVAR_DIR/scripts/da_trace.ksh da_run_real $RUN_DIR >&! /dev/null
   ${SCRIPTS_DIR}/da_run_real.ksh > $RUN_DIR/index.html 2>&1
   RC=$?
   if test $RC != 0; then
      echo `date` "${ERR}Failed with error $RC$END"
      exit 1
   fi

   export NEXT_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null`
   export DATE=$NEXT_DATE

done
echo ""

export FCST_RANGE=$FCST_RANGE_SAVE
export NL_ANALYSIS_TYPE=randomcv
export NL_PUT_RAND_SEED=.TRUE.

#Overwrite originals:
export DATE=$DATE_SAVE
export RC_DIR=$RC_DIR_SAVE
export RUN_DIR=$RUN_DIR_SAVE

export MEM=1
export JOB=1

while test $MEM -le $NUM_MEMBERS; do 
   echo "Producing perturbed wrfbdy files for ensemble member $MEM"
   ${SCRIPTS_DIR}/da_perturb_wrf_bc.ksh > ${RUN_DIR}/da_perturb_wrf_bc.${MEM}.out 2>&1 &

   export MEM=`expr $MEM + 1`
   export JOB=`expr $JOB + 1`

   if test $JOB -gt $NUM_JOBS || test $MEM -gt $NUM_MEMBERS; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
   sleep 1
done

export END_DATE=$END_DATE_SAVE
export FCST_RANGE=$FCST_RANGE_SAVE
export NL_RUN_HOURS=$NL_RUN_HOURS_SAVE
export NL_ANALYSIS_TYPE=$NL_ANALYSIS_TYPE_SAVE
export RUN_DIR=$RUN_DIR_SAVE

exit 0

