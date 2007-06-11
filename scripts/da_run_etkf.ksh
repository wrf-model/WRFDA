#!/bin/ksh
#########################################################################
# Script: da_run_etkf.ksh
#
# Purpose: To perform an Ensemble Transform Kalman Filter (ETKF)
# rescaling of ensemble forecast perturbations.
# The ensemble mean is the WRF-Var analysis.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
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

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

export ETKF_DIR=$FC_DIR/etkf
export RUN_DIR=$EXP_DIR/run/${DATE}/run_etkf

export NL_CHECK_RH=0
export NL_CHECK_MAX_IV=.false.
export NL_ANALYSIS_TYPE="VERIFY"
export DA_ANALYSIS=analysis_not_used

#------------------------------------------------------------------------------------------

mkdir -p $ETKF_DIR
mkdir -p $RUN_DIR
export RUN_DIR_SAVE=$RUN_DIR

export PREV_DATE=`${BUILD_DIR}/da_advance_cymdh.exe $DATE -$FCST_RANGE`
export YYYY=`echo $DATE | cut -c1-4`
export MM=`echo $DATE | cut -c5-6`
export DD=`echo $DATE | cut -c7-8`
export HH=`echo $DATE | cut -c9-10`
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
export DA_FILE=${FC_DIR}/${PREV_DATE}/wrf_3dvar_input_d01_${FILE_DATE}

export MEM=1
export JOB=1
while test $MEM -le $NUM_MEMBERS; do
   echo "   Producing observation file (yo, H(xb), sigma_o) for member $MEM"

   export CMEM=e$MEM
   if test $MEM -lt 100; then export CMEM=e0$MEM; fi
   if test $MEM -lt 10; then export CMEM=e00$MEM; fi
   export DA_FIRST_GUESS=${DA_FILE}.${CMEM}

   export RUN_DIR=${RUN_DIR_SAVE}/wrfvar.${CMEM}
   mkdir -p $RUN_DIR; cd $RUN_DIR
   ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > da_run_wrfvar.${CMEM}.out 2>&1 &

   export MEM=`expr $MEM + 1`
   export JOB=`expr $JOB + 1`

   if test $JOB -gt $NUM_JOBS || test $MEM -gt $NUM_MEMBERS; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
done

#Prepare ETKF input files:

export RUN_DIR=$RUN_DIR_SAVE
cd $RUN_DIR
cp ${DA_FILE} wrfinput_d01 # Prior ensemble mean.

export MEM=1
while test $MEM -le $NUM_MEMBERS; do
   export CMEM=e$MEM
   if test $MEM -lt 100; then export CMEM=e0$MEM; fi
   if test $MEM -lt 10; then export CMEM=e00$MEM; fi

   cp ${DA_FILE}.${CMEM} wrfinput_d01.${CMEM} # ETKF input.
   cp ${DA_FILE}.${CMEM} analysis.${CMEM} # ETKF input.
###   cp ${FC_DIR}/${DATE}/analysis analysis.${CMEM}     # ETKF output (overwrite).

   wc -l wrfvar.${CMEM}/working/ob.etkf.000 > ob.etkf.${CMEM}
   cat wrfvar.${CMEM}/working/ob.etkf.000 >> ob.etkf.${CMEM}

   export MEM=`expr $MEM + 1`
done

#Run Ensemble Transform Kalman Filter:

export DA_FILE=wrfinput_d01

cat > gen_be_etkf_nl.nl << EOF
  &gen_be_etkf_nl
    directory = '${ETKF_DIR}',
    filestub = '${DA_FILE}',
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV},
    naccumt1 = ${NACCUMT1},
    naccumt2 = ${NACCUMT2},
    nstartaccum1 = ${NSTARTACCUM1},
    nstartaccum2 = ${NSTARTACCUM2},
    nout = ${CYCLE_NUMBER},
    tainflatinput = ${TAINFLATINPUT},
    rhoinput = ${RHOINPUT} /
EOF

ln -fs ${BUILD_DIR}/gen_be_etkf.exe .
./gen_be_etkf.exe > gen_be_etkf.out 2>&1

#Move ensemble of analyses:
export MEM=1
while test $MEM -le $NUM_MEMBERS; do
   export CMEM=e$MEM
   if test $MEM -lt 100; then export CMEM=e0$MEM; fi
   if test $MEM -lt 10; then export CMEM=e00$MEM; fi
   mv analysis.${CMEM} ${FC_DIR}/$DATE/wrfinput_d01.${CMEM}
   export MEM=`expr $MEM + 1`
done

exit 0

