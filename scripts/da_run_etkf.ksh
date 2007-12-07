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
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}
export FCST_RANGE=${FCST_RANGE:-$CYCLE_PERIOD}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

export NL_CHECK_MAX_IV=.false.
export NL_ANALYSIS_TYPE="VERIFY"
export DA_ANALYSIS=analysis_not_used

#-----------------------------------------------------------------------
# [2] Set up configuration:
#-----------------------------------------------------------------------

mkdir -p $RUN_DIR
export RUN_DIR_SAVE=$RUN_DIR

export PREV_DATE=$(${BUILD_DIR}/da_advance_cymdh.exe $DATE -$FCST_RANGE)
export YYYY=$(echo $DATE | cut -c1-4)
export MM=$(echo $DATE | cut -c5-6)
export DD=$(echo $DATE | cut -c7-8)
export HH=$(echo $DATE | cut -c9-10)
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
export DA_FILE=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d${DOMAIN}_${FILE_DATE} #JEFS test uses wrfout

#-----------------------------------------------------------------------
# [3] Create observation files for ETKF:
#-----------------------------------------------------------------------

let MEM=1
let JOB=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   echo "   Producing observation file (yo, H(xb), sigma_o) for member $MEM"

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
   export DA_FIRST_GUESS=${DA_FILE}.${CMEM}

   export RUN_DIR=${RUN_DIR_SAVE}/wrfvar.${CMEM}
   mkdir -p $RUN_DIR; cd $RUN_DIR
   ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > da_run_wrfvar.${CMEM}.out 2>&1

   let MEM=$MEM+1
   let JOB=$JOB+1

   if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
done

rm ${RUN_DIR_SAVE}/ob.etkf.*
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
   export RUN_DIR=${RUN_DIR_SAVE}/wrfvar.${CMEM}

   wc -l ${RUN_DIR}/ob.etkf.000 > ${RUN_DIR_SAVE}/ob.etkf.${CMEM}
   cat ${RUN_DIR}/ob.etkf.000 >> ${RUN_DIR_SAVE}/ob.etkf.${CMEM}

# Added by JLS to write out obs count to index.html file
   obs_count=`wc -l ${RUN_DIR}/ob.etkf.000`
   echo "\n Obs count for member $MEM is ${obs_count} \n"

   let MEM=$MEM+1
done

export RUN_DIR=$RUN_DIR_SAVE
cd $RUN_DIR

#-----------------------------------------------------------------------
# [4] Calculate ensemble mean:
#-----------------------------------------------------------------------

cp ${DA_FILE}.e001 ${DA_FILE} # Initialise ensemble mean with first member.
cp ${DA_FILE}.e001 ${DA_FILE}.vari # Initialise ensemble variance with first member.

cat > gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    filestub = '${DA_FILE}'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV} /
EOF

ln -fs ${BUILD_DIR}/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

#-----------------------------------------------------------------------
# [5] Run Ensemble Transform Kalman Filter:
#-----------------------------------------------------------------------

#Prepare ETKF input/output files:

ln -sf ${DA_FILE} etkf_input                     # ETKF input mean (unchanged)
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi

   ln -sf ${DA_FILE}.${CMEM} etkf_input.${CMEM}  # ETKF input (unchanged).
   cp ${DA_FILE}.${CMEM} etkf_output.${CMEM}     # ETKF output (overwritten).
   let MEM=$MEM+1
done

#Create namelist and run ETKF:

cat > gen_be_etkf_nl.nl << EOF
  &gen_be_etkf_nl
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

mkdir -p ${FC_DIR}/$DATE
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
#    mv etkf_output.${CMEM} ${FC_DIR}/$DATE/${FILE_TYPE}_d${DOMAIN}_${FILE_DATE}.${CMEM}
# JLS: retain the name of the etkf output files for clarity
     mv etkf_output.${CMEM} ${FC_DIR}/$DATE/etkf_output.${CMEM}
   let MEM=$MEM+1
done

if $CLEAN; then
   rm -rf wrfvar.* ob.etkf.* etkf_input* *.exe
fi

exit 0

