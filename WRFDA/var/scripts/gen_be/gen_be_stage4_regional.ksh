#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage4_regional.ksh
#
# Purpose: To calculate correlation lengthscales for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be/gen_be_set_defaults.ksh

echo "---------------------------------------------------------------"
echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)." 
echo "---------------------------------------------------------------"

export BEGIN_CPU=$(date)
echo "Beginning CPU time: ${BEGIN_CPU}"

export TMP_DIR=${WORK_DIR}/gen_be_stage4_regional.${STRIDE}
if [[ ! -d $TMP_DIR ]]; then mkdir $TMP_DIR 2> /dev/null; fi

for VARIABLE in $CONTROL_VARIABLES; do

   # Check data exists:
   if [[ ! -d ${WORK_DIR}/$VARIABLE ]]; then
      echo "Input data directory ${WORK_DIR}/$VARIABLE is missing. Exiting"
      exit 1
   fi

   if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ]]; then
      let MAX_VINDEX=1
   else
      let MAX_VINDEX=$NUM_LEVELS
   fi

   let VINDEX=1
   let JOB=1

   while [[ $VINDEX -le $MAX_VINDEX ]]; do
      export TMP_DIR1=${TMP_DIR}/dir.${VARIABLE}${VINDEX}
      mkdir ${TMP_DIR1} 2> /dev/null
      cd ${TMP_DIR1}

      ln -sf ${BUILD_DIR}/gen_be_stage4_regional.exe .

      # Create namelist:
      cat > gen_be_stage4_regional_nl.nl << EOF
&gen_be_stage4_regional_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    ne = ${NE},
    k = ${VINDEX},
    stride = ${STRIDE},
    run_dir = '${WORK_DIR}' /
EOF
 
      if $LOCAL; then
         echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on local machine"
         # (./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1) &
         ./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1 &
      else
         export MACHINE=${MACHINES[$JOB]}
         echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on $MACHINE"
#         (rsh -n $MACHINE "cd $TMP_DIR1; ./gen_be_stage4_regional.exe > gen_be_stage4_regional_${VARIABLE}_${VINDEX}.out 2>&1") &
         export JJ=gen_be_stage4_regional_${VARIABLE}_${VINDEX}
    bsub -a poe -R "span[ptile=16]" -n ${NUM_PROCS} -o ${JJ}.out -e ${JJ}.err -J ${JJ} -q ${QUEUE} -W ${WALL_CLOCK} -P ${PROJECT_ID} mpirun.lsf gen_be_stage4_regional.exe
         sleep 2 # Create small gap between submissions to avoid overwriting output.
      fi

      let JOB=$JOB+1
      let VINDEX=$VINDEX+1

      if [[ $JOB -gt $NUM_JOBS || $VINDEX -gt $MAX_VINDEX ]]; then
         wait # Wait for current jobs to finish.
         let JOB=1
      fi
   done  # End loop over VINDEX.
done     # End loop over VARIABLE.

for VARIABLE in $CONTROL_VARIABLES; do

   if [[ $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ]]; then
      let MAX_VINDEX=1
   else
      let MAX_VINDEX=$NUM_LEVELS
   fi
      if [[ $MAX_VINDEX -le 9 ]] then
       LAST_FILE=${TMP_DIR}/dir.${VARIABLE}${MAX_VINDEX}/sl_print.${VARIABLE}.0${MAX_VINDEX}
      else
       LAST_FILE=${TMP_DIR}/dir.${VARIABLE}${MAX_VINDEX}/sl_print.${VARIABLE}.${MAX_VINDEX}
      fi 
   while [[ ! -s ${LAST_FILE} ]] ; do
   echo "Waiting for file: " ${LAST_FILE}
   sleep 600
   done 


   # Check that all lengthscales are generated
   let i=0
    while [[ $i -lt $MAX_VINDEX ]]; do
     let i=0
     let VINDEX=1
     while [[ $VINDEX -le $MAX_VINDEX ]]; do
      if [[ $VINDEX -le 9 ]] then
       FILE=${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_print.${VARIABLE}.0${VINDEX}
      else
       FILE=${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_print.${VARIABLE}.${VINDEX}
      fi 
       if [[ -s ${FILE} ]] then
        let i=$i+1
       else
       echo "Waiting for " $FILE
       sleep 60
       fi
       let VINDEX=$VINDEX+1
     done
    done
 
   # Collect files together: 

   let VINDEX=1
   cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}

   if [[ $MAX_VINDEX -gt 1 ]]; then
      let VINDEX=2
      while [[ $VINDEX -le $MAX_VINDEX ]]; do
         cat ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* >> ${WORK_DIR}/${VARIABLE}/sl_print.${VARIABLE}
         let VINDEX=$VINDEX+1
      done
   fi

done     # End loop over VARIABLE.

export END_CPU=$(date)
echo "Ending CPU time: ${END_CPU}"

exit 0

