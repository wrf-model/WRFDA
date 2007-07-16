#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage4_regional.ksh
#
# Purpose: To calculate correlation lengthscales for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/gen_be/gen_be_set_defaults.ksh

 echo "---------------------------------------------------------------"
 echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)." 
 echo "---------------------------------------------------------------"

 export BEGIN_CPU=`date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 export TMP_DIR=${RUN_DIR}/gen_be_stage4_regional.${STRIDE}
 if test ! -d $TMP_DIR; then mkdir $TMP_DIR 2> /dev/null; fi

 for VARIABLE in $CONTROL_VARIABLES; do

#   Check data exists:
    if test ! -d ${RUN_DIR}/$VARIABLE; then
       echo "Input data directory ${RUN_DIR}/$VARIABLE is missing. Exiting"
       exit
    fi

    if test $VARIABLE == "ps" || test $VARIABLE == "ps_u" || test $VARIABLE == "ps_b"; then
       export MAX_VINDEX=1
    else
       export MAX_VINDEX=$NUM_LEVELS
    fi

    export VINDEX=1
    export JOB=1

    while test $VINDEX -le $MAX_VINDEX; do

       export TMP_DIR1=${TMP_DIR}/dir.${VARIABLE}${VINDEX}
       mkdir ${TMP_DIR1} 2> /dev/null
       cd ${TMP_DIR1}

       ln -sf ${BUILD_DIR}/gen_be_stage4_regional.exe .

#      Create namelist:
cat > gen_be_stage4_regional_nl.nl << EOF
  &gen_be_stage4_regional_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    ne = ${NE},
    k = ${VINDEX},
    stride = ${STRIDE},
    run_dir = '${RUN_DIR}' /
EOF
 
       if $LOCAL; then
          echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on local machine"
#          (./gen_be_stage4_regional.exe > gen_be_stage4_regional.out 2>&1) &
          ./gen_be_stage4_regional.exe > gen_be_stage4_regional.out 2>&1 &
       else
          export MACHINE=$MACHINES[$JOB]
          echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on $MACHINE"
          (rsh -n $MACHINE "cd $TMP_DIR1; ./gen_be_stage4_regional.exe > gen_be_stage4_regional.out 2>&1") &

          sleep 2 # Create small gap between submissions to avoid overwriting output.
       fi

       export JOB=`expr $JOB + 1`
       export VINDEX=`expr $VINDEX + 1`

       if test $JOB -gt $NUM_JOBS || test $VINDEX -gt $MAX_VINDEX; then
          wait # Wait for current jobs to finish.
          export JOB=1
       fi

    done  # End loop over VINDEX.

#   Collect files together: 

    export VINDEX=1
    cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* ${RUN_DIR}/${VARIABLE}/sl_print.${VARIABLE}

    if test $MAX_VINDEX -gt 1; then
       export VINDEX=2
       while test $VINDEX -le $MAX_VINDEX; do
          cat ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* >> ${RUN_DIR}/${VARIABLE}/sl_print.${VARIABLE}
          export VINDEX=`expr $VINDEX + 1`
       done
    fi

 done     # End loop over VARIABLE.

 export END_CPU=`date`
 echo "Ending CPU time: ${END_CPU}"

 exit 0

