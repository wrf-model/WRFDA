#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage4_global.ksh
#
# Purpose: To calculate power spectra for 2D control variable fields. 
#
#-----------------------------------------------------------------------

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "---------------------------------------------------------------------"
 echo "Run Stage 4: Calculate horizontal covariances (global power spectra)."
 echo "---------------------------------------------------------------------"

 export BEGIN_CPU=`date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

 export REL_DIR=${REL_DIR:-$HOME/trunk}
 export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

 . ${WRFVAR_DIR}/scripts/gen_be/gen_be_set_defaults.ksh

 for CV in $CONTROL_VARIABLES; do
    if test ! -d ${RUN_DIR}/$CV; then
       echo "Input data directory ${RUN_DIR}/$CV is missing. Exiting"
       exit
    fi
 done
   
 ln -sf ${BUILD_DIR}/gen_be_stage4_global.exe .

 for CV in $CONTROL_VARIABLES; do
    export VARIABLE=$CV

    if test $CV == "ps"; then
       export MAX_VINDEX=1
    else if test $CV == "ps_u"; then
       export MAX_VINDEX=1
    else
       export MAX_VINDEX=$NUM_LEVELS
    fi

    export VINDEX=1
    while test $VINDEX -le $MAX_VINDEX; do

cat > gen_be_stage4_global_nl.nl << EOF
  &gen_be_stage4_global_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    gaussian_lats = ${GAUSSIAN_LATS},
    testing_spectral = ${TESTING_SPECTRAL},
    ne = ${NE},
    k = ${VINDEX} /
EOF

         ./gen_be_stage4_global.exe > gen_be_stage4_global.log 2>&1

         export VINDEX=`expr $VINDEX + 1`
    done # Loop over levels
 done    # Loop over control variables

 exit 0

