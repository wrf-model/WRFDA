#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_stage4_regional.csh
#
# Purpose: To calculate correlation lengthscales for 2D control variable fields. 
#
# History:
#     06/06/2005    Modified by                          Y.-R. Guo
#-----------------------------------------------------------------------

#Define job by overriding default environment variables:

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

 echo "---------------------------------------------------------------"
 echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)." 
 echo "---------------------------------------------------------------"

 set BEGIN_CPU = `date`
 echo "Beginning CPU time: ${BEGIN_CPU}"

#Define environment variables:
 if ( ! $?START_DATE )    setenv START_DATE    2003081512 # Starting time of period.
 if ( ! $?END_DATE )      setenv END_DATE      2003091500 # Ending time of period.
 if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
 if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
 if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
 if ( ! $?STRIDE )        setenv STRIDE 1                 # Calculate correlation evert STRIDE point.
 if ( ! $?NUM_LEVELS )    setenv NUM_LEVELS    30         # Hard-wired for now....

 if ( ! $?EXPT )          setenv EXPT         wrfvar_cwb_be
 if ( ! $?ID )            setenv ID           cwb_wrf
 if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR   /palm/users/guo/wrfvar
 if ( ! $?SRC_DIR )       setenv SRC_DIR      ${WRFVAR_DIR}/gen_be
 if ( ! $?DAT_DIR )       setenv DAT_DIR      /mmmtmp/guo/${EXPT}
 if ( ! $?RUN_DIR )       setenv RUN_DIR      ${DAT_DIR}/${ID}
 if ( ! -d ${RUN_DIR} )   mkdir ${RUN_DIR}

 if ( ! $?LOCAL )         setenv LOCAL       .true.                                          # True if local machine.
 if ( ! $?NUM_JOBS )      setenv NUM_JOBS    1                                               # Number of jobs to run.
 if ( ! $?MACHINES )      set MACHINES = ( node1 node2 node3 node4 node1 node2 node3 node4)  # For parallel runs.
 if ( ! $?CONTROL_VARIABLES) set CONTROL_VARIABLES = ( psi chi_u t_u rh ps_u )               # Fields to process.

 setenv TMP_DIR ${RUN_DIR}/gen_be_stage4_regional.${STRIDE}
 if ( ! -d ${TMP_DIR} ) mkdir ${TMP_DIR} >&! /dev/null

#OK, let's go!

 foreach VARIABLE ( $CONTROL_VARIABLES )

#   Check data exists:
    if ( ! -d ${RUN_DIR}/$VARIABLE ) then
       echo "Input data directory ${RUN_DIR}/$VARIABLE is missing. Exiting"
       exit
    endif

    if ( $VARIABLE == "ps" || $VARIABLE == "ps_u" || $VARIABLE == "ps_b" ) then
       setenv MAX_VINDEX 1
    else
       setenv MAX_VINDEX $NUM_LEVELS
    endif

    set VINDEX = 1
    set JOB = 1

    while ( $VINDEX <= $MAX_VINDEX )

       setenv TMP_DIR1 ${TMP_DIR}/dir.${VARIABLE}${VINDEX}
       mkdir ${TMP_DIR1} >&! /dev/null

       cd ${TMP_DIR1}
       ln -sf ${SRC_DIR}/gen_be_stage4_regional.exe .

#      Create namelist:
cat >! gen_be_stage4_regional_nl.nl << EOF
  &gen_be_stage4_regional_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    k = ${VINDEX},
    stride = ${STRIDE},
    run_dir = '${RUN_DIR}' /
EOF
 
       if ( $LOCAL == .true. ) then
          echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on local machine"
          (./gen_be_stage4_regional.exe >&! gen_be_stage4_regional.out) &
       else
          setenv MACHINE $MACHINES[$JOB]
          echo "Submitting job for variable $VARIABLE and vertical index $VINDEX on $MACHINE"
          (rsh -n $MACHINE "cd ${TMP_DIR1}; ./gen_be_stage4_regional.exe >&! gen_be_stage4_regional.out") &

          sleep 2 # Create small gap between submissions to avoid overwriting output.
       endif

       set JOB = `expr $JOB + 1`
       set VINDEX = `expr $VINDEX + 1`

       if ( $JOB > $NUM_JOBS || $VINDEX > $MAX_VINDEX  ) then
          wait # Wait for current jobs to finish.
          set JOB = 1
       endif

    end  # End loop over VINDEX.

#   Collect files together: 

    set VINDEX = 1
    cp ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* ${RUN_DIR}/${VARIABLE}/sl_print.${VARIABLE}

    if ( $MAX_VINDEX > 1 ) then
       set VINDEX = 2
       while ( $VINDEX <= $MAX_VINDEX )
          cat ${TMP_DIR}/dir.${VARIABLE}${VINDEX}/sl_* >> ${RUN_DIR}/${VARIABLE}/sl_print.${VARIABLE}
          set VINDEX = `expr $VINDEX + 1`
       end
    endif

 end     # End loop over VARIABLE.

 set END_CPU = `date`
 echo "Ending CPU time: ${END_CPU}"

 exit(0)

