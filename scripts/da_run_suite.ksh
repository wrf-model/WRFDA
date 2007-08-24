#!/bin/ksh
#########################################################################
# Script: da_run_suite.ksh
#
# Purpose: End-to-end testing of the WRF system.
#
# Description:
# The da_run_suite.ksh script is designed for end-to-end real data 
# testing of the following components of the WRF system:
#
# WRF real, OBSPROC, WRFVAR, UPDATE_BC, NDOWN, NUP and WRF.
#
# Any stage can be switched on/off via environment variables as
# described below. The da_run_suite.ksh script can also cycle the
# above sequence for multiple times, and so can be used for
# extended period assessment of scientific impact. I have
# successfully run the script for month long periods with 6 
# hourly cycling all the above. 
#
# Before running da_run_suite.ksh, you must do the following:
#
# 1) Compile the executables for the WRF components you wish to 
# test.
# 3) Restore input datasets (e.g. AVN fields, observations, etc).
# A template da_restore_data_mss.ksh script is called from da_run_suite.ksh, 
# which I use when working on machines that have access to NCAR's 
# Mass Store).
# 4) Overwrite default directories, filenames, namelist parameters,
# etc in script da_run_wrf_wrapper.ksh. This is done via environment
# variables. (TO DO: Automate vertical levels ENV variable)
#
# NOTE: The idea is that you overwrite defaults in da_run_suite_wrapper.ksh, 
# NOT da_run_suite.ksh itself. We want to maintain a clean script interface
# (da_run_wrf.ksh) to the WRF system. We cannot support changes made to 
# da_run_wrf.ksh. If you feel you need to modify da_run_wrf.ksh, please 
# email wrfhelp@ucar.edu and we will try to include your ideas. 
#
# Once you have set up your experiment as described above, then run the
# da_run_wrf_wrapper.ksh script.
#
# Thank you, and good luck.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh
export SUITE_DIR=${SUITE_DIR:-$RUN_DIR}

echo "<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY><H1>$EXPT</H1><PRE>"

echo 'REL_DIR      <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR      <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'WRFVAR_DIR   <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
echo 'WRFPLUS_DIR  <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
echo 'WPS_DIR      <A HREF="file:'$WPS_DIR'">'$WPS_DIR'</a>' $WPS_VN

echo "CYCLING      $CYCLING"
echo "DUMMY        $DUMMY"
echo "CLEAN        $CLEAN"
echo "NUM_PROCS    $NUM_PROCS"
echo "INITIAL_DATE $INITIAL_DATE"
echo "FINAL_DATE   $FINAL_DATE"
echo "CYCLE_PERIOD $CYCLE_PERIOD"
echo "LBC_FREQ     $LBC_FREQ"
echo "OBS_FREQ     $OBS_FREQ"
echo "WINDOW_START $WINDOW_START"
echo "WINDOW_END   $WINDOW_END"
echo 'BE_DIR       <A HREF="file:'$BE_DIR'">'$BE_DIR'</a>'
echo 'GRIB_DIR     <A HREF="file:'$GRIB_DIR'">'$GRIB_DIR'</a>'
echo 'RC_DIR       <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR       <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo 'OB_DIR       <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo 'RTOBS_DIR    <A HREF="file:'$RTOBS_DIR'">'$RTOBS_DIR'</a>'

export DATE=$INITIAL_DATE

while [[ $DATE -le $FINAL_DATE ]] ; do 
   export PREV_DATE=$($WRFVAR_DIR/build/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
   export HOUR=$(echo $DATE | cut -c9-10)

   if [[ ! -d $FC_DIR/$DATE ]]; then mkdir -p $FC_DIR/$DATE; fi

   echo "=========="
   echo $DATE
   echo "=========="

   # Decide on length of forecast to run
   export FCST_RANGE=$CYCLE_PERIOD
   if [[ $HOUR -eq $LONG_FCST_TIME_1 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_1; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_2 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_2; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_3 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_3; fi
   if [[ $HOUR -eq $LONG_FCST_TIME_4 ]]; then export FCST_RANGE=$LONG_FCST_RANGE_4; fi

   if $RUN_RESTORE_DATA_GRIB; then
      export RUN_DIR=$SUITE_DIR/$DATE/restore_data_grib
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_restore_data_grib $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_restore_data_grib.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error$RC$END"
         exit 1
      fi
   fi

   if $RUN_RESTORE_DATA_RTOBS; then
      export RUN_DIR=$SUITE_DIR/$DATE/restore_data_rtobs
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_restore_data_rtobs $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_restore_data_rtobs.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error$RC$END"
         exit 1
      fi
   fi
  
   if $RUN_WPS; then
      export RUN_DIR=$SUITE_DIR/$DATE/wps
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wps $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_wps.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
      export RUN_GEOGRID=false # Only need to run it once.
   fi

   if $RUN_REAL; then
      export RUN_DIR=$SUITE_DIR/$DATE/real
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_real $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_real.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_IDEAL; then
      export RUN_DIR=$SUITE_DIR/$DATE/ideal
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_ideal $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_ideal.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_OBSPROC; then
      export RUN_DIR=$SUITE_DIR/$DATE/obsproc
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_obsproc $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_obsproc.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00

   if $NL_VAR4D; then
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            if $RUN_UPDATE_BC; then
               export RUN_DIR=$SUITE_DIR/$DATE/update_bc_4dvar
               export PHASE=true
               mkdir -p $RUN_DIR

               $WRFVAR_DIR/scripts/da_trace.ksh da_run_update_bc $RUN_DIR
               $WRFVAR_DIR/scripts/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
               RC=$?
               if [[ $? != 0 ]]; then
        	  echo $(date) "${ERR}Failed with error $RC$END"
        	  exit 1
               fi
               export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d01
            else
               export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d01
            fi 
         fi
      fi
   fi

   if $RUN_WRFVAR; then
      export RUN_DIR=$SUITE_DIR/$DATE/wrfvar
      mkdir -p $RUN_DIR

      export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d01
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfinput_d01_${ANALYSIS_DATE}
         fi
      fi
      export DA_ANALYSIS=$FC_DIR/$DATE/analysis

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrfvar $RUN_DIR
      ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
      export WRF_INPUT=$DA_ANALYSIS
   else     
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfinput_d01_${ANALYSIS_DATE}
         fi
      fi
   fi

   if $RUN_ETKF; then
      export RUN_DIR=$EXP_DIR/run/$DATE/run_etkf
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh gen_be_etkf $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_etkf.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $? != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_UPDATE_BC; then
      if [[ $NUM_MEMBERS -gt 0 ]]; then
         export MEM=1
         export JOB=1

         while [[ $MEM -le $NUM_MEMBERS ]]; do
            export CMEM=e$MEM
            if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
            if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

            export RUN_DIR=$EXP_DIR/run/$DATE/update_bc.${CMEM}
            export PHASE=false
            mkdir -p $RUN_DIR

            export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d01
            export BDYIN=$RC_DIR/$DATE/wrfbdy_d01.${CMEM}
            export DA_ANALYSIS=$FC_DIR/$DATE/wrfinput_d01.${CMEM}
            export BDYOUT=$FC_DIR/$DATE/wrfbdy_d01}.${CMEM}

            $WRFVAR_DIR/scripts/da_trace.ksh da_run_update_bc $RUN_DIR
            $WRFVAR_DIR/scripts/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1 &
            RC=$?
            if [[ $? != 0 ]]; then
               echo $(date) "${ERR}Failed with error $RC$END"
               exit 1
            fi

            let MEM=$MEM+1
            let JOB=$JOB+1

            if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
               export JOB=1
               wait # Wait for current jobs to finish
            fi
            sleep 1 # Leave 1s gap between job start
         done
      else
         export RUN_DIR=$EXP_DIR/run/$DATE/update_bc
         export PHASE=false
         mkdir -p $RUN_DIR

         export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d01
         export BDYIN=$RC_DIR/$DATE/wrfbdy_d01
         export DA_ANALYSIS=$FC_DIR/$DATE/wrfinput_d01
         export BDYOUT=$FC_DIR/$DATE/wrfbdy_d01

         $WRFVAR_DIR/scripts/da_trace.ksh da_run_update_bc $RUN_DIR
         $WRFVAR_DIR/scripts/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
         RC=$?
         if [[ $? != 0 ]]; then
            echo $(date) "${ERR}Failed with error $RC$END"
            exit 1
         fi
      fi
   else
      export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
   fi

   if $RUN_NDOWN; then
      export RUN_DIR=$SUITE_DIR/$DATE/ndown
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_ndown $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_ndown.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_NUP; then
      export RUN_DIR=$SUITE_DIR/$DATE/nup
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh da_run_nup $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_nup.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   if $RUN_WRF; then
      if test $NUM_MEMBERS -gt 0; then
         export MEM=1
         export JOB=1

         while [[ $MEM -le $NUM_MEMBERS ]]; do
            export CMEM=e$MEM
            if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
            if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

            export RUN_DIR=$EXP_DIR/run/$DATE/wrf.${CMEM}
            mkdir -p $RUN_DIR

            export WRF_INPUT_DIR=$RC_DIR/$DATE/wrfinput_d${DOMAIN}
            export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
            if [[ $CYCLE_NUMBER -gt 0 && $CYCLING ]]; then
               export WRF_INPUT=$FC_DIR/$DATE/wrfinput_d01
               export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d01
            fi

            $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrf $RUN_DIR
            $WRFVAR_DIR/scripts/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1 &
            RC=$?
            if [[ $RC != 0 ]]; then
               echo $(date) "${ERR}Failed with error $RC$END"
               exit 1
            fi

            let MEM=$MEM+1
            let JOB=$JOB+1

            if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
               export JOB=1
               wait # Wait for current jobs to finish
            fi
            sleep 1 # Leave 1s gap between job start
         done
      else
         export RUN_DIR=$EXP_DIR/run/$DATE/wrf
         mkdir -p $RUN_DIR

         export WRF_INPUT_DIR=$RC_DIR
         if [[ $CYCLE_NUMBER -gt 0 && $CYCLING ]]; then
            export WRF_INPUT_DIR=$FC_DIR
         fi

         $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrf $RUN_DIR
         $WRFVAR_DIR/scripts/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1
         RC=$?
         if [[ $RC != 0 ]]; then
            echo $(date) "${ERR}Failed with error $RC$END"
            exit 1
         fi
      fi
   fi

   if $RUN_ENSMEAN; then
      export RUN_DIR=$EXP_DIR/run/$DATE/ensmean
      mkdir -p $RUN_DIR

      $WRFVAR_DIR/scripts/da_trace.ksh gen_be_ensmean $RUN_DIR
      $WRFVAR_DIR/scripts/da_run_ensmean.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if [[ $? != 0 ]]; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi
   fi

   export NEXT_DATE=$($WRFVAR_DIR/build/da_advance_time.exe $DATE $CYCLE_PERIOD 2>/dev/null)
   export DATE=$NEXT_DATE
   let CYCLE_NUMBER=$CYCLE_NUMBER+1

done

echo
echo $(date) "Suite finished"

echo "</PRE></BODY></HTML>"

exit 0

