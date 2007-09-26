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
# WRFSI, WRF real, 3DVAR_OBSPROC, WRFVAR, UPDATE_BC, and WRF.
#
# Any stage can be switched on/off via environment variables as
# described below. The da_run_suite.ksh script can also cycle the
# above sequence for multiple times, and so can be used for
# extended period assessment of scientific impact. I have
# successfully run the script for month long periods with 6 
# hourly cycling all the above. 

# Before running da_run_suite.ksh, you must do the following:

# 1) Compile the executables for the WRF components you wish to 
# test.
# 3) Restore input datasets (e.g. AVN fields, observations, etc).
# A template da_restore_data_mss.ksh script is called from da_run_suite.ksh, 
# which I use when working on machines that have access to NCAR's 
# Mass Store).
# 4) Overwrite default directories, filenames, namelist parameters,
# etc in script da_run_wrf_wrapper.ksh. This is done via environment
# variables. (TO DO: Automate vertical levels ENV variable - currently hardwired
# in da_run_wrfsi.ksh).
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

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#########################################################################
# [1] Set default environment variables:
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

if test ! -d $DAT_DIR; then mkdir $DAT_DIR; fi
if test ! -d $REG_DIR; then mkdir $REG_DIR; fi
if test ! -d $EXP_DIR; then mkdir $EXP_DIR; fi
if test ! -d $EXP_DIR/run; then mkdir $EXP_DIR/run; fi

echo "<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY><H1>$EXPT</H1><PRE>"

echo 'REL_DIR      <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR      <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'WRFVAR_DIR   <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
echo 'WRFPLUS_DIR  <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
echo 'WPS_DIR      <A HREF="file:'$WPS_DIR'">'$WPS_DIR'</a>' $WPS_VN
echo 'WRFSI_DIR    <A HREF="file:'$WRFSI_DIR'">'$WRFSI_DIR'</a>'
echo 'OBSPROC_DIR  <A HREF="file:'$OBSPROC_DIR'">'$OBSPROC_DIR'</a>'

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
echo 
echo $(date) "Start"

export DATE=$INITIAL_DATE

while test $DATE -le $FINAL_DATE; do 
   export PREV_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE -$CYCLE_PERIOD 2>/dev/null`
   export HOUR=`echo $DATE | cut -c9-10`

   if test ! -d $FC_DIR/$DATE; then mkdir -p $FC_DIR/$DATE; fi

   echo "=========="
   echo $DATE
   echo "=========="

   # Decide on length of forecast to run
   export FCST_RANGE=$CYCLE_PERIOD
   if test $HOUR = $LONG_FCST_TIME_1; then export FCST_RANGE=$LONG_FCST_RANGE_1; fi
   if test $HOUR = $LONG_FCST_TIME_2; then export FCST_RANGE=$LONG_FCST_RANGE_2; fi
   if test $HOUR = $LONG_FCST_TIME_3; then export FCST_RANGE=$LONG_FCST_RANGE_3; fi
   if test $HOUR = $LONG_FCST_TIME_4; then export FCST_RANGE=$LONG_FCST_RANGE_4; fi
   
   # Check if it is "update_cycling" run:
   
   if $UPDATE_CYCLING; then
        if [[ $HOUR = $LONG_FCST_TIME_1 || $HOUR = $LONG_FCST_TIME_3 ]] then
              export CYCLING=false  # cold start
	      export FIRST=false
	    else
	      export CYCLING=true   # cycling
	      export FIRST=false
        fi	
   fi	# End of check for "update_cycling" 
   
          
   if $RUN_RESTORE_DATA_GRIB; then
      export RUN_DIR=$EXP_DIR/run/$DATE/restore_data_grib
      mkdir -p $RUN_DIR

      ${SCRIPTS_DIR}/da_trace.ksh da_run_restore_data_grib $RUN_DIR
      ${SCRIPTS_DIR}/da_restore_data_grib.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error$RC$END"
         echo restore_data_grib > FAIL
         exit 1
      fi
   fi

   if $RUN_RESTORE_DATA_RTOBS; then
      export RUN_DIR=$EXP_DIR/run/$DATE/restore_data_rtobs
      mkdir -p $RUN_DIR

      ${SCRIPTS_DIR}/da_trace.ksh da_run_restore_data_rtobs $RUN_DIR
      ${SCRIPTS_DIR}/da_restore_data_rtobs.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error$RC$END"
         echo restore_data_rtobs > FAIL
         exit 1
      fi
   fi
  
   if $RUN_WRFSI; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrfsi
      mkdir -p $RUN_DIR

      ${SCRIPTS_DIR}/da_trace.ksh da_run_wrfsi $RUN_DIR
      ${SCRIPTS_DIR}/da_run_wrfsi.ksh > $RUN_DIR/index.html 2>&1
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo wrfsi > FAIL
         exit 1
      fi
   fi

   if $RUN_WPS; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wps
      mkdir -p $RUN_DIR
       	     
       ${SCRIPTS_DIR}/da_trace.ksh da_run_wps $RUN_DIR
       ${SCRIPTS_DIR}/da_run_wps.ksh > $RUN_DIR/index.html 2>&1      
      
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo wps > FAIL
         exit 1
      fi
      export RUN_GEOGRID=false # Only need to run it once.
   fi

   if $RUN_REAL; then
      export RUN_DIR=$EXP_DIR/run/$DATE/real
      mkdir -p $RUN_DIR      
	      
	${SCRIPTS_DIR}/da_trace.ksh da_run_real $RUN_DIR
        ${SCRIPTS_DIR}/da_run_real.ksh > $RUN_DIR/index.html 2>&1          
      
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo real > FAIL
         exit 1
      fi
   fi

   if $RUN_OBSPROC; then
      export RUN_DIR=$EXP_DIR/run/$DATE/obsproc
      mkdir -p $RUN_DIR

       ${SCRIPTS_DIR}/da_trace.ksh da_run_obsproc $RUN_DIR
       ${SCRIPTS_DIR}/da_run_obsproc.ksh > $RUN_DIR/index.html 2>&1
 
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo obsproc > FAIL
         exit 1
      fi
   fi

   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00

   if $NL_VAR4D; then
     if $CYCLING; then
       if ! $FIRST; then
         if $RUN_UPDATE_BC; then
           export RUN_DIR=$EXP_DIR/run/$DATE/update_bc_4dvar
           export PHASE=true
           mkdir -p $RUN_DIR

           ${SCRIPTS_DIR}/da_trace.ksh da_run_update_bc $RUN_DIR
           ${SCRIPTS_DIR}/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
           RC=$?
           if test $? != 0; then
              echo `date` "${ERR}Failed with error $RC$END"
              echo update_bc > FAIL
              exit 1
           fi
           export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d${DOMAIN}
         else
           export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
         fi 
       fi
      fi
   fi

   if $RUN_WRFVAR; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrfvar
      mkdir -p $RUN_DIR
           
      export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d${DOMAIN}
      if $CYCLING; then
         if ! $FIRST; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrf_3dvar_input_d${DOMAIN}_${ANALYSIS_DATE}
         fi
      fi
      export DA_ANALYSIS=$FC_DIR/$DATE/analysis

      ${SCRIPTS_DIR}/da_trace.ksh da_run_wrfvar $RUN_DIR
      ${SCRIPTS_DIR}/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1                    
    
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo wrfvar > FAIL
         exit 1
      fi
      export WRF_INPUT=$DA_ANALYSIS
   else     
      if $CYCLING; then
         if ! $FIRST; then
            export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrf_3dvar_input_d${DOMAIN}_${ANALYSIS_DATE}
         fi
      fi
   fi

   if $RUN_UPDATE_BC; then
      export RUN_DIR=$EXP_DIR/run/$DATE/update_bc
      export PHASE=false
      mkdir -p $RUN_DIR      
             
      ${SCRIPTS_DIR}/da_trace.ksh da_run_update_bc $RUN_DIR
      ${SCRIPTS_DIR}/da_run_update_bc.ksh > $RUN_DIR/index.html 2>&1
      
      RC=$?
      if test $? != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo update_bc > FAIL
         exit 1
      fi
      export WRF_BDY=$FC_DIR/$DATE/wrfbdy_d${DOMAIN}
#DALE remove
   else
      export WRF_BDY=$RC_DIR/$DATE/wrfbdy_d${DOMAIN}
   fi

   if $RUN_WRF; then
      export RUN_DIR=$EXP_DIR/run/$DATE/wrf
      mkdir -p $RUN_DIR     
      
      ${SCRIPTS_DIR}/da_trace.ksh da_run_wrf $RUN_DIR
      ${SCRIPTS_DIR}/da_run_wrf.ksh > $RUN_DIR/index.html 2>&1
       
      RC=$?
      if test $RC != 0; then
         echo `date` "${ERR}Failed with error $RC$END"
         echo wrf > FAIL
         exit 1
      fi
   fi

   export NEXT_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $CYCLE_PERIOD 2>/dev/null`
   export DATE=$NEXT_DATE
   export FIRST=false

done

echo
echo $(date) "Finished"

touch SUCCESS

echo "</PRE></BODY></HTML>"

exit 0

