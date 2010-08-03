#!/bin/ksh
#########################################################################
# Script: da_run_tltest.ksh
#
# Purpose: Run Tangent-Linear Test
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export DATE_R=$($BUILD_DIR/da_advance_time.exe $DATE 00 -w 2>/dev/null)
export DATE_P=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE -w 2>/dev/null)
export DATE_C=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE -f ccyymmddhh 2>/dev/null)
export RUN_DIR1=$EXP_DIR/$DATE/tltest/wrf_xb
export RUN_DIR2=$EXP_DIR/$DATE/tltest/wrf_xa

mkdir -p $RUN_DIR1 $RUN_DIR2 

echo "<HTML><HEAD><TITLE>$EXPT Tangent-Linear Test</TITLE></HEAD><BODY>"
echo "<H1>$EXPT Tangent-Linear Test</H1><PRE>"

date

echo 'REL_DIR        <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'         
echo 'RUN_DIR        <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'         
echo 'RC_DIR         <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'           
echo 'WRF_XB         <A HREF="file:'$RUN_DIR1'">'$RUN_DIR1'</a>'         
echo 'WRF_XA         <A HREF="file:'$RUN_DIR2'">'$RUN_DIR2'</a>'         
echo "DATE_R    = ${DATE_R}"
echo "DATE_P    = ${DATE_P}"
echo "PREV_DATE = ${PREV_DATE}"
echo "DATE      = ${DATE}"
echo "DATE_C    = ${DATE_C}"    
echo ""
##################### Setup target domain #################
      echo '**** Target domain for Forecast Aspect ****'
      echo "I, J, K (START, END) = $ADJ_ISTART $ADJ_IEND $ADJ_JSTART $ADJ_JEND $ADJ_KSTART $ADJ_KEND"
      rm -fr  ${GRAPHICS_DIR}/Diag2_measure.ncl
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/Diag2_target_measure.ncl > ${GRAPHICS_DIR}/Diag2_measure.ncl

############################## Run WRFNL #################################
if $ADJ_RUN_NL; then	
   echo '**** Running WRFNL ****' 
#------------------- First WRFNL run (from Xb) --------------------
   export RUN_DIR=$RUN_DIR1
   export WRF_INPUT_DIR=$RC_DIR/$DATE
   if $CYCLING; then
      if [[ $CYCLE_NUMBER -gt 0 ]]; then
         export WRF_INPUT_DIR=${FC_DIR}/${PREV_DATE}
      fi
   fi   
	
   $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1

#------------------- Second WRFNL run (from Xa) --------------------
   export RUN_DIR=$RUN_DIR2
   export WRF_INPUT_DIR=${FC_DIR}/${DATE}

   $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1
else    
   echo '**** WRFNL is not run ****' 
fi

##################### Calculate Analysis Increments ######################
echo '**** Calculating Analysis Increments ****' 
export DA_ANALYSIS=$FC_DIR/$DATE/wrfinput_d01
export DA_FIRST_GUESS=$RC_DIR/$DATE/wrfinput_d01
if $CYCLING; then
   if [[ $CYCLE_NUMBER -gt 0 ]]; then
      export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/wrfout_d01_${DATE_R}
   fi
fi   

cp $DA_FIRST_GUESS a
ln -fs $DA_ANALYSIS b
ncl $GRAPHICS_DIR/wrf_diff.ncl	
rm b
mv a $RUN_DIR1/working/tl_d${DOMAINS}_${DATE_R}

############################## Run WRF+ #################################
if $ADJ_RUN_PLUS; then	
   echo '**** Running WRFPlus ****' 
   export RUN_DIR=$RUN_DIR1
   cd $RUN_DIR/working
   ln -sf tl_d${DOMAINS}_${DATE_R} wrfinput_d01     

   export WRF_INPUT_DIR=$RUN_DIR
	
   $SCRIPTS_DIR/da_run_wrf.ksh "TL" > $RUN_DIR/WRF_TL.html 2>&1
else    
   echo '**** WRFPlus is not run ****' 
fi

############################## Diagnostics #################################
echo "Computing Diagnostics"
cd $EXP_DIR/$DATE/tltest
ln -fs $RUN_DIR1/working/wrfout_d${DOMAINS}_${DATE_P} xbf
ln -fs $RUN_DIR2/working/wrfout_d${DOMAINS}_${DATE_P} xaf
ln -fs $RUN_DIR1/working/tl_d${DOMAINS}_${DATE_P} tl
ln -fs $RUN_DIR1/working/auxinput3_d01_${DATE_R} anainc

ncl $GRAPHICS_DIR/Diag2_measure.ncl
rm -f xbf xaf tl anainc
      
date

echo "</BODY></HTML>"

exit $RC
