#!/bin/ksh
#########################################################################
# Script: da_run_sensitivity.ksh
#
# Purpose: Run Adjoint Sensitivity
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export DATE_R=$($BUILD_DIR/da_advance_time.exe $DATE 00 -w 2>/dev/null)
export DATE_P=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE -w 2>/dev/null)
export DATE_C=$($BUILD_DIR/da_advance_time.exe $DATE $FCST_RANGE -f ccyymmddhh 2>/dev/null)
export RUN_DIR1=$EXP_DIR/$DATE/sensitivity/wrf1
export RUN_DIR2=$EXP_DIR/$DATE/sensitivity/wrf2

mkdir -p $RUN_DIR1 $RUN_DIR2 

echo "<HTML><HEAD><TITLE>$EXPT Sensitivity</TITLE></HEAD><BODY>"
echo "<H1>$EXPT Sensitivity</H1><PRE>"

date

echo 'WRF1        <A HREF="file:'$RUN_DIR1'">'$RUN_DIR1'</a>'         
echo 'WRF2        <A HREF="file:'$RUN_DIR2'">'$RUN_DIR2'</a>'         
echo "DATE_R    = ${DATE_R}"
echo "DATE_P    = ${DATE_P}"
echo "PREV_DATE = ${PREV_DATE}"
echo "DATE      = ${DATE}"
echo "DATE_C    = ${DATE_C}"    
echo ""

##################### Setup target domain #################
      echo '**** Target domain for Forecast Aspect ****'
      echo "I, J, K (START, END) = $ADJ_ISTART $ADJ_IEND $ADJ_JSTART $ADJ_JEND $ADJ_KSTART $ADJ_KEND"
      rm -fr  ${GRAPHICS_DIR}/ini_grad4.ncl
      rm -fr  ${GRAPHICS_DIR}/Diag1_measure.ncl
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/ini_target_grad4.ncl > ${GRAPHICS_DIR}/ini_grad4.ncl
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/Diag1_target_measure.ncl > ${GRAPHICS_DIR}/Diag1_measure.ncl 

############################## Run WRFNL #################################
     if $ADJ_RUN_NL; then
        echo '**** Running WRFNL ****' 
#------------------- First WRFNL run (from Xb) --------------------
        export RUN_DIR=$RUN_DIR1
	if $CYCLING && [[ $CYCLE_NUMBER -gt 0 ]]; then
	   export WRF_INPUT_DIR=${FC_DIR}/${PREV_DATE}
    	else
	   export WRF_INPUT_DIR=${RC_DIR}/${DATE}
        fi
	
#	if [[ $ADJ_MEASURE == 4 ]]; then let NL_AUXHIST2_INTERVAL=$FCST_RANGE*60; fi
	
#        $SCRIPTS_DIR/da_trace.ksh da_run_wrfnl $RUN_DIR
        $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1
         
#------------------- Second WRFNL run (from Xa) --------------------
	if [[ $ADJ_MEASURE -ne 1 ]]; then
           export RUN_DIR=$RUN_DIR2
   	   export WRF_INPUT_DIR=${FC_DIR}/${DATE}
	   
#      	   if [[ $ADJ_MEASURE == 5 ]]; then let NL_AUXHIST2_INTERVAL=$FCST_RANGE*60; fi

#           $SCRIPTS_DIR/da_trace.ksh da_run_wrfnl $RUN_DIR
           $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1
         
	fi
     else    
        echo '**** WRFNL is not run ****' 
     fi

############################## Calculate Forecast Accuracy #################################    
     echo '**** Calculating Forecast Accuracy **** Option' $ADJ_REF
     
     #--------- Define Reference (Xt): 1->Own Analysis (3DVar)
     #-------------------------------- 2->NCEP Analysis
     #-------------------------------- 3-> Observations (Jo)  
     if [[ $ADJ_REF == 3 ]]; then
        export NL_ANALYSIS_TYPE=VERIFY
        export NL_WRFVAR_MEM_MODEL=2
     elif [[ $ADJ_REF == 1 ]]; then
        export XREF=$FC_DIR/${DATE_C}/${FILE_TYPE}_d01
     else
        export XREF=$RC_DIR/${DATE_C}/${FILE_TYPE}_d01
     fi

     #--------- First Forecast Norm: (Xbf - Xt)
     export DA_FIRST_GUESS=$RUN_DIR1/working/wrfout_d${DOMAINS}_${DATE_P}
     if [[ $ADJ_REF -ne 3 ]]; then
        export RUN_DIR=$RUN_DIR1
	cd $RUN_DIR/working
        cp $DA_FIRST_GUESS fcst
        ln -fs $XREF xref
        ncl $GRAPHICS_DIR/ini_grad4.ncl > $RUN_DIR/Fcerr.html 2>&1
     else
        export RUN_DIR=$RUN_DIR1/ObsFcErr
	mkdir -p $RUN_DIR
        DATE_TMP=$DATE
        DATE=$DATE_C
        $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
        $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    
        DATE=$DATE_TMP		
     fi   
     mv $RUN_DIR/working/fcst $RUN_DIR/auxinput3_d${DOMAINS}_${DATE_P}

     #--------- Second Forecast Norm: (Xaf - Xt)
     if [[ $ADJ_MEASURE -ne 1 ]]; then
	export DA_FIRST_GUESS=$RUN_DIR2/working/wrfout_d${DOMAINS}_${DATE_P}
	if [[ $ADJ_REF -ne 3 ]]; then
           export RUN_DIR=$RUN_DIR2
	   cd $RUN_DIR/working
           cp $DA_FIRST_GUESS fcst
           ln -fs $XREF xref
           ncl $GRAPHICS_DIR/ini_grad4.ncl > $RUN_DIR/Fcerr.html 2>&1
	else
           export RUN_DIR=$RUN_DIR2/ObsFcErr
           mkdir -p $RUN_DIR
           DATE_TMP=$DATE
           DATE=$DATE_C
           $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
           $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    		      
           DATE=$DATE_TMP		
        fi   
        mv $RUN_DIR/working/fcst $RUN_DIR/auxinput3_d${DOMAINS}_${DATE_P}
     fi

############################## Run WRF+ #################################
     if $ADJ_RUN_PLUS; then
        echo '**** Running WRFPlus ****' 
        export WRF_INPUT_DIR=${RC_DIR}/${DATE} # wrfinput/wrfbdy for dimensions only
        if [[ $ADJ_MEASURE == 4 ]]; then        
           ln -sf $RUN_DIR2/working/auxhist* $RUN_DIR1/working/.
        elif [[ $ADJ_MEASURE == 5 ]]; then        
           ln -sf $RUN_DIR1/working/auxhist* $RUN_DIR2/working/.
        fi   

#------------------- First WRF+ run --------------------
        export RUN_DIR=$RUN_DIR1
	
        if [[ $ADJ_MEASURE -ne 2 ]]; then
	       ln -sf $RUN_DIR1/auxinput3_* $RUN_DIR/working/.	   
	    else 
           ln -sf $RUN_DIR2/auxinput3_* $RUN_DIR/working/.
	    fi
	   
    #	$SCRIPTS_DIR/da_trace.ksh da_run_wrf_ad $RUN_DIR
        $SCRIPTS_DIR/da_run_wrf.ksh "AD" > $RUN_DIR/WRF_AD.html 2>&1
	
	    mv $RUN_DIR/working/ad_* $RUN_DIR

#------------------- Second WRF+ run --------------------
	    if [[ $ADJ_MEASURE -ne 1 ]]; then
           export RUN_DIR=$RUN_DIR2
	   
           if [[ $ADJ_MEASURE == 2 ]]; then
              ln -sf $RUN_DIR1/auxinput3_* $RUN_DIR/working/.
	       else   
              ln -sf $RUN_DIR2/auxinput3_* $RUN_DIR/working/.
	       fi
	          
#      	   $SCRIPTS_DIR/da_trace.ksh da_run_wrf_ad $RUN_DIR
           $SCRIPTS_DIR/da_run_wrf.ksh "AD" > $RUN_DIR/WRF_AD.html 2>&1

   	       mv $RUN_DIR/working/ad_* $RUN_DIR
	    fi        
	 else    
        echo '**** WRFPlus is not run ****' 
     fi

#------------------- Add NetCDF components --------------------
   echo '**** Adding NetCDF Components ****' 
   export RUN_DIR=$EXP_DIR/$DATE/sensitivity 
   cd $RUN_DIR  
   cp $RUN_DIR1/ad_* a
   if [[ $ADJ_MEASURE -ne 1 ]]; then
      ln -fs $RUN_DIR2/ad_* b
      ncl $GRAPHICS_DIR/wrf_sum.ncl 
      rm b
   fi
   mv a $FC_DIR/$DATE/ad_d01_$DATE 
     
#------------------- Diagnostics --------------------
   echo "**** Running Diagnostics ****"
   ln -fs $XREF xt
   cp $FC_DIR/$DATE/ad_d${DOMAINS}_$DATE ad
   ln -fs ${FC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xa
   ln -fs ${RC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xb
   if $CYCLING; then
      if [[ $CYCLE_NUMBER -gt 0 ]]; then
         ln -fs ${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE} xb
      fi
   fi
   ln -fs $RUN_DIR1/working/wrfout_d${DOMAINS}_${DATE_P} xbf
   if [[ $ADJ_MEASURE == 1 ]]; then
      cp xbf xaf
   else
      ln -fs $RUN_DIR2/working/wrfout_d${DOMAINS}_${DATE_P} xaf
   fi 

   ncl $GRAPHICS_DIR/Diag1_measure.ncl      

   rm xa xb xaf xbf xt

date

echo "</BODY></HTML>"

exit $RC

