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
export RUN_DIR1=$EXP_DIR/$DATE/sensitivity/wrf_xb
export RUN_DIR2=$EXP_DIR/$DATE/sensitivity/wrf_xa

mkdir -p $RUN_DIR1 $RUN_DIR2 

echo "<HTML><HEAD><TITLE>$EXPT Sensitivity</TITLE></HEAD><BODY>"
echo "<H1>$EXPT Sensitivity</H1><PRE>"

date

echo 'WRF_XB      <A HREF="file:'$RUN_DIR1'">'$RUN_DIR1'</a>'         
echo 'WRF_XA      <A HREF="file:'$RUN_DIR2'">'$RUN_DIR2'</a>'         
echo "DATE_R    = ${DATE_R}"
echo "DATE_P    = ${DATE_P}"
echo "PREV_DATE = ${PREV_DATE}"
echo "DATE      = ${DATE}"
echo "DATE_C    = ${DATE_C}"    
echo ""

##################### Setup target domain #################
      echo '**** Target domain for Forecast Aspect ****' \
           "I, J, K (START, END) = $ADJ_ISTART $ADJ_IEND $ADJ_JSTART $ADJ_JEND $ADJ_KSTART $ADJ_KEND"
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/adj_forcing.ncl > ${RUN_DIR1}/adj_forcing.ncl
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/adj_diagnostic.ncl > ${RUN_DIR1}/adj_diagnostic.ncl 

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
      		
        $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1
         
#------------------- Second WRFNL run (from Xa) --------------------
	if [[ $ADJ_MEASURE -ne 1 ]]; then
           export RUN_DIR=$RUN_DIR2
   	   export WRF_INPUT_DIR=${FC_DIR}/${DATE}
	   	   
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
     if [[ $ADJ_REF -ne 3 ]]; then
	cd $RUN_DIR1/working
        cp wrfout_d${DOMAINS}_${DATE_P} fcst
        ln -fs $XREF xref
        ncl $RUN_DIR1/adj_forcing.ncl > $RUN_DIR1/Fcerr.html 2>&1
     else
        export RUN_DIR=$RUN_DIR1/ObsFcErr
	    mkdir -p $RUN_DIR
        DATE_TMP=$DATE
        DATE=$DATE_C
        $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
        $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    
        DATE=$DATE_TMP		
     fi   
     cp $RUN_DIR1/working/fcst $RUN_DIR1/final_sens_d${DOMAINS}_${DATE_P}

     #--------- Second Forecast Norm: (Xaf - Xt)
     if [[ $ADJ_MEASURE -ne 1 ]]; then
	if [[ $ADJ_REF -ne 3 ]]; then
	   cd $RUN_DIR2/working
           cp wrfout_d${DOMAINS}_${DATE_P} fcst
           ln -fs $XREF xref
           ncl $RUN_DIR1/adj_forcing.ncl > $RUN_DIR2/Fcerr.html 2>&1
	else
           export RUN_DIR=$RUN_DIR2/ObsFcErr
           mkdir -p $RUN_DIR
           DATE_TMP=$DATE
           DATE=$DATE_C
           $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
           $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    		      
           DATE=$DATE_TMP		
        fi   
        mv $RUN_DIR2/working/fcst $RUN_DIR2/final_sens_d${DOMAINS}_${DATE_P}
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
	   ln -sf $RUN_DIR1/final_sens_d${DOMAINS}_${DATE_P} $RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}	   
	else 
           ln -sf $RUN_DIR2/final_sens_d${DOMAINS}_${DATE_P} $RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}
	fi
	   
        $SCRIPTS_DIR/da_run_wrf.ksh "AD" > $RUN_DIR/WRFAD.html 2>&1
	
	mv $RUN_DIR/working/ad_d${DOMAINS}_${DATE_R} $RUN_DIR/init_sens_d${DOMAINS}_${DATE_R}

#------------------- Second WRF+ run --------------------
	if [[ $ADJ_MEASURE -ne 1 ]]; then
           export RUN_DIR=$RUN_DIR2
	   
           if [[ $ADJ_MEASURE == 2 ]]; then
              ln -sf $RUN_DIR1/final_sens_d${DOMAINS}_${DATE_P} $RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}
	   else   
              ln -sf $RUN_DIR2/final_sens_d${DOMAINS}_${DATE_P} $RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}
	   fi
	          
           $SCRIPTS_DIR/da_run_wrf.ksh "AD" > $RUN_DIR/WRFAD.html 2>&1

   	   mv $RUN_DIR/working/ad_d${DOMAINS}_${DATE_R} $RUN_DIR/init_sens_d${DOMAINS}_${DATE_R}
        fi        
     else    
        echo '**** WRFPlus is not run ****' 
     fi

#------------------- Add NetCDF components --------------------
   echo '**** Adding NetCDF Components ****' 
   export RUN_DIR=$EXP_DIR/$DATE/sensitivity 
   cd $RUN_DIR  
   cp $RUN_DIR1/init_sens_d${DOMAINS}_${DATE_R} a
   if [[ $ADJ_MEASURE -ne 1 ]]; then
      ln -fs $RUN_DIR2/init_sens_d${DOMAINS}_${DATE_R} b
      ncl $GRAPHICS_DIR/wrf_sum.ncl 
      rm b
   fi
   mv a ad_d${DOMAINS}_$DATE
     
#------------------- Diagnostics --------------------
   echo "**** Running Diagnostics ****"
   ln -fs $XREF xt
   cp ad_d${DOMAINS}_$DATE ad
   ln -fs ${FC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xa
   ln -fs ${RC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xb
   if $CYCLING; then
      if [[ $CYCLE_NUMBER -gt 0 ]]; then
         ln -fs ${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE} xb
      fi
   fi
   ln -fs $RUN_DIR1/working/wrfout_d${DOMAINS}_${DATE_P} xbf
   ln -fs $RUN_DIR2/working/wrfout_d${DOMAINS}_${DATE_P} xaf

   ncl $RUN_DIR1/adj_diagnostic.ncl      

#------------------- Cleaning up --------------------
   rm -fr xa xb xaf xbf xt
   rm -fr ${RUN_DIR1}/adj_forcing.ncl
   rm -fr ${RUN_DIR1}/adj_diagnostic.ncl

date

echo "</BODY></HTML>"

exit $RC

