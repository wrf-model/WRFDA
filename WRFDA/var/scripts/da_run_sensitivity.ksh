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
if $RUN_ADJ_SENS; then
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/adj_forcing.ncl > ${RUN_DIR1}/adj_forcing.ncl
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/adj_diagnostic.ncl > ${RUN_DIR1}/adj_diagnostic.ncl 
fi
if $RUN_TL_TEST; then
      m4 -D istart=$ADJ_ISTART -D iend=$ADJ_IEND -D jstart=$ADJ_JSTART -D jend=$ADJ_JEND -D kstart=$ADJ_KSTART -D kend=$ADJ_KEND \
            ${GRAPHICS_DIR}/tl_diagnostic.ncl > ${RUN_DIR1}/tl_diagnostic.ncl 
fi

############################## Run WRFNL #################################
     if $ADJ_RUN_NL; then
        echo '**** Running WRFNL from Xb ****' 
#------------------- First WRFNL run (from Xb) --------------------
        export RUN_DIR=$RUN_DIR1
        if $CYCLING && [[ $CYCLE_NUMBER -gt 0 ]]; then
           export WRF_INPUT_DIR=${FC_DIR}/${PREV_DATE}
    	else
           export WRF_INPUT_DIR=${RC_DIR}/${DATE}
        fi
      		
        $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1
         
#------------------- Second WRFNL run (from Xa) --------------------
        echo '**** Running WRFNL from Xa ****' 
        export RUN_DIR=$RUN_DIR2
        export WRF_INPUT_DIR=${FC_DIR}/${DATE}
	   	   
        $SCRIPTS_DIR/da_run_wrf.ksh "NL" > $RUN_DIR/WRFNL.html 2>&1         
     else    
        echo '**** WRFNL is not run ****' 
     fi

##################### Calculate Analysis Increments ######################
   if $RUN_TL_TEST; then
     echo '**** Calculating Analysis Increments ****' 
     export DA_ANALYSIS=$FC_DIR/$DATE/${FILE_TYPE}_d01
     export DA_FIRST_GUESS=$RC_DIR/$DATE/${FILE_TYPE}_d01
     if $CYCLING; then
        if [[ $CYCLE_NUMBER -gt 0 ]]; then
           export DA_FIRST_GUESS=${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${DATE_R}
        fi
     fi   

     cp $DA_FIRST_GUESS a
     ln -fs $DA_ANALYSIS b
     ncl $GRAPHICS_DIR/wrf_diff.ncl	
     rm b
     mv a $RUN_DIR1/working/${FILE_TYPE}_d01
   
############################## Calculate Forecast Accuracy #################################    
   else    
     echo '**** Calculating Forecast Accuracy **** Option' $ADJ_REF
     
     #--------- Define Reference (Xt): 1->Own Analysis (3DVar)
     #-------------------------------- 2->NCEP Analysis
     #-------------------------------- 3-> Observations (Jo)  
     if   [[ $ADJ_REF == 1 ]]; then
        export XREF=$FC_DIR/${DATE_C}/${FILE_TYPE}_d01
     elif [[ $ADJ_REF == 3 ]]; then
        export NL_ANALYSIS_TYPE=VERIFY
        export NL_ADJ_SENS=true
        export NL_IOFIELDS_FILENAME="${DAT_DIR}/fso.io_config"
        export NL_IO_FORM_AUXINPUT7=2
        DATE_TMP=$DATE
        DATE=$DATE_C
        if $NL_USE_VARBC; then
           export VARBC_PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$VARBC_CYCLE_PERIOD 2>/dev/null)
           if  [[ -f ${SUITE_DIR}/${VARBC_PREV_DATE}/wrfvar/VARBC.out ]]; then
	      export DA_VARBC_IN=${SUITE_DIR}/${VARBC_PREV_DATE}/wrfvar/VARBC.out
           fi
	fi
     else
        export XREF=$RC_DIR/${DATE_C}/${FILE_TYPE}_d01
     fi

     #--------- First Forecast Norm: (Xbf - Xt)
     export RUN_DIR=$RUN_DIR1
     if [[ $ADJ_REF -ne 3 ]]; then
        cd $RUN_DIR/working
	cp wrfout_d${DOMAINS}_${DATE_P} fcst
        ln -fs $XREF xref
        ncl $RUN_DIR1/adj_forcing.ncl > $RUN_DIR1/Fcerr.html 2>&1
     else
        export DA_FIRST_GUESS=$RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}
        export RUN_DIR=$RUN_DIR/ObsFcErr
        mkdir -p $RUN_DIR
        $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
        $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    
     fi   
     mv $RUN_DIR/working/fcst $RUN_DIR1/final_sens_d01

     #--------- Second Forecast Norm: (Xaf - Xt)
     if [[ $ADJ_MEASURE -ne 1 ]]; then
        export RUN_DIR=$RUN_DIR2
        if [[ $ADJ_REF -ne 3 ]]; then
           cd $RUN_DIR/working
           cp wrfout_d${DOMAINS}_${DATE_P} fcst
           ln -fs $XREF xref
           ncl $RUN_DIR1/adj_forcing.ncl > $RUN_DIR2/Fcerr.html 2>&1
	else
           export DA_FIRST_GUESS=$RUN_DIR/working/wrfout_d${DOMAINS}_${DATE_P}
           export RUN_DIR=$RUN_DIR/ObsFcErr
           mkdir -p $RUN_DIR
           $SCRIPTS_DIR/da_trace.ksh da_run_wrfvar $RUN_DIR
           $SCRIPTS_DIR/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1    		      
        fi   
        mv $RUN_DIR/working/fcst $RUN_DIR2/final_sens_d01
     fi   
     
     if [[ $ADJ_REF == 3 ]]; then; DATE=$DATE_TMP; fi
   fi

############################## Run WRF+ #################################
        if $ADJ_RUN_PLUS; then
        echo '**** Running WRFPlus ****' 
        
        if $RUN_TL_TEST; then
           export ADJ_MEASURE=1
           export WRFPLUS_OPTION="TL"
           export WRF_INPUT_DIR=$RUN_DIR1/working
        else    
           export WRFPLUS_OPTION="AD"
	   export WRF_INPUT_DIR=${RC_DIR}/${DATE} # wrfinput/wrfbdy for dimensions only	
        fi
   
        if [[ $ADJ_MEASURE == 4 ]]; then        
           if  $NL_TRAJECTORY_IO; then
              export WRF_INPUT_DIR=${FC_DIR}/${DATE}
           else
              export WRF_INPUT_DIR=${FC_DIR}/${DATE}
              ln -sf $RUN_DIR2/working/auxhist* $RUN_DIR1/working/.
           fi
        elif [[ $ADJ_MEASURE == 5 ]]; then        
           if  $NL_TRAJECTORY_IO; then
              export WRF_INPUT_DIR=${RC_DIR}/${DATE}
           else
              export WRF_INPUT_DIR=${RC_DIR}/${DATE}
              ln -sf $RUN_DIR1/working/auxhist* $RUN_DIR2/working/.
           fi
        fi
	   
#------------------- First WRF+ run --------------------
        echo '**** Running WRFPlus along Xb ****' 

        export RUN_DIR=$RUN_DIR1

        if [[ $ADJ_MEASURE -ne 2 ]]; then
           ln -sf $RUN_DIR1/final_sens_d${DOMAINS} $RUN_DIR/working/final_sens_d${DOMAINS}
        else 
           ln -sf $RUN_DIR2/final_sens_d${DOMAINS} $RUN_DIR/working/final_sens_d${DOMAINS}
        fi
       
        $SCRIPTS_DIR/da_run_wrf.ksh $WRFPLUS_OPTION > $RUN_DIR/WRF$WRFPLUS_OPTION.html 2>&1

        if $RUN_TL_TEST; then
           cp $RUN_DIR/working/tl_d${DOMAINS}_${DATE_P} $RUN_DIR/final_pert_d${DOMAINS}_${DATE_P}	
	else
           mv $RUN_DIR/working/gradient_wrfplus_d${DOMAINS}_${DATE_R} $RUN_DIR/init_sens_d${DOMAINS}_${DATE_R}
        fi
	
#------------------- Second WRF+ run --------------------
        if [[ $ADJ_MEASURE -ne 1 ]]; then
           echo '**** Running WRFPlus along Xa ****' 
           export RUN_DIR=$RUN_DIR2

           if [[ $ADJ_MEASURE == 2 ]]; then
              ln -sf $RUN_DIR1/final_sens_d${DOMAINS} $RUN_DIR/working/final_sens_d${DOMAINS}
           else   
              ln -sf $RUN_DIR2/final_sens_d${DOMAINS} $RUN_DIR/working/final_sens_d${DOMAINS}
           fi

           $SCRIPTS_DIR/da_run_wrf.ksh $WRFPLUS_OPTION > $RUN_DIR/WRF$WRFPLUS_OPTION.html 2>&1

           mv $RUN_DIR/working/gradient_wrfplus_d${DOMAINS}_${DATE_R} $RUN_DIR/init_sens_d${DOMAINS}_${DATE_R}
        fi        
     else    
        echo '**** WRFPlus is not run ****' 
     fi

#------------------- Add NetCDF components --------------------
 cd $EXP_DIR/$DATE/sensitivity  

 if $RUN_ADJ_SENS; then
   echo '**** Adding NetCDF Components ****' 
   cp $RUN_DIR1/init_sens_d${DOMAINS}_${DATE_R} a
   if [[ $ADJ_MEASURE -ne 1 ]]; then
      ln -fs $RUN_DIR2/init_sens_d${DOMAINS}_${DATE_R} b
   else
      cp a b
   fi   
   
   ncl $GRAPHICS_DIR/wrf_sum.ncl 
   
   mv a ad_d${DOMAINS}_$DATE
   rm b
 fi
     
#------------------- Diagnostics --------------------
   echo "**** Running Diagnostics ****"

 ln -fs $RUN_DIR1/working/wrfout_d${DOMAINS}_${DATE_P} xbf
 ln -fs $RUN_DIR2/working/wrfout_d${DOMAINS}_${DATE_P} xaf

 if $RUN_ADJ_SENS; then
   if [[ $ADJ_REF -ne 3 ]]; then
      ln -fs $XREF xt
      ln -fs ad_d${DOMAINS}_$DATE ad
      ln -fs ${FC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xa
      ln -fs ${RC_DIR}/$DATE/${FILE_TYPE}_d${DOMAINS} xb
      if $CYCLING; then
         if [[ $CYCLE_NUMBER -gt 0 ]]; then
            ln -fs ${FC_DIR}/${PREV_DATE}/${FILE_TYPE}_d01_${ANALYSIS_DATE} xb
         fi
      fi
      ncl $RUN_DIR1/adj_diagnostic.ncl      
   fi
 else
   ln -fs $RUN_DIR1/working/tl_d${DOMAINS}_${DATE_P} tl
   ncl $RUN_DIR1/tl_diagnostic.ncl   
 fi

#------------------- Cleaning up --------------------
   rm -fr xa xb xaf xbf xt tl anainc
   rm -fr ${RUN_DIR1}/adj_forcing.ncl
   rm -fr ${RUN_DIR1}/adj_diagnostic.ncl
   rm -fr ${RUN_DIR1}/tl_diagnostic.ncl

date

echo "</BODY></HTML>"

exit $RC

