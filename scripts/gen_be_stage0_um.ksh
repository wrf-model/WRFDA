#!/bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_stage0_um.ksh
#
# Purpose: To calculate UM perturbations in "standard fields".
#
# Note: 
# 1) START_DATE and END_DATE are defined as the times of the first and 
#    last perturbation. 
# 2) UM data are in directories where dir_name = forecast time of validity  
#   (and not forecast initialization time as in WRF)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir $STAGE0_DIR; fi

DATE=$START_DATE
END_DATE_STAGE0=$END_DATE

while [[ $DATE -le $END_DATE_STAGE0 ]]; do
   echo "gen_be_stage0_um: Calculating standard perturbation fields valid at time " $DATE
   echo ""

   export TMP_DIR=${WORK_DIR}/${DATE}
   rm -rf ${TMP_DIR} 2>/dev/null
   mkdir ${TMP_DIR}  2>/dev/null
   cd ${TMP_DIR}

   if [[ $MASSCV == "temp" ]]; then
       STANDARD_VARIABLES="psi chi t rh ps"
   else
       if [[ $BALPRES == "purestats" ]]; then
	   STANDARD_VARIABLES="psi chi p rh"
       else
	   STANDARD_VARIABLES="psi chi p lbp rh"
       fi
   fi

   for SV in $STANDARD_VARIABLES; do
      if [[ ! -d $SV ]]; then mkdir $SV; fi
   done

   #  Create file dates:
   export FCST_TIME_1=$(${BUILD_DIR}/da_advance_time.exe $DATE -${FCST_RANGE1})
   export FCST_TIME_2=$(${BUILD_DIR}/da_advance_time.exe $DATE -${FCST_RANGE2})

   export YYYY1=$(echo $FCST_TIME_1 | cut -c1-4)
   export MM1=$(echo $FCST_TIME_1 | cut -c5-6)
   export DD1=$(echo $FCST_TIME_1 | cut -c7-8)
   export HH1=$(echo $FCST_TIME_1 | cut -c9-10)
   echo "     - Forecast #1 init DATE: ${YYYY1}-${MM1}-${DD1}_${HH1}:00:00" 

   export YYYY2=$(echo $FCST_TIME_2 | cut -c1-4)
   export MM2=$(echo $FCST_TIME_2 | cut -c5-6)
   export DD2=$(echo $FCST_TIME_2 | cut -c7-8)
   export HH2=$(echo $FCST_TIME_2 | cut -c9-10)
   echo "     - Forecast #2 init DATE: ${YYYY2}-${MM2}-${DD2}_${HH2}:00:00"

   #  Softlink name
   export FILE1=UM_${DATE}
   export FILE2=UM_${DATE}.e001 # "longest-range" forecast
   export FILE3=UM_${DATE}.e002 # "shortest-range" forecast

   #  Create link to input files
   if [[ $BE_METHOD == NMC ]]; then
      ln -sf ${FC_DIR}/${DATE}/${YYYY1}${MM1}${DD1}_${UMRUN}${HH1}_${UMTAG}${FCST_RANGE1}${UMMN}.nc $FILE1
      ln -sf ${FC_DIR}/${DATE}/${YYYY1}${MM1}${DD1}_${UMRUN}${HH1}_${UMTAG}${FCST_RANGE1}${UMMN}.nc $FILE2
      ln -sf ${FC_DIR}/${DATE}/${YYYY2}${MM2}${DD2}_${UMRUN}${HH2}_${UMTAG}${FCST_RANGE2}${UMMN}.nc $FILE3
   fi

   if [[ $BE_METHOD == ENS ]]; then
      typeset -Z3 count
      count=1 
      while [[ $count -le ${NE} ]];do
         ln -sf ${FC_DIR}/${DATE}/${YYYY1}${MM1}${DD1}_${UMRUN}${HH1}_${UMTAG}${FCST_RANGE1}${UMMN}_e${count}.nc UM_${DATE}.e${count}
         (( count += 1 ))
      done
   fi

   #  Launch stage0.exe
   ln -fs ${BUILD_DIR}/gen_be_stage0_um.exe .

   ./gen_be_stage0_um.exe ${BE_METHOD} ${DATE} $NE $FILE1 $CUT $MASSCV $HUMCV $BALPRES $VERTICAL_IP $FILEPLEV $FILETLEV $UM_NPOLE_LAT $UM_NPOLE_LON $UM_LAT_SW_CORNER $UM_LON_SW_CORNER $UM_DX_DEG > gen_be_stage0_um.${DATE}.log 2>&1

   #  Tidy:
   mv pert.${DATE}* ${STAGE0_DIR}
   mv -f p_for_innerprod.${DATE} ${STAGE0_DIR} 2> /dev/null
   mv -f gen_be_stage0_um.${DATE}.log ${STAGE0_DIR}
   # rm -rf $TMP_DIR 2> /dev/null

   export DATE=$(${BUILD_DIR}/da_advance_time.exe $DATE $INTERVAL)

done     # End loop over dates.

exit 0

