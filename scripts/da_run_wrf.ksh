#!/bin/ksh
#########################################################################
# Script: da_run_wrf.ksh
#
# Purpose: Run WRF system.
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wrf}
export WORK_DIR=$RUN_DIR/working

if $NL_VAR4D; then
    export NL_RUN_HOURS=$FCST_RANGE
fi

# allow for ensemble members identified by CMEM
if [[ ! -z $CMEM ]]; then
   export WRF_INPUT_DIR=${WRF_INPUT_DIR:-$RC_DIR/$DATE}.$CMEM
else
   export WRF_INPUT_DIR=${WRF_INPUT_DIR:-$RC_DIR/$DATE}
fi

if [[ ! -d $FC_DIR/$DATE ]]; then mkdir -p $FC_DIR/$DATE; fi
rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

# Get extra namelist variables:
. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT wrf</TITLE></HEAD><BODY>"
echo "<H1>$EXPT wrf</H1><PRE>"

date

echo 'REL_DIR        <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'         
echo 'WRF_DIR        <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN 
echo 'RUN_DIR        <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'         
echo 'WORK_DIR       <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'       
echo 'RC_DIR         <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'           
echo 'FC_DIR         <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'              
echo 'WRF_INPUT_DIR  <A HREF="file:'$WRF_INPUT_DIR'">'$WRF_INPUT_DIR'</a>'         
echo "DATE           $DATE"                                            
echo "END_DATE       $END_DATE"                                        
echo "FCST_RANGE     $FCST_RANGE"                                      
echo "LBC_FREQ       $LBC_FREQ"  
echo "DOMAINS        $DOMAINS"
echo "MEM            $MEM"

# Copy necessary info (better than link as not overwritten):
ln -fs $WRF_DIR/main/wrf.exe .
ln -fs $WRF_DIR/run/gribmap.txt .
ln -fs $WRF_DIR/run/ozone* .
ln -fs $WRF_DIR/run/*.TBL .
ln -fs $WRF_DIR/run/*.tbl .
if $DOUBLE; then
   ln -fs $WRF_DIR/run/RRTM_DATA_DBL RRTM_DATA
   ln -fs $WRF_DIR/run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA
else
   ln -fs $WRF_DIR/run/RRTM_DATA .
   ln -fs $WRF_DIR/run/ETAMPNEW_DATA .
fi
ln -fs $WRF_DIR/run/CAM_ABS_DATA .
ln -fs $WRF_DIR/run/CAM_AEROPT_DATA .

for DOMAIN in $DOMAINS; do
   # Copy this file, so the copy back of wrfinput files later does
   # not create a recursive link
   cp $WRF_INPUT_DIR/wrfinput_d${DOMAIN} wrfinput_d${DOMAIN}
   # WHY
   # cp ${RC_DIR}/$DATE/wrflowinp_d${DOMAIN} wrflowinp_d${DOMAIN}
done
ln -fs $WRF_INPUT_DIR/wrfbdy_d01 .

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600


if [[ ! -f $WRF_DIR/inc/namelist_script.inc ]]; then
   # No namelist_script logic introduced during build, so add manually
   ln -fs $WRFVAR_DIR/inc/namelist_script_wrf_2234.inc $WRF_DIR/inc/namelist_script.inc
fi

if [[ $WRF_NAMELIST'.' != '.' ]]; then
   ln -fs $WRF_NAMELIST namelist.input
elif [[ -f $WRF_DIR/inc/namelist_script.inc ]]; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

if $DUMMY; then
   echo Dummy wrf
   LOCAL_DATE=$DATE
   while [[ $LOCAL_DATE -le $END_DATE ]]; do
      export L_YEAR=$(echo $LOCAL_DATE | cut -c1-4)
      export L_MONTH=$(echo $LOCAL_DATE | cut -c5-6)
      export L_DAY=$(echo $LOCAL_DATE | cut -c7-8)
      export L_HOUR=$(echo $LOCAL_DATE | cut -c9-10)
      for DOMAIN in $DOMAINS; do
         echo Dummy wrf > wrfout_d${DOMAIN}_${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00
         echo Dummy wrf > wrfinput_d${DOMAIN}_${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00
      done
      LOCAL_DATE=$($WRFVAR_DIR/build/da_advance_time.exe $LOCAL_DATE $NL_HISTORY_INTERVAL)
   done
else
   if $NL_VAR4D && [[ $NUM_PROCS -gt 1 ]]; then
      touch wrfnl_go_ahead
   fi
   $RUN_CMD ./wrf.exe
   RC=$?
   grep -q 'SUCCESS COMPLETE WRF' rsl.out.0000 

   rm -rf $RUN_DIR/rsl
   mkdir -p $RUN_DIR/rsl
   mv rsl* $RUN_DIR/rsl > /dev/null 2>&1
   cd $RUN_DIR/rsl
   for FILE in rsl*; do
      echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
      echo "<H1>$FILE</H1><PRE>" >> $FILE.html
      cat $FILE >> $FILE.html
      echo "</PRE></BODY></HTML>" >> $FILE.html
      rm $FILE
   done
   cd $WORK_DIR
   cp namelist.output $RUN_DIR
   echo '<A HREF="namelist.output">Namelist output</a>'

   if [[ -f rsl.out.0000 ]]; then
      rm -rf $RUN_DIR/rsl
      mkdir -p $RUN_DIR/rsl
      mv rsl* $RUN_DIR/rsl
      cd $RUN_DIR/rsl
      for FILE in rsl*; do
         echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
         echo "<H1>$FILE</H1><PRE>" >> $FILE.html
         cat $FILE >> $FILE.html
         echo "</PRE></BODY></HTML>" >> $FILE.html
         rm $FILE
      done
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'
   fi

   echo $(date +'%D %T') "Ended $RC"
fi

mv wrfinput_* $FC_DIR/$DATE
mv wrfout_* $FC_DIR/$DATE

if $CLEAN; then
   rm -rf $WORK_DIR
fi

date

echo "</BODY></HTML>"

exit $RC

