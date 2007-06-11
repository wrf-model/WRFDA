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

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

export WORK_DIR=$RUN_DIR/working

#From WRF (namelist.input):
export NL_RUN_HOURS=${NL_RUN_HOURS:-$FCST_RANGE}
if  $NL_VAR4D ; then
    export NL_RUN_HOURS=$FCST_RANGE
fi
export WRF_INPUT=${WRF_INPUT:-$RC_DIR/$DATE/wrfinput_d${DOMAIN}}
export WRF_BDY=${WRF_BDY:-$RC_DIR/$DATE/wrfbdy_d${DOMAIN}}

if test ! -d $FC_DIR/$DATE; then mkdir -p $FC_DIR/$DATE; fi
rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

#Get extra namelist variables:
. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT wrf</TITLE></HEAD><BODY>"
echo "<H1>$EXPT wrf</H1><PRE>"

date

echo 'REL_DIR    <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR    <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR    <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR   <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'RC_DIR     <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo 'FC_DIR     <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo "DATE       $DATE"
echo "END_DATE   $END_DATE"
echo "FCST_RANGE $FCST_RANGE"
echo "LBC_FREQ   $LBC_FREQ"
echo "WRF_INPUT  $WRF_INPUT"
echo "WRF_BDY    $WRF_BDY"

# Copy necessary info (better than link as not overwritten):
ln -fs ${WRF_DIR}/main/wrf.exe .
ln -fs ${WRF_DIR}/run/RRTM_DATA .
ln -fs ${WRF_DIR}/run/GENPARM.TBL .
ln -fs ${WRF_DIR}/run/LANDUSE.TBL .

ln -fs ${WRF_DIR}/run/SOILPARM.TBL .
ln -fs ${WRF_DIR}/run/VEGPARM.TBL .
ln -fs ${WRF_DIR}/run/gribmap.txt .
ln -fs ${WRF_INPUT} wrfinput_d${DOMAIN}
ln -fs ${WRF_BDY} wrfbdy_d${DOMAIN}
#cp ${RC_DIR}/$DATE/wrflowinp_d${DOMAIN} wrflowinp_d${DOMAIN}

export NL_INTERVAL_SECONDS=`expr $LBC_FREQ \* 3600`

cp $WRFVAR_DIR/inc/namelist_script_wrf_2234.inc $WRF_DIR/inc/namelist_script.inc
if test $WRF_NAMELIST'.' != '.'; then
   ln -fs $WRF_NAMELIST namelist.input
elif test -f $WRF_DIR/inc/namelist_script.inc; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

#if test ! -f $FC_DIR/$DATE/wrfout_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00; then

   if $DUMMY; then
      echo Dummy wrf
      LOCAL_DATE=$DATE
      while test $LOCAL_DATE -le $END_DATE; do
         export L_YEAR=`echo $LOCAL_DATE | cut -c1-4`
         export L_MONTH=`echo $LOCAL_DATE | cut -c5-6`
         export L_DAY=`echo $LOCAL_DATE | cut -c7-8`
         export L_HOUR=`echo $LOCAL_DATE | cut -c9-10`
         echo Dummy wrf > wrfout_d${DOMAIN}_${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00
         LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $LOCAL_DATE $NL_HISTORY_INTERVAL`
      done
   else
      if  $NL_VAR4D && test $NUM_PROCS -gt 1; then
         touch wrfnl_go_ahead
      fi
      $RUN_CMD ./wrf.exe
      RC=$?

      if test -f fort.9; then
        cp fort.9 $RUN_DIR/namelist.output
      fi

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

      echo '<A HREF="namelist.output">Namelist output</a>'
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'
      echo `date +'%D %T'` "Ended $RC"
   fi
   mv wrfout* $FC_DIR/$DATE
   mv wrf_3dvar* $FC_DIR/$DATE
#else
#   echo "$FC_DIR/$DATE/wrfout_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00 already exists, skipping"
#fi

#mkdir -p $FC_DIR/$END_DATE
#ln -fs $FC_DIR/$DATE/wrfout_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00 \
#   $FC_DIR/$END_DATE/wrfinput_d${DOMAIN}

if $CLEAN; then
   rm -rf $WORK_DIR
fi

date

echo "</BODY></HTML>"

exit 0

