#!/bin/ksh

# Run real

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export SOLVER=${SOLVER:-em}
export DUMMY=${DUMMY:-false}
export CYCLE_PERIOD=${CYCLE_PERIOD:-06}
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export LBC_FREQ=${LBC_FREQ:-06}

export RUN_DIR=${RUN_DIR:-$EXP_DIR/DATE/real}
export WORK_DIR=$RUN_DIR/working

# Do we remove the WORK_DIR at the end to save space
export CLEAN=${CLEAN:-false}

rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

echo "<HTML><HEAD><TITLE>$EXPT real</TITLE></HEAD><BODY>"
echo "<H1>$EXPT real</H1><PRE>"

date    

export D1=`$WRFVAR_DIR/build/advance_cymdh.exe ${DATE} $WINDOW_START 2>/dev/null`
let OFFSET=$FCST_RANGE+$WINDOW_START 
. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh $D1 $OFFSET

echo 'REL_DIR    <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR    <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR    <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR   <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'RC_DIR     <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo "DATE       $DATE"
echo "START_DATE $START_DATE"
echo "END_DATE   $END_DATE"

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600

export NL_AUXINPUT1_INNAME="met_em.d<domain>.<date>"

if test $WRF_NAMELIST'.' != '.' ; then
   ln -fs $WRF_NAMELIST namelist.input
elif test -f $WRF_DIR/inc/namelist_script.inc; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

if test ! -f $RC_DIR/$DATE/wrfinput_d${DOMAIN}; then
   if $DUMMY; then
      echo "Dummy real"
      echo Dummy real > wrfinput_d${DOMAIN}
      echo Dummy real > wrfbdy_d${DOMAIN}
      # echo Dummy real > wrflowinp_d${DOMAIN}
   else
      ln -fs $RC_DIR/$DATE/met_em.d* .
      cp ${WRF_DIR}/main/real.exe .
      $RUN_CMD ./real.exe
      RC=$?

      if test -f fort.9; then
        cp fort.9 $RUN_DIR/namelist.output
      fi

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
      cd $RUN_DIR

      echo '<A HREF="namelist.output">Namelist output</a>'
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'

      if test $RC = 0; then
         echo `date` "${OK}Succeeded${END}"
      else
         echo `date` "${ERR}Failed${END} with error $RC"
         exit $RC
      fi    
   fi

   mv $WORK_DIR/wrfinput_d${DOMAIN} $RC_DIR/$DATE
   mv $WORK_DIR/wrfbdy_d${DOMAIN} $RC_DIR/$DATE
#   mv $WORK_DIR/wrflowinp_d${DOMAIN} $RC_DIR/$DATE
else
   echo $RC_DIR/$DATE/wrfinput_d${DOMAIN} exists, skipping
fi

#rm -f ${MOAD_DATAROOT}/siprd/wrf_real_input_${SOLVER}*
#rm -f ${WRF_DIR}/test/${SOLVER}_real/wrf_real_input_${SOLVER}*

date

echo "</BODY></HTML>"
