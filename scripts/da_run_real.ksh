#!/bin/ksh
#########################################################################
# Script: da_run_real.ksh
#
# Purpose: Run WRF's real utility.
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

if test ! -d $RC_DIR; then mkdir $RC_DIR; fi
rm -rf $WORK_DIR
mkdir -p $RUN_DIR $WORK_DIR
cd $WORK_DIR

#Get extra namelist variables:
. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh

echo "<HTML><HEAD><TITLE>$EXPT real</TITLE></HEAD><BODY>"
echo "<H1>$EXPT real</H1><PRE>"

date    

echo 'REL_DIR    <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR    <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'RUN_DIR    <A HREF="file:'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR   <A HREF="file:'$WORK_DIR'">'$WORK_DIR'</a>'
echo 'RC_DIR     <A HREF="file:'$RC_DIR'">'$RC_DIR'</a>'
echo "DATE       $DATE"
echo "END_DATE   $END_DATE"

let NL_INTERVAL_SECONDS=$LBC_FREQ*3600

export NL_AUXINPUT1_INNAME="met_em.d<domain>.<date>"

if test ! -f $WRF_DIR/inc/namelist_script.inc; then
   # No namelist_script logic introduced during build, so add manually
   cp $WRFVAR_DIR/inc/namelist_script_wrf_2234.inc $WRF_DIR/inc/namelist_script.inc
fi

if test $WRF_NAMELIST'.' != '.' ; then
   ln -fs $WRF_NAMELIST namelist.input
elif test -f $WRF_DIR/inc/namelist_script.inc; then
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo '<A HREF="namelist.input">Namelist input</a>'

   if $DUMMY; then
      echo "Dummy real"
      echo Dummy real > wrfinput_d${DOMAIN}
      echo Dummy real > wrfbdy_d${DOMAIN}
      # echo Dummy real > wrflowinp_d${DOMAIN}
   else
      ln -fs $RC_DIR/$DATE/met_em.d* .
      ln -fs ${WRF_DIR}/main/real.exe .
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

      echo `date +'%D %T'` "Ended $RC"
   fi

   mv $WORK_DIR/wrfinput_d${DOMAIN} $RC_DIR/$DATE
   mv $WORK_DIR/wrfbdy_d${DOMAIN} $RC_DIR/$DATE
#   mv $WORK_DIR/wrflowinp_d${DOMAIN} $RC_DIR/$DATE

if $CLEAN; then
   rm -rf $WORK_DIR $RC_DIR/$DATE/met_em.d* 
fi

date

echo "</BODY></HTML>"

exit 0
