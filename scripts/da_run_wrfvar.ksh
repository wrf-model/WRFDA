#!/bin/ksh

DIR=`dirname $0`

export NUM_PROCS=${NUM_PROCS:-1}               # Number of processors to run on.
export HOSTS=${HOSTS:-$PWD/hosts}
export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -nolocal -machinefile $HOSTS}

#-----------------------------------------------------------------------
# [1.0] Specify default environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010112}       # Analysis date.

#Default directories/files:

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}

export CHECK_SVNVERSION=${CHECK_SVNVERSION:-false}

if $CHECK_SVNVERSION; then
   WRFVAR_REV=`svnversion -n $WRFVAR_DIR`
   WRF_REV=`svnversion -n $WRF_DIR`
   WRFPLUS_REV=`svnversion -n $WRFPLUS_DIR`
fi

export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXPT=${EXPT:-test}
export DOMAIN=${DOMAIN:-01}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export HOSTS=${HOSTS:-$DAT_DIR/hosts/$HOSTNAME.hosts}

export RUN_DIR=${RUN_DIR:-$REG_DIR/$EXPT}
export WORK_DIR=$RUN_DIR/working
export OUT_DIR=${OUT_DIR:-$RUN_DIR/$DATE}

export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export BE_DIR=${BE_DIR:-$REG_DIR/be}
export CS_DIR=${CS_DIR:-$REG_DIR/cs}
export DA_DIR=${DA_DIR:-$RUN_DIR/da}

# Do we remove the WORK_DIR at the end to save space
export CLEAN=${CLEAN:-false}

export DUMMY=${DUMMY:-false}

export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$CS_DIR/$DATE/wrfinput_d$DOMAIN}    # wrfvar "first guess" input.
export DA_BOUNDARIES=${DA_BOUNDARIES:-$CS_DIR/$DATE/wrfbdy_d$DOMAIN}    # wrfvar boundaries input.
export DA_ANALYSIS=${DA_ANALYSIS:-$DA_DIR/$DATE/wrfvar_output}
export DA_OBSERVATIONS=${DA_OBSERVATIONS:-$OB_DIR/$DATE/ob.ascii} # wrfvar observation input.
export DA_BUFR_DIR=${DA_BUFR_DIR:-$OB_DIR/$DATE} # radiance bufr file directory
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/gen_be.NMC.dat} # wrfvar background errors.
export DA_SSMI=${DA_SSMI:-$OB_DIR/$DATE/ssmi.dat}               # SSM/I radiances (ignore if not using).
export DA_RADAR=${DA_RADAR:-$OB_DIR/$DATE/radar.dat}            # Radar data (ignore if not using).
export ENDIAN=${ENDIAN:-big_endian}

export RTTOV=${RTTOV:-$HOME/rttov/rttov85}                            # RTTOV
export DA_RTTOV_COEFFS=${DA_RTTOV_COEFFS:-$RTTOV/rtcoef_rttov7}

export NL_GLOBAL=${NL_GLOBAL:-.FALSE.}
export NL_LVAR4D=${NL_LVAR4D:-.FALSE.}
export NL_RUN_HOURS=${NL_RUN_HOURS:-6}
export NL_USE_HTML=${NL_USE_HTML:-false}

#=======================================================

mkdir -p $OUT_DIR $DA_DIR/$DATE

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT wrfvar</TITLE></HEAD><BODY><H1>$EXPT wrfvar</H1><PRE>"
fi

date

echo "Release directory:           $REL_DIR"
echo "WRFVAR directory:            $WRFVAR_DIR $WRFVAR_REV"
echo "WRF directory:               $WRF_DIR $WRF_REV"
echo "WRFPLUS directory:           $WRFPLUS_DIR $WRFPLUS_REV"
echo "Subversion revision:         $WRFVAR_REV"
echo "First Guess Input File:      $DA_FIRST_GUESS"
echo "Background Error Input File: $DA_BACK_ERRORS"
echo "Observation Input File:      $DA_OBSERVATIONS"
echo "Analysis:                    $DA_ANALYSIS"
echo "Working directory:           $WORK_DIR"
echo "Output directory:            $OUT_DIR"
echo "Start date:                  $DATE"
echo "End date:                    $END_DATE"

if test ! -f $DA_ANALYSIS; then

   rm -rf ${WORK_DIR}
   mkdir -p ${WORK_DIR}
   cd $WORK_DIR

   # DA_FG01_DATE=$DATE
   # DA_FG02_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DA_FG01 1`
   # DA_FG03_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DA_FG01 2`
   # DA_FG04_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DA_FG01 3`
   # export DA_FG01=${DA_FG01:-$OB_DIR}

   export NPROC_X=${NPROC_X:-0} # Regional, always set NPROC_X to 0, Global, always 1
   if test $NL_GLOBAL = ".TRUE."; then
      export NPROC_X=1
   fi

   export CCYY=`echo $DATE | cut -c1-4`
   export MM=`echo $DATE | cut -c5-6`
   export DD=`echo $DATE | cut -c7-8`
   export HH=`echo $DATE | cut -c9-10`

   export NL_START_YEAR=$CCYY
   export NL_START_MONTH=$MM
   export NL_START_DAY=$DD
   export NL_START_HOUR=$HH

   export NL_ANALYSIS_DATE=${CCYY}-${MM}-${DD}_${HH}:00:00.0000

   export END_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $NL_RUN_HOURS 2>/dev/null`

   export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
   export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
   export NL_END_DAY=`echo $END_DATE | cut -c7-8`
   export NL_END_HOUR=`echo $END_DATE | cut -c9-10`

   # Default WRF namelist variables:
   export NL_OB_FORMAT=${NL_OB_FORMAT:-2}              # Observation format: 1=BUFR, 2=ASCII "little_r"

   #-----------------------------------------------------------------------
   # [2.0] Perform sanity checks:
   #-----------------------------------------------------------------------

   if test ! -r $DA_FIRST_GUESS; then
      echo "${ERR}First Guess file >$DA_FIRST_GUESS< does not exist:$END"
      exit 1
   fi

   if test ! -r $DA_OBSERVATIONS; then
      echo "${ERR}Observation file >$DA_OBSERVATIONS< does not exist:$END"
      exit 1
   fi

   if test ! -r $DA_BACK_ERRORS; then
      echo "${ERR}Background Error file >$DA_BACK_ERRORS< does not exist:$END"
      exit 1
   fi

   #-----------------------------------------------------------------------
   # [3.0] Prepare for assimilation:
   #-----------------------------------------------------------------------

   if test $DA_RTTOV_COEFFS'.' != '.'; then
      ln -s $DA_RTTOV_COEFFS/* .
   fi

   cp $WRFVAR_DIR/run/gribmap.txt .
   cp $WRFVAR_DIR/run/LANDUSE.TBL .
   cp $WRFVAR_DIR/run/gmao_airs_bufr.tbl .
   cp $WRFVAR_DIR/main/wrfvar.exe  wrfvar.exe
   if test $NL_LVAR4D = .TRUE.; then
      cp $WRFPLUS_DIR/main/wrfplus.exe  wrfplus.exe # For TL and AD
      cp $WRF_DIR/main/wrf.exe  wrf.exe # for NL
      export PATH=$WRFVAR_DIR/scripts:$PATH
   fi

   ln -sf $DA_FIRST_GUESS	 wrfvar_input
   ln -sf $DA_BOUNDARIES 	 wrfbdy_d$DOMAIN
   ln -sf $DA_FIRST_GUESS	 wrfinput_d$DOMAIN
   ln -sf $DA_BACK_ERRORS   fort.34
   if test $NL_OB_FORMAT = 1; then
      ln -sf $DA_OBSERVATIONS ob.bufr
   else
      ln -sf $DA_OBSERVATIONS ob.ascii
   fi

   for FILE in $DAT_DIR/*.inv; do
      if test -f $FILE; then
         ln -s $FILE .
      fi
   done

   for FILE in $WRFVAR_DIR/run/*.bias; do
      if test -f $FILE; then
         ln -s $FILE .
      fi
   done

   for FILE in $WRFVAR_DIR/run/*.info; do
      if test -f $FILE; then
         ln -s $FILE .
      fi
   done

   for FILE in $DA_BUFR_DIR/*.bufr; do
      if test -f $FILE; then
         ln -s $FILE .
      fi
   done

   # Overwrite with specific endian files if available

   for FILE in $DA_BUFR_DIR/*.$ENDIAN; do 
      if test -f $FILE; then
         FILE1=`basename $FILE`
         ln -sf $FILE ${FILE1%%.$ENDIAN}
      fi
   done

   if test -r $DA_SSMI; then
      ln -sf $DA_SSMI	fort.93
      set NL_USE_SSMIRETRIEVALOBS = .TRUE.
   fi

   # Create namelist.input files:

   if test $NL_LVAR4D = .TRUE.; then
      export NL_AUXHIST2_INNAME='auxhist2_d<domain>_<date>'

      export NL_DYN_OPT=2
      export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input namelist.var4dnl


      export NL_DYN_OPT=202
      export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
      export NL_AUXINPUT3_INNAME='auxinput3_d<domain>_<date>'
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input namelist.var4dad


      export NL_DYN_OPT=302
      export NL_INPUT_OUTNAME='tl<date>'
      export NL_AUXHIST3_OUTNAME='auxhist3_d<domain>_<date>'
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input namelist.var4dtl
      export NL_DYN_OPT=2
   fi

   . $WRFVAR_DIR/inc/namelist_script.inc

   #-------------------------------------------------------------------
   #Run WRF-Var:
   #-------------------------------------------------------------------

   mkdir trace

   if $DUMMY; then
      echo Dummy wrfvar
      echo "Dummy wrfvar" > wrfvar_output
      echo "Dummy wrfvar" > wrfvar.out
      echo "Dummy wrfvar" > wrfvar.error
      echo "Dummy wrfvar" > fort.9
      echo "Dummy wrfvar" > fort.12
      echo "Dummy wrfvar" > fort.81
      echo "Dummy wrfvar" > fort.82
      echo "Dummy wrfvar" > rsl.out.0000
      echo "Dummy wrfvar" > rsl.error.0000
      RC=0
   else
      $RUN_CMD ./wrfvar.exe
      RC=$?
   fi

   LOCAL_DATE=`date`

   if test -f namelist.input; then
     cp namelist.input $OUT_DIR
   fi

   if test -f fort.9; then
     cp fort.9 $OUT_DIR/namelist.output
   fi

   if test -f fort.12; then
      cp fort.12 $OUT_DIR/statistics
   fi

   if test -f fort.81; then 
      cp fort.81 $OUT_DIR/cost_fn
   fi

   if test -f fort.82; then
      cp fort.82 $OUT_DIR/grad_fn
   fi

   if test -f wrfvar_output; then
      mv wrfvar_output $DA_ANALYSIS
   fi

   if test -d trace; then
      mv trace $OUT_DIR
   fi

   mkdir $OUT_DIR/rsl
   mv rsl* $OUT_DIR/rsl
   if $NL_USE_HTML; then
      cd $OUT_DIR/rsl
      for FILE in rsl*; do
         echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
         echo "<H1>$FILE</H1><PRE>" >> $FILE.html
         cat $FILE >> $FILE.html
         echo "</PRE></BODY></HTML>" >> $FILE.html
         rm $FILE
      done
      cd $OUT_DIR

      echo '<A HREF="namelist.input">Namelist input</a>'
      echo '<A HREF="namelist.output">Namelist output</a>'
      echo '<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>'
      echo '<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>'
      echo '<A HREF="rsl">Other RSL output</a>'
      echo '<A HREF="trace/0.html">PE 0 trace</a>'
      echo '<A HREF="trace">Other tracing</a>'
      echo '<A HREF="cost_fn">Cost function</a>'
      echo '<A HREF="grad_fn">Gradient function</a>'
      echo '<A HREF="statistics">Statistics</a>'
   fi

   ls -l $OUT_DIR/cost_fn

   if test $RC = 0; then
     echo "${OK}Succeeded${END} on $LOCAL_DATE"
   else
      echo "${ERR}Failed${END} on $LOCAL_DATE with error $RC"
   fi

   # We never look at core files

   for DIR in $WORK_DIR/coredir.*; do
      if test -d $DIR; then
         rm -rf $DIR
      fi
   done

   if $CLEAN; then
      rm -rf $WORK_DIR
   fi

   if test $OUT_DIR != $RUN_DIR; then
      ln -fs ${OUT_DIR##$RUN_DIR/}/* $RUN_DIR
   fi
else
   echo "$DA_ANALYSIS already exists, skipping"
fi

date

if $NL_USE_HTML; then
   echo '</PRE></BODY></HTML>'
fi

exit $RC
