#!/bin/ksh

PLATFORM=`uname`

DIR=`dirname $0`
SVN_REV=`svnversion -n $DIR`

export MP_SHARED_MEMORY=yes

echo ""
echo "Running script da_run_wrfvar.ksh"
echo "---------------------------------"
echo ""

date

#-----------------------------------------------------------------------
# [1.0] Specify default environment variables:
#-----------------------------------------------------------------------

export DA_DATE=${DA_DATE:-2003010112}       # Analysis date.

#Default directories/files:

export RELEASE=${RELEASE:-WRF_V2.1.2}
export REL_DIR=${REL_DIR:-$HOME/code_development/$RELEASE}
export DA_DIR=${DA_DIR:-$REL_DIR/wrfvar}
export DA_BUILD_DIR=${DA_BUILD_DIR:-$DA_DIR/main}
export DA_INC_DIR=${DA_INC_DIR:-$DA_DIR/inc}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}
export WRFPLUS_BUILD_DIR=${WRFPLUS_BUILD_DIR:-$WRFPLUS_DIR/main}
export WRFPLUS_INC_DIR=${WRFPLUS_INC_DIR:-$WRFPLUS_DIR/inc}
export DA_ID=${DA_ID:-wrfvar}
export HOSTS=${HOSTS:-$PWD/hosts}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-/data7/da/bray/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export BE_DIR=${BE_DIR:-$REG_DIR/be}
export CS_DIR=${CS_DIR:-$REG_DIR/cs}

export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DA_ID}

rm -rf ${RUN_DIR}
mkdir -p ${RUN_DIR}
cd $RUN_DIR

export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$CS_DIR/$DA_DATE/wrfinput_d01}    # wrfvar "first guess" input.
export DA_BOUNDARIES=${DA_BOUNDARIES:-$CS_DIR/$DA_DATE/wrfbdy_d01}    # wrfvar boundaries input.
export DA_OBSERVATIONS=${DA_OBSERVATIONS:-$OB_DIR/$DA_DATE/ob.ascii} # wrfvar observation input.
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/gen_be.NMC.dat} # wrfvar background errors.
export DA_SSMI=${DA_SSMI:-$OB_DIR/$DA_DATE/ssmi.dat}               # SSM/I radiances (ignore if not using).
export DA_RADAR=${DA_RADAR:-$OB_DIR/$DA_DATE/radar.dat}            # Radar data (ignore if not using).

# DA_FG01_DATE=$DA_DATE
# DA_FG02_DATE=`$DA_BUILD_DIR/advance_cymdh.exe $DA_FG01 1`
# DA_FG03_DATE=`$DA_BUILD_DIR/advance_cymdh.exe $DA_FG01 2`
# DA_FG04_DATE=`$DA_BUILD_DIR/advance_cymdh.exe $DA_FG01 3`
# export DA_FG01=${DA_FG01:-$OB_DIR}

export RTTOV=${RTTOV:-$HOME/rttov/rttov85}                            # RTTOV

export NUM_PROCS=${NUM_PROCS:-1}               # Number of processors to run on.

export NL_GLOBAL=${NL_GLOBAL:-.FALSE.}
export NL_LVAR4D=${NL_LVAR4D:-.FALSE.}
export NL_RUN_HOURS=${NL_RUN_HOURS:-1}
export NL_USE_HTML=${NL_USE_HTML:-.FALSE.}

export NPROC_X=${NPROC_X:-0}                        # Regional, always set NPROC_X to 0, Global, always 1
if test $NL_GLOBAL = ".TRUE."; then
   export NPROC_X=1
fi

export DA_CY=`echo $DA_DATE | cut -c1-4`
export DA_MM=`echo $DA_DATE | cut -c5-6`
export DA_DD=`echo $DA_DATE | cut -c7-8`
export DA_HH=`echo $DA_DATE | cut -c9-10`

export NL_START_YEAR=$DA_CY
export NL_START_MONTH=$DA_MM
export NL_START_DAY=$DA_DD
export NL_START_HOUR=$DA_HH

export NL_ANALYSIS_DATE=${DA_CY}-${DA_MM}-${DA_DD}_${DA_HH}:00:00.0000

export DA_END_DATE=`$DA_BUILD_DIR/advance_cymdh.exe $DA_DATE $NL_RUN_HOURS`

export DA_CY=`echo $DA_END_DATE | cut -c1-4`
export DA_MM=`echo $DA_END_DATE | cut -c5-6`
export DA_DD=`echo $DA_END_DATE | cut -c7-8`
export DA_HH=`echo $DA_END_DATE | cut -c9-10`

export NL_END_YEAR=$DA_CY
export NL_END_MONTH=$DA_MM
export NL_END_DAY=$DA_DD
export NL_END_HOUR=$DA_HH

# Default WRF namelist variables:
export NL_OB_FORMAT=${NL_OB_FORMAT:-2}              # Observation format: 1=BUFR, 2=ASCII "little_r"

#-----------------------------------------------------------------------
# [2.0] Perform sanity checks:
#-----------------------------------------------------------------------

if test ! -r $DA_FIRST_GUESS; then
  echo "Error: First Guess file does not exist:"
  echo  $DA_FIRST_GUESS
  exit 1
fi

if test ! -r $DA_OBSERVATIONS; then
  echo "Error: Observation file does not exist:"
  echo  $DA_OBSERVATIONS
  exit 1
fi

if test ! -r $DA_BACK_ERRORS; then
  echo "Error: Background Error file does not exist:"
  echo  $DA_BACK_ERRORS
  exit 1
fi

#-----------------------------------------------------------------------
# [3.0] Prepare for assimilation:
#-----------------------------------------------------------------------

if test $DA_RTTOV_COEFFS'.' != '.'; then
  ln -s $DA_RTTOV_COEFFS/* .
fi

cp $DA_DIR/run/gribmap.txt .
cp $DA_DIR/run/LANDUSE.TBL .
cp $DA_DIR/run/gmao_airs_bufr.tbl .
cp $DA_BUILD_DIR/wrfvar.exe  wrfvar.exe
if test $NL_LVAR4D = .TRUE.; then
  cp $WRFPLUS_BUILD_DIR/wrfplus.exe  wrfplus.exe
  ln -s $DA_DIR/run/*sh .
  export PATH=$DA_DIR/run:$PATH
fi

ln -sf $DA_FIRST_GUESS	 wrfvar_input
ln -sf $DA_BOUNDARIES 	 wrfbdy_d01
ln -sf $DA_FIRST_GUESS	 wrfinput_d01
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

for FILE in $DA_DIR/run/*.bias; do
  if test -f $FILE; then
    ln -s $FILE .
  fi
done

# Link to bufr files, picking which endian type when a
# choice is available

for FILE in $DAT_DIR/*.bufr; do
  if test -f $FILE; then
    ln -s $FILE .
  fi
done

if test $PLATFORM = "Linux"; then
#  ENDIAN=little_endian
  ENDIAN=big_endian
fi

if test $PLATFORM = "Darwin" -o $PLATFORM = "AIX"; then
  ENDIAN=big_endian
fi

for FILE in $DAT_DIR/*.$ENDIAN; do 
  if test -f $FILE; then
    FILE1=`basename $FILE`
    ln -sf $FILE ${FILE1%%.$ENDIAN}
  fi
done

if test -r $DA_SSMI; then
  ln -sf $DA_SSMI	fort.93
  set NL_USE_SSMIRETRIEVALOBS = .TRUE.
fi

DATE=`date`

if test $NL_USE_HTML = .TRUE.; then

   HTML=$EXP_DIR/index.html

   cat > $HTML << EOF
<HTML><HEAD><TITLE>$EXPT</TITLE></HEAD><BODY>
<H1>$EXPT</H1>

<PRE>
Started: $DATE
Experiment directory:        $EXP_DIR
Run directory:               $RUN_DIR
Release directory:           $REL_DIR
Subversion revision:         $SVN_REV
First Guess Input File:      $DA_FIRST_GUESS
Background Error Input File: $DA_BACK_ERRORS
Observation Input File:      $DA_OBSERVATIONS
Start date:                  $DA_DATE
End date:                    $DA_END_DATE
EOF
fi

echo "Experiment directory:        $EXP_DIR"
echo "Run directory:               $RUN_DIR"
echo "Release directory:           $REL_DIR"
echo "Subversion revision:         $SVN_REV"
echo "First Guess Input File:      $DA_FIRST_GUESS"
echo "Background Error Input File: $DA_BACK_ERRORS"
echo "Observation Input File:      $DA_OBSERVATIONS"
echo "Start date:                  $DA_DATE"
echo "End date:                    $DA_END_DATE"


# Create namelist.input file:


if test $NL_LVAR4D = .TRUE.; then
   export NL_AUXHIST2_INNAME='auxhist2_d<domain>_<date>'

   export NL_DYN_OPT=2
   export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
   . $WRFPLUS_INC_DIR/namelist_script.inc
   mv namelist.input namelist.var4dnl


   export NL_DYN_OPT=202
   export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
   export NL_AUXINPUT3_INNAME='auxinput3_d<domain>_<date>'
   . $WRFPLUS_INC_DIR/namelist_script.inc
   mv namelist.input namelist.var4dad


   export NL_DYN_OPT=302
   export NL_INPUT_OUTNAME='tl<date>'
   export NL_AUXHIST3_OUTNAME='auxhist3_d<domain>_<date>'
   . $WRFPLUS_INC_DIR/namelist_script.inc
   mv namelist.input namelist.var4dtl
   export NL_DYN_OPT=2
fi

. $DA_INC_DIR/namelist_script.inc



mkdir trace

#-------------------------------------------------------------------
#Run WRF-Var:
#-------------------------------------------------------------------

if test $PLATFORM = "Linux"; then
  if test $NUM_PROCS -gt 1; then
    mpirun -v -np $NUM_PROCS -nolocal -machinefile $HOSTS ./wrfvar.exe > wrfvar.out 2>wrfvar.error
    RC=$?
  else
    # mpirun -v -np 1 ./wrfvar.exe > /dev/null #Assumes compile in DM mode.
    ./wrfvar.exe > wrfvar.out 2>wrfvar.error
    RC=$?
  fi
fi

if test $PLATFORM = "Darwin"; then
  if test $NUM_PROCS -gt 1; then
    mpirun -v -np $NUM_PROCS -all-local -machinefile $HOSTS ./wrfvar.exe > wrfvar.out 2>wrfvar.error
    RC=$?
  else
    ./wrfvar.exe > wrfvar.out 2>wrfvar.error
    RC=$?
  fi
fi

#DEC:
#./wrfvar.exe > wrfvar.out 2>wrfvar.error
#RC=$?

if test $PLATFORM = "AIX"; then
  #IBM (llsubmit):
  #poe ./wrfvar.exe
  #mpirun -np ${NUM_PROCS} ./wrfvar.exe
  ./wrfvar.exe > wrfvar.out 2>wrfvar.error
  RC=$?
fi

DATE=`date`

cd $EXP_DIR

if test -f $RUN_DIR/fort.9; then
  cp $RUN_DIR/fort.9 namelist.output
fi

if test -f $RUN_DIR/fort.12; then
  cp $RUN_DIR/fort.12 statistics
fi

if test -f $RUN_DIR/fort.81; then 
  cp $RUN_DIR/fort.81 cost_fn
fi

if test -f $RUN_DIR/fort.82; then
  cp $RUN_DIR/fort.82 grad_fn
fi

if test -d $RUN_DIR/trace; then
  mv $RUN_DIR/trace .
fi

cp $RUN_DIR/namelist.* .

mkdir rsl
cp $RUN_DIR/rsl* rsl
if test $NL_USE_HTML = .TRUE.; then
   cd rsl
   for FILE in rsl*; do
     echo "<HTML><HEAD><TITLE>$FILE</TITLE></HEAD>" > $FILE.html
     echo "<H1>$FILE</H1><PRE>" >> $FILE.html
     cat $FILE >> $FILE.html
     echo "</PRE></BODY></HTML>" >> $FILE.html
     rm $FILE
   done
   cd $EXP_DIR
 
   if test -f $RUN_DIR/wrfvar.out; then
     echo "<HTML><HEAD><TITLE>stdout</TITLE></HEAD>" > stdout.html
     echo "<H1>stdout</H1><PRE>" >> stdout.html
     cat $RUN_DIR/wrfvar.out >> stdout.html
     echo "</PRE></BODY></HTML>" >> stdout.html
   fi

   if test -f $RUN_DIR/wrfvar.error; then
     echo "<HTML><HEAD><TITLE>stderr</TITLE></HEAD>" > stderr.html
     echo "<H1>stderr</H1><PRE>" >> stderr.html
     cat $RUN_DIR/wrfvar.error >> stderr.html
     echo "</PRE></BODY></HTML>" >> stderr.html
   fi

   cat >> $HTML << EOF
<A HREF="namelist.input">Namelist input</a>
<A HREF="namelist.output">Namelist output</a>
<A HREF="stdout.html">Stdout</a>
<A HREF="stderr.html">Stderr</a>
<A HREF="rsl/rsl.out.0000.html">rsl.out.0000</a>
<A HREF="rsl/rsl.error.0000.html">rsl.error.0000</a>
<A HREF="rsl">Other RSL output</a>
<A HREF="trace/0.html">PE 0 trace</a>
<A HREF="trace">Other tracing</a>
<A HREF="cost_fn">Cost function</a>
<A HREF="grad_fn">Gradient function</a>
<A HREF="statistics">Statistics</a>
</PRE>
</BODY></HTML>
EOF
fi

if test $NL_USE_HTML = .TRUE.; then
   ls -l $EXP_DIR/cost_fn >> $HTML
   if test $RC = 0; then
     echo "passed on $DATE" >> $HTML
   else
     echo "failed on $DATE with error $RC" >> $HTML
   fi
fi

ls -l $EXP_DIR/cost_fn

if test $RC = 0; then
  echo "passed on $DATE"
else
  echo "failed on $DATE with error $RC"
fi

echo "WRF-Var completed"
date

exit 0
