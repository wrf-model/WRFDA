#!/bin/ksh
DIR=`dirname $0`

export NUM_PROCS=${NUM_PROCS:-1}               # Number of processors to run on.
export HOSTS=${HOSTS:-$PWD/hosts}
export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -nolocal -machinefile $HOSTS}

#-----------------------------------------------------------------------
# [1.0] Specify default environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010112}              # Analysis date.
export WINDOW_START=${WINDOW_START:-0}
export WINDOW_END=${WINDOW_END:-6}

#Default directories/files:

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_NL_DIR=${WRF_NL_DIR:-$REL_DIR/wrf_nl}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}

export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXPT=${EXPT:-test}
export DOMAIN=${DOMAIN:-01}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export HOSTS=${HOSTS:-$DAT_DIR/hosts/$HOSTNAME.hosts}

export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DATE/wrfvar}
export WORK_DIR=$RUN_DIR/working

export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export BE_DIR=${BE_DIR:-$REG_DIR/be}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export FC_DIR=${FC_DIR:-$REG_DIR/fc}

# Do we remove the WORK_DIR at the end to save space
export CLEAN=${CLEAN:-false}

export DUMMY=${DUMMY:-false}
export CYCLING=${CYCLING:-false}

if $CYCLING; then
  export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$FC_DIR/$DATE/wrfinput_d$DOMAIN}    # wrfvar "first guess" input.
  export DA_BOUNDARIES=${DA_BOUNDARIES:-$FC_DIR/$DATE/wrfbdy_d$DOMAIN}    # wrfvar boundaries input.
else
  export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$RC_DIR/$DATE/wrfinput_d$DOMAIN}    # wrfvar "first guess" input.
  export DA_BOUNDARIES=${DA_BOUNDARIES:-$RC_DIR/$DATE/wrfbdy_d$DOMAIN}    # wrfvar boundaries input.
fi

export DA_ANALYSIS=${DA_ANALYSIS:-analysis}
export DA_OBSERVATIONS=${DA_OBSERVATIONS:-$OB_DIR/$DATE/ob.ascii} # wrfvar observation input.
export DA_BUFR_DIR=${DA_BUFR_DIR:-$OB_DIR/$DATE} # radiance bufr file directory
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/gen_be.NMC.dat} # wrfvar background errors.
export DA_SSMI=${DA_SSMI:-$OB_DIR/$DATE/ssmi.dat}               # SSM/I radiances (ignore if not using).
export DA_RADAR=${DA_RADAR:-$OB_DIR/$DATE/radar.dat}            # Radar data (ignore if not using).
export ENDIAN=${ENDIAN:-big_endian}

export RTTOV=${RTTOV:-$HOME/rttov/rttov85}                            # RTTOV
export DA_RTTOV_COEFFS=${DA_RTTOV_COEFFS:-$RTTOV/rtcoef_rttov7}

export NL_GLOBAL=${NL_GLOBAL:-false}
export NL_VAR4D=${NL_VAR4D:-false}
export NL_RUN_HOURS=${NL_RUN_HOURS:-6}
export NL_JCDFI_USE=${NL_JCDFI_USE:-false}
export NL_JCDFI_ONOFF=$NL_JCDFI_USE

#=======================================================

mkdir -p $RUN_DIR

echo "<HTML><HEAD><TITLE>$EXPT wrfvar</TITLE></HEAD><BODY><H1>$EXPT wrfvar</H1><PRE>"

date

echo 'REL_DIR               <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR            <A HREF="file:'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>' $WRFVAR_VN
if $NL_VAR4D; then
   echo 'WRF_NL_DIR            <A HREF="file:'$WRF_NL_DIR'">'$WRF_NL_DIR'</a>' $WRF_NL_VN
   echo 'WRFPLUS_DIR           <A HREF="file:'$WRFPLUS_DIR'">'$WRFPLUS_DIR'</a>' $WRFPLUS_VN
fi
echo "DA_FIRST_GUESS        $DA_FIRST_GUESS"
echo "DA_BACK_ERRORS        $DA_BACK_ERRORS"
echo 'OB_DIR                <A HREF="file:'$OB_DIR'">'$OB_DIR'</a>'
echo "DA_ANALYSIS           $DA_ANALYSIS"
echo 'RUN_DIR               <A HREF="'${RUN_DIR##$PWD}'">'$RUN_DIR'</a>'
echo "DATE                  $DATE"
echo "WINDOW_START          $WINDOW_START"
echo "WINDOW_END            $WINDOW_END"

if test ! -f $DA_ANALYSIS; then

   rm -rf ${WORK_DIR}
   mkdir -p ${WORK_DIR}
   cd $WORK_DIR

   START_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe $DATE $WINDOW_START`
   END_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe $DATE $WINDOW_END`

   for INDEX in 01 02 03 04 05 06 07; do
      let H=$INDEX-1+$WINDOW_START
      D_DATE[$INDEX]=`$WRFVAR_DIR/build/advance_cymdh.exe $DATE $H`
      export D_YEAR[$INDEX]=`echo ${D_DATE[$INDEX]} | cut -c1-4`
      export D_MONTH[$INDEX]=`echo ${D_DATE[$INDEX]} | cut -c5-6`
      export D_DAY[$INDEX]=`echo ${D_DATE[$INDEX]} | cut -c7-8`
      export D_HOUR[$INDEX]=`echo ${D_DATE[$INDEX]} | cut -c9-10`
   done

   export NPROC_X=${NPROC_X:-0} # Regional, always set NPROC_X to 0, Global, always 1
   if $NL_GLOBAL; then
      export NPROC_X=1
   fi

   export YEAR=`echo $DATE | cut -c1-4`
   export MONTH=`echo $DATE | cut -c5-6`
   export DAY=`echo $DATE | cut -c7-8`
   export HOUR=`echo $DATE | cut -c9-10`

   export NL_START_YEAR=`echo $START_DATE | cut -c1-4`
   export NL_START_MONTH=`echo $START_DATE | cut -c5-6`
   export NL_START_DAY=`echo $START_DATE | cut -c7-8`
   export NL_START_HOUR=`echo $START_DATE | cut -c9-10`

   export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
   export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
   export NL_END_DAY=`echo $END_DATE | cut -c7-8`
   export NL_END_HOUR=`echo $END_DATE | cut -c9-10`

   export NL_TIME_WINDOW_MIN=${NL_TIME_WINDOW_MIN:-${NL_START_YEAR}-${NL_START_MONTH}-${NL_START_DAY}_${NL_START_HOUR}:00:00.0000}
# JRB are these duplicates?
   export NL_ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00.0000
   export NL_TIME_ANALYSIS=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00.0000
   export NL_TIME_WINDOW_MAX=${NL_TIME_WINDOW_MAX:-${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00.0000}

   # Default WRF namelist variables:
   export NL_OB_FORMAT=${NL_OB_FORMAT:-2}              # Observation format: 1=BUFR, 2=ASCII "little_r"

   #-----------------------------------------------------------------------
   # [2.0] Perform sanity checks:
   #-----------------------------------------------------------------------

   if test ! -r $DA_FIRST_GUESS; then
      echo "${ERR}First Guess file >$DA_FIRST_GUESS< does not exist:$END"
      exit 1
   fi

   if test ! -d $OB_DIR; then
      echo "${ERR}Observation directory >$OB_DIR< does not exist:$END"
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
   ln -s $WRFVAR_DIR/build/wrfvar.exe .
   export PATH=$WRFVAR_DIR/scripts:$PATH

   ln -sf $DA_BOUNDARIES 	 wrfbdy_d$DOMAIN
   ln -sf $DA_FIRST_GUESS	 fg01
   ln -sf $DA_FIRST_GUESS	 wrfinput_d$DOMAIN
   ln -sf $DA_BACK_ERRORS   fort.34
   if test $NL_OB_FORMAT = 1; then
      ln -fs $DA_OBSERVATIONS ob.bufr
   else
      ln -fs $DA_OBSERVATIONS ob01.ascii
   fi

   for FILE in $DAT_DIR/*.inv; do
      if test -f $FILE; then
         ln -fs $FILE .
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
      ln -sf $DA_SSMI ssmi01.dat
      export NL_USE_SSMIRETRIEVALOBS=true
   fi

   if test -r $DA_RADAR; then
      ln -sf $DA_RADAR radar01.dat
      export NL_USE_RADAROBS=true
   fi

   if $NL_VAR4D; then
      # Create nl, tl, ad links structures
      mkdir nl tl ad

      # nl

      # Inputs
      export NL_AUXHIST2_INNAME='auxhist2_d<domain>_<date>'
      export NL_DYN_OPT=2
      export NL_INPUT_OUTNAME='nl_d<domain>_<date>'
      export NL_INPUTOUT_INTERVAL=60
      export NL_AUXHIST2_INTERVAL=10
      export NL_FRAMES_PER_AUXHIST2=1
      export NL_INTERVAL_SECONDS=10800
      export NL_HISTORY_INTERVAL=180
      export NL_RESTART=false
      export NL_FRAMES_PER_OUTFILE=1000
      export NL_INPUT_FROM_FILE=true
      export NL_TIME_STEP=600
      export NL_WRITE_INPUT=true
      export NL_DEBUG_LEVEL=999
      . $WRF_NL_DIR/inc/namelist_script.inc
      export NL_DEBUG_LEVEL=0
      unset NL_AUXHIST2_INNAME
      unset NL_AUXHIST2_INTERVAL
      unset NL_FRAMES_PER_AUXHIST2
      mv namelist.input nl
      ln -fs $WORK_DIR/LANDUSE.TBL nl
      ln -fs $WORK_DIR/wrfbdy_d$DOMAIN nl
      ln -fs $WORK_DIR/fg01 nl/wrfinput_d${DOMAIN}
#      if test -e $WORK_DIR/wrfvar_output; then
#         ln -fs $WORK_DIR/wrfvar_output nl/wrfinput_d$DOMAIN
#      else
         ln -fs $WORK_DIR/fg01 nl/wrfinput_d${DOMAIN}
#      fi
      ln -s $WRF_NL_DIR/main/wrf.exe nl

      # Outputs
      for I in 02 03 04 05 06 07; do
         ln -fs nl/nl_d${DOMAIN}_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00 fg$I
      done

      # tl

      # Inputs

      export NL_DYN_OPT=202
      export NL_INPUT_OUTNAME='tl_d<domain>_<date>'
      export NL_AUXHIST3_OUTNAME='auxhist3_d<domain>_<date>'
      export NL_AUXHIST3_INTERVAL=60
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>'
      export NL_AUXINPUT2_INTERVAL=10
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input tl
      ln -fs $WORK_DIR/LANDUSE.TBL tl
      ln -fs $WORK_DIR/wrfbdy_d$DOMAIN tl
      ln -fs $WORK_DIR/tl01 tl/wrfinput_d${DOMAIN}
      ln -fs $WRFPLUS_DIR/main/wrfplus.exe tl

      # Outputs
      for I in 02 03 04 05 06 07; do
         ln -s tl/tl_d${DOMAIN}_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00 tl$I
      done
      if $NL_JCDFI_USE; then
         ln -s tl/auxhist3_d${DOMAIN}_${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00 tldf
      fi

      # ad

      # Inputs
      export NL_DYN_OPT=302
      export NL_INPUT_OUTNAME='ad_d<domain>_<date>'
      export NL_AUXINPUT2_INNAME='../nl/auxhist2_d<domain>_<date>'
      export NL_AUXINPUT2_INTERVAL=10
      export NL_AUXINPUT3_INNAME='auxinput3_d<domain>_<date>'
      export NL_AUXINPUT3_INTERVAL=60
      export NL_HISTORY_INTERVAL=9999
      export NL_AUXHIST3_INTERVAL=60
      export NL_INPUTOUT_INTERVAL=60
      export NL_INTERVAL_SECONDS=21600
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input ad
      ln -fs $WORK_DIR/LANDUSE.TBL ad
      ln -fs $WORK_DIR/wrfbdy_d$DOMAIN ad
      ln -fs $WORK_DIR/fg01 ad/wrfinput_d${DOMAIN}
      for I in 01 02 03 04 05 06 07; do
         ln -fs $WORK_DIR/af$I ad/auxinput3_d${DOMAIN}_${D_YEAR[$I]}-${D_MONTH[$I]}-${D_DAY[$I]}_${D_HOUR[$I]}:00:00
      done
# JRB
#      if $NL_JCDFI_USE; then
         ln -fs $WORK_DIR/afdf ad/auxinput3_d${DOMAIN}_dfi
#      fi   
      ln -fs $WRFPLUS_DIR/main/wrfplus.exe ad

      # Outputs
      ln -fs ad/ad_d${DOMAIN}_${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00 gr01

      export NL_DYN_OPT=2
   fi

   . $WRFVAR_DIR/inc/namelist_script.inc

   if test -f namelist.input; then
     cp namelist.input $RUN_DIR
   fi

   echo '<A HREF="namelist.input">Namelist input</a>'

   #-------------------------------------------------------------------
   #Run WRF-Var:
   #-------------------------------------------------------------------
   mkdir trace

   if $DUMMY; then
      echo Dummy wrfvar
      echo "Dummy wrfvar" > $DA_ANALYSIS
      RC=0
   else
      if $NL_VAR4D; then
         ln -fs $OB_DIR/${D_DATE[01]}/ob.ascii+ ob01.ascii
         for I in 02 03 04 05 06; do
            ln -fs $OB_DIR/${D_DATE[$I]}/ob.ascii ob${I}.ascii
         done
         ln -fs $OB_DIR/${D_DATE[07]}/ob.ascii- ob07.ascii

         ln -fs $OB_DIR/${D_DATE[01]}/ssmi.dat+ ssmi01.dat
         for I in 02 03 04 05 06; do
            ln -fs $OB_DIR/${D_DATE[$I]}/ssmi.dat ssmi${I}.dat
         done
         ln -fs $OB_DIR/${D_DATE[07]}/ssmi.dat- ssmi07.dat

         ln -fs $OB_DIR/${D_DATE[01]}/radar.dat+ radar01.dat
         for I in 02 03 04 05 06; do
            ln -fs $OB_DIR/${D_DATE[$I]}/radar.dat radar${I}.dat
         done
         ln -fs $OB_DIR/${D_DATE[07]}/radar.dat- radar07.dat

         if $POE && test $NUM_PROCS -gt 1; then
            # JRB kludge until we work out what we are doing here
            export MP_PGMMODEL=mpmd
            export MP_CMDFILE=poe.cmdfile
            if test $NUM_PROCS -lt 3; then
               echo "Need at least 3 processors for 4dvar"
               exit 1
            fi
            NUM_PROCS_VAR=1
            NUM_PROCS_WRF=1
            let NUM_PROCS_WRFPLUS=$NUM_PROCS-$NUM_PROCS_VAR-$NUM_PROCS_WRF
            echo "NUM_PROCS_VAR                $NUM_PROCS_VAR"
            echo "NUM_PROCS_WRF                $NUM_PROCS_WRF"
            echo "NUM_PROCS_WRFPLUS            $NUM_PROCS_WRFPLUS"

            rm -f $MP_CMDFILE
            let I=0
            while test $I -lt $NUM_PROCS_VAR; do
               echo "env BIND_TASKS=no ./wrfvar.exe" >> $MP_CMDFILE
               let I=$I+1
            done
            while test $I -lt $NUM_PROCS_VAR+$NUM_PROCS_WRF; do
               echo "env BIND_TASKS=no ./wrf.exe" >> $MP_CMDFILE
               let I=$I+1
            done
            while test $I -lt $NUM_PROCS; do
               echo "env BIND_TASKS=no ./wrfplus.exe" >> $MP_CMDFILE
               let I=$I+1
            done
            poe
            RC=$?
         else
            $RUN_CMD ./wrfvar.exe
            RC=$?
         fi
      else
         $RUN_CMD ./wrfvar.exe
         RC=$?
      fi

      if test -f fort.9; then
        cp fort.9 $RUN_DIR/namelist.output
      fi

      if test -f fort.12; then
         cp fort.12 $RUN_DIR/statistics
      fi

      if test -f fort.81; then 
         cp fort.81 $RUN_DIR/cost_fn
      fi

      if test -f fort.82; then
         cp fort.82 $RUN_DIR/grad_fn
      fi

      if test -f wrfvar_output; then
         if test $DA_ANALYSIS != wrfvar_output; then 
            mv wrfvar_output $DA_ANALYSIS
         fi
      fi

      if test -d trace; then
         mkdir -p $RUN_DIR/trace
         mv trace/* $RUN_DIR/trace
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
      echo '<A HREF="trace/0.html">PE 0 trace</a>'
      echo '<A HREF="trace">Other tracing</a>'
      echo '<A HREF="cost_fn">Cost function</a>'
      echo '<A HREF="grad_fn">Gradient function</a>'
      echo '<A HREF="statistics">Statistics</a>'

      cat $RUN_DIR/cost_fn

      if test $RC = 0; then
        echo `date` "${OK}Succeeded${END}"
      else
         echo `date` "${ERR}Failed${END} with error $RC"
         exit 1
      fi
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
else
   echo "$DA_ANALYSIS already exists, skipping"
fi

echo '</PRE></BODY></HTML>'

exit $RC
