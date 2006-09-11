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
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}

export CHECK_SVNVERSION=${CHECK_SVNVERSION:-true}

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

export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DATE/wrfvar}
export WORK_DIR=$RUN_DIR/working

export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export BE_DIR=${BE_DIR:-$REG_DIR/be}
export MD_DIR=${MD_DIR:-$REG_DIR/md}

# Do we remove the WORK_DIR at the end to save space
export CLEAN=${CLEAN:-false}

export DUMMY=${DUMMY:-false}

export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$MD_DIR/$DATE/wrfinput_d$DOMAIN}    # wrfvar "first guess" input.
export DA_BOUNDARIES=${DA_BOUNDARIES:-$MD_DIR/$DATE/wrfbdy_d$DOMAIN}    # wrfvar boundaries input.
export DA_ANALYSIS=${DA_ANALYSIS:-wrfvar_output}
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
export NL_USE_HTML=${NL_USE_HTML:-false}
export NL_JCDFI_USE=${NL_JCDFI_USE:-false}

#=======================================================

mkdir -p $RUN_DIR $MD_DIR/$DATE

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
echo "Observation Directory:       $OB_DIR"
echo "Analysis:                    $DA_ANALYSIS"
echo "Run directory:               $RUN_DIR"
echo "Working directory:           $WORK_DIR"
echo "Analysis date:               $DATE"
echo "Time window start:           $WINDOW_START"
echo "Time window end:             $WINDOW_END"

if test ! -f $DA_ANALYSIS; then

   rm -rf ${WORK_DIR}
   mkdir -p ${WORK_DIR}
   cd $WORK_DIR

   START_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $WINDOW_START`
   END_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $WINDOW_END`

   for HOUR in 01 02 03 04 05 06 07; do
      let H=$HOUR-1+$WINDOW_START
      FG_DATE[$HOUR]=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $H`
      export FG_CCYY[$HOUR]=`echo ${FG_DATE[$HOUR]} | cut -c1-4`
      export FG_MM[$HOUR]=`echo ${FG_DATE[$HOUR]} | cut -c5-6`
      export FG_DD[$HOUR]=`echo ${FG_DATE[$HOUR]} | cut -c7-8`
      export FG_HH[$HOUR]=`echo ${FG_DATE[$HOUR]} | cut -c9-10`
   done

   for HOUR in 00 01 02 03 04 05 06; do
      let H=$HOUR+$WINDOW_START
      OB_DATE[$HOUR]=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $H`
      export OB_CCYY[$HOUR]=`echo ${OB_DATE[$HOUR]} | cut -c1-4`
      export OB_MM[$HOUR]=`echo ${OB_DATE[$HOUR]} | cut -c5-6`
      export OB_DD[$HOUR]=`echo ${OB_DATE[$HOUR]} | cut -c7-8`
      export OB_HH[$HOUR]=`echo ${OB_DATE[$HOUR]} | cut -c9-10`
   done

   export NPROC_X=${NPROC_X:-0} # Regional, always set NPROC_X to 0, Global, always 1
   if $NL_GLOBAL; then
      export NPROC_X=1
   fi

   export CCYY=`echo $DATE | cut -c1-4`
   export MM=`echo $DATE | cut -c5-6`
   export DD=`echo $DATE | cut -c7-8`
   export HH=`echo $DATE | cut -c9-10`

   export NL_ANALYSIS_DATE=${CCYY}-${MM}-${DD}_${HH}:00:00.0000

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
   ln -s $WRFVAR_DIR/main/wrfvar.exe .
   export PATH=$WRFVAR_DIR/scripts:$PATH

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
      ln -sf $DA_SSMI ssmi.dat
      export NL_USE_SSMIRETRIEVALOBS=true
   fi

   if test -r $DA_RADAR; then
      ln -sf $DA_RADAR radar.dat
      export NL_USE_RADAROBS=true
   fi

   export NL_START_YEAR=`echo $START_DATE | cut -c1-4`
   export NL_START_MONTH=`echo $START_DATE | cut -c5-6`
   export NL_START_DAY=`echo $START_DATE | cut -c7-8`
   export NL_START_HOUR=`echo $START_DATE | cut -c9-10`
   export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
   export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
   export NL_END_DAY=`echo $END_DATE | cut -c7-8`
   export NL_END_HOUR=`echo $END_DATE | cut -c9-10`

   if $NL_VAR4D; then
      # Create nl, tl, ad links structures
      mkdir nl tl ad

      # nl

      # Inputs
      export NL_AUXHIST2_INNAME='auxhist2_d<domain>_<date>'
      export NL_DYN_OPT=2
      export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
      . $WRF_DIR/inc/namelist_script.inc
      mv namelist.input nl
      ln -fs $WORK_DIR/LANDUSE.TBL nl
      ln -fs $WORK_DIR/wrfbdy_d$DOMAIN nl
      if test -e $WORD_DIR/wrfvar_output; then
         ln -fs $WORK_DIR/wrfvar_output nl/wrfinput_d$DOMAIN
      else
         ln -fs $WORK_DIR/wrfvar_input nl/wrfinput_d$DOMAIN
      fi
      ln -s $WRF_DIR/main/wrf.exe nl

      # Outputs
      for HOUR in 01 02 03 04 05 06; do
         ln -s nl/wrfvar_input_${FG_CCYY[$HOUR]}_${FG_MM[$HOUR]}_${FG_DD[$HOUR]}_${FG_HH[$HOUR]}:00.00 fgat_fg.$HOUR
      done

      # ad

      # Inputs
      export NL_DYN_OPT=202
      export NL_INPUT_OUTNAME='wrfvar_input_d<domain>_<date>'
      export NL_AUXINPUT3_INNAME='auxinput3_d<domain>_<date>'
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input ad
      ln -fs $WORK_DIR/LANDUSE.TBL ad
      for HOUR in 01 02 03 04 05 06; do
         ln -fs $WORK_DIR/af$HOUR ad/auxinput3_d${DOMAIN}_${FG_CCYY[$HOUR]}_${FG_MM[$HOUR]}_${FG_DD[$HOUR]}_${FG_HH[$HOUR]}:00.00
      done
      if $NL_JCDFI_USE; then
         ln -fs $WORK_DIR/afdf ad/auxinput3_d${DOMAIN}_dfi
      fi   
      ln -fs $WRFPLUS_DIR/main/wrfplus.exe ad

      # Outputs
      export CCYY=`echo $DATE | cut -c1-4`
      export MM=`echo $DATE | cut -c5-6`
      export DD=`echo $DATE | cut -c7-8`
      export HH=`echo $DATE | cut -c9-10`
      ln -fs ad/wrfvar_input_d${DOMAIN}_${CCYY}_${MM}_${DD}_${HH}:00.00 gr00

      # tl

      # Inputs

      export NL_DYN_OPT=302
      export NL_INPUT_OUTNAME='tl<date>'
      export NL_AUXHIST3_OUTNAME='auxhist3_d<domain>_<date>'
      . $WRFPLUS_DIR/inc/namelist_script.inc
      mv namelist.input tl
      ln -fs $WORK_DIR/LANDUSE.TBL tl
      ln -fs $WORK_DIR/tl00 tl/wrfinput_d${DOMAIN}
      ln -fs $WORK_DIR/wrfvar_input tl/wrfinput_d${DOMAIN}
      ln -fs $WRFPLUS_DIR/main/wrfplus.exe tl

      # Outputs
      for HOUR in 01 02 03 04 05 06; do
         ln -s tl/tl${FG_CCYY[$HOUR]}_${FG_MM[$HOUR]}_${FG_DD[$HOUR]}_${FG_HH[$HOUR]}:00.00 tl$HOUR
      done
      if $NL_JCDFI_USE; then
         export CCYY=`echo $END_DATE | cut -c1-4`
         export MM=`echo $END_DATE | cut -c5-6`
         export DD=`echo $END_DATE | cut -c7-8`
         export HH=`echo $END_DATE | cut -c9-10`
         ln -s tl/auxhist3_d${DOMAIN}_${CCYY}_${MM}_${DD}_${HH}:00.00 tldf
      fi

      export NL_DYN_OPT=2
   fi

   . $WRFVAR_DIR/inc/namelist_script.inc

   if test -f namelist.input; then
     cp namelist.input $RUN_DIR
   fi

   if $NL_USE_HTML; then
      echo '<A HREF="working">Working directory</a>'
      echo '<A HREF="namelist.input">Namelist input</a>'
   fi

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
      if $NL_VAR4D; then
         ln -s $OB_DIR/${OB_DATE[00]}/ob.ascii+ ob.00
         for HOUR in 01 02 03 04 05; do
            ln -s $OB_DIR/${OB_DATE[$HOUR]}/ob.ascii ob.$HOUR
         done
         ln -s $OB_DIR/${OB_DATE[06]}/ob.ascii- ob.06

         ln -s $OB_DIR/${OB_DATE[00]}/ssmi.dat+ ssmi.00
         for HOUR in 01 02 03 04 05; do
            ln -s $OB_DIR/${OB_DATE[$HOUR]}/ssmi.dat ssmi.$HOUR
         done
         ln -s $OB_DIR/${OB_DATE[06]}/ssmi.dat- ssmi.06

         ln -s $OB_DIR/${OB_DATE[00]}/radar.dat+ radar.00
         for HOUR in 01 02 03 04 05; do
            ln -s $OB_DIR/${OB_DATE[$HOUR]}/radar.dat radar.$HOUR
         done
         ln -s $OB_DIR/${OB_DATE[06]}/radar.dat- radar.06

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

   mkdir -p $RUN_DIR/rsl
   mv rsl* $RUN_DIR/rsl
   if $NL_USE_HTML; then
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
   fi

   cat $RUN_DIR/cost_fn

   if test $RC = 0; then
     echo `date` "${OK}Succeeded${END}"
   else
      echo `date` "${ERR}Failed${END} with error $RC"
      exit 1
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

if $NL_USE_HTML; then
   echo '</PRE></BODY></HTML>'
fi

exit $RC
