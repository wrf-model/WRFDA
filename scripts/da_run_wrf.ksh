#!/bin/ksh
#########################################################################
# Script: da_run_wrf.ksh
#
# Purpose: Run WRF system.
#########################################################################

#set echo

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

export DATE=${DATE:-2003010100}            # Start date of test period
export FCST_RANGE=${FCST_RANGE:-06} # Length of forecasts to run at FCST_TIMES.
export LBC_FREQ=${LBC_FREQ:-06}                        # Boundary condition frequency.
export DOMAIN=${DOMAIN:-01}                            # Domain name. 
export REGION=${REGION:-con200}                        # Region name. 
export SOLVER=${SOLVER:-em}
export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors for WRF-Var/WRF.
export DUMMY=${DUMMY:-false}
export HOSTS=${HOSTS:-$PWD/hosts}
export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -nolocal -machinefile $HOSTS}

export EXPT=${EXPT:-test}                             # Experiment name.
export HOSTNAME=${HOSTNAME:-`hostname`}

# Directories:
export DAT_DIR=${DAT_DIR:-$HOME/data} # Data directory.
export HOSTS=${HOSTS:-$DAT_DIR/hosts/$HOSTNAME.hosts}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} # Data directory for region.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DATE/wrf} #Run directory.
export WORK_DIR=$RUN_DIR/working  # Temporary directory.
export REL_DIR=${REL_DIR:-$HOME/trunk} # Code directory.
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}                  # Code subdirectory.

export RC_DIR=${RC_DIR:-$REG_DIR/rc}

export WRF_INPUT=${WRF_INPUT:-$RC_DIR/$DATE/wrfinput_d${DOMAIN}}
export WRF_BDY=${WRF_BDY:-$RC_DIR/$DATE/wrfbdy_d${DOMAIN}}


# Additional Namelist variable for real (namelist.input):

# Note things DIFFERENT from Registry.EM
export NL_RUN_HOURS=${NL_RUN_HOURS:-$CYCLE_PERIOD}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_E_VERT=${NL_E_VERT:-28}                   # 
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           # 
export NL_RADT=${NL_RADT:-30}                # 
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_NUM_SOIL_LAYERS=${NL_NUM_SOIL_LAYERS:-5} # goes with surface_physics

export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             # 
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}           # 
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-4}    # 

echo "<HTML><HEAD><TITLE>$EXPT wrf</TITLE></HEAD><BODY>"
echo "<H1>$EXPT wrf</H1><PRE>"

date

export D1=`$WRFVAR_DIR/build/advance_cymdh.exe ${DATE} $WINDOW_START 2>/dev/null`
let OFFSET=$FCST_RANGE+$WINDOW_START 
. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh $D1 $OFFSET

echo 'REL_DIR    <A HREF="file:'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_DIR    <A HREF="file:'$WRF_DIR'">'$WRF_DIR'</a>' $WRF_VN
echo 'FC_DIR     <A HREF="file:'$FC_DIR'">'$FC_DIR'</a>'
echo "WRF_INPUT  $WRF_INPUT"
echo "WRF_BDY    $WRF_BDY"
echo "FCST_RANGE $FCST_RANGE"
echo "LBC_FREQ   $LBC_FREQ"
echo "DATE       $DATE"
echo "START_DATE $START_DATE"
echo "END_DATE   $END_DATE"

# Create temporary directory:
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}

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

export YEAR=`echo $DATE | cut -c1-4`
export MONTH=`echo $DATE | cut -c5-6`
export DAY=`echo $DATE | cut -c7-8`
export HOUR=`echo $DATE | cut -c9-10`

export NL_ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00.0000

export NL_INTERVAL_SECONDS=`expr $LBC_FREQ \* 3600`

if test $WRF_NAMELIST'.' != '.'; then
   ln -fs $WRF_NAMELIST namelist.input
elif test -f $WRF_DIR/inc/namelist_script.inc; then
   rm namelist.input
   . $WRF_DIR/inc/namelist_script.inc
else
   ln -fs $WRF_DIR/test/em_real/namelist.input .
fi

cp namelist.input $RUN_DIR

echo 'RUN_DIR               <A HREF="'${RUN_DIR##$PWD}'">'$RUN_DIR'</a>'
echo '<A HREF="namelist.input">Namelist input</a>'

if test ! -f $FC_DIR/$DATE/wrfout_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00; then

   if $DUMMY; then
      echo Dummy wrf
      LOCAL_DATE=$DATE
      while test $LOCAL_DATE -le $END_DATE; do
         export L_YEAR=`echo $LOCAL_DATE | cut -c1-4`
         export L_MONTH=`echo $LOCAL_DATE | cut -c5-6`
         export L_DAY=`echo $LOCAL_DATE | cut -c7-8`
         export L_HOUR=`echo $LOCAL_DATE | cut -c9-10`
         echo Dummy wrf > wrfout_d${DOMAIN}_${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00
         LOCAL_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe $LOCAL_DATE $LBC_FREQ`
      done
   else
      $RUN_CMD ./wrf.exe
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
      cd $WORK_DIR

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
   mv wrfinput* $FC_DIR/$DATE
else
   echo "$FC_DIR/$DATE/wrfinput_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00 already exists, skipping"
fi

mkdir -p $FC_DIR/$END_DATE
ln -fs $FC_DIR/$DATE/wrfinput_d${DOMAIN}_${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00 \
   $FC_DIR/$END_DATE/wrfinput_d${DOMAIN}


if $CLEAN; then
   rm -rf $WORK_DIR
fi

date

echo "</BODY></HTML>"

exit 0

