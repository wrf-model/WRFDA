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

export DATE=${DATE:-2003010100}                        # Time of analysis (unchanged on exit).
export FCST_RANGE=${FCST_RANGE:-6}
export LBC_FREQ=${LBC_FREQ:-06}
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}
export EXPT=${EXPT:-test}
export SOLVER=${SOLVER:-em}
export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors for WRF-Var/WRF.
export HOSTS=${HOSTS:-${HOME}/hosts}
if test -f $HOSTS; then
   export RUN_CMD=${RUN_CMD:-mpirun -machinefile $HOSTS -np $NUM_PROCS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi
export CLEAN=${CLEAN:-false}

# Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/$DATE/wrf}
export WORK_DIR=$RUN_DIR/working

#From WPS (namelist.wps):
export NL_E_WE=${NL_E_WE:-45}                          #
export NL_E_SN=${NL_E_SN:-45}                          #
export NL_DX=${NL_DX:-200000}                # Resolution (m).
export NL_DY=${NL_DY:-200000}                # Resolution (m).

#From WRF (namelist.input):
#&time_control:
export NL_RUN_HOURS=${NL_RUN_HOURS:-$FCST_RANGE}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_FRAMES_PER_OUTFILE=${NL_FRAMES_PER_OUTFILE:-1}
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-.true.} 
export NL_INPUT_FROM_FILE=${NL_INPUT_FROM_FILE:-.true.}
export NL_INPUT_OUTNAME=${NL_INPUT_OUTNAME:-'wrf_3dvar_input_d<domain>_<date>'}
#export NL_INPUTOUT_INTERVAL=$NL_HISTORY_INTERVAL # Write wrfinput files at same freq. as output.
export NL_INPUTOUT_INTERVAL=${NL_INPUTOUT_INTERVAL:-360}
export NL_INPUTOUT_BEGIN_H=${NL_INPUTOUT_BEGIN_H:-$CYCLE_PERIOD} # Output input format start.
export NL_INPUTOUT_END_H=${NL_INPUTOUT_END_H:-$FCST_RANGE}       # Output input format end.
#&domains:
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_E_VERT=${NL_E_VERT:-28}                   #
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
#&physics:
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           #
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_RADT=${NL_RADT:-30}                # 
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1}
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_CUDT=${NL_CUDT:-5}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}
#&dynamics:
export NL_W_DAMPING=${NL_W_DAMPING:-0}            # 
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             # 
export NL_KM_OPT=${NL_KM_OPT:-1}               # 
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-6}    # 
#&bdy_control:
export NL_SPECIFIED=${NL_SPECIFIED:-.true.}          #

#For WRF:
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

cp $WRFVAR_DIR/inc/namelist_script_wrf_wps_2004.inc $WRF_DIR/inc/namelist_script.inc
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

