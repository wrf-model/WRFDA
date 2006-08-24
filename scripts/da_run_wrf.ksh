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
export SOLVER=${SOLVER:-EM}
export NUM_PROCS=${NUM_PROCS:-1}                       # Number of processors for WRF-Var/WRF.
export DUMMY=${DUMMY:-false}
export HOSTS=${HOSTS:-$PWD/hosts}
export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -nolocal -machinefile $HOSTS}

export EXPT=${EXPT:-test}                             # Experiment name.
export HOSTNAME=${HOSTNAME:-`hostname`}

# Directories:
export TMP_DIR=${TMP_DIR:-/var/tmp/$USER}  # Temporary directory.
export DAT_DIR=${DAT_DIR:-$HOME/data} # Data directory.
export HOSTS=${HOSTS:-$DAT_DIR/hosts/$HOSTNAME.hosts}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} # Data directory for region.
export RUN_DIR=${RUN_DIR:-$DAT_DIR/$REGION/$EXPT} #Run directory.
export REL_DIR=${REL_DIR:-$HOME/trunk} # Code directory.
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}                  # Code subdirectory.


# Additional Namelist variable for real (namelist.input):

export NL_RUN_HOURS=${NL_RUN_HOURS:-6}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_TIME_STEP=${NLTIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_E_VERT=${NL_E_VERT:-28}                   # 
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           # 
export NL_RADT=${NL_RADT:-30}                # 
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-0}          # 
export NL_W_DAMPING=${NL_W_DAMPING:-0}            # 
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             # 
export NL_KM_OPT=${NL_KM_OPT:-1}               # 
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}           # 
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-4}    # 

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT wrf</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT wrf</H1><PRE>"
fi

date

# Create temporary directory:
export TMP_DIR=${RUN_DIR}/working
rm -rf ${TMP_DIR}
mkdir -p ${TMP_DIR}
cd ${TMP_DIR}

# Copy necessary info (better than link as not overwritten):
cp ${WRF_DIR}/main/wrf.exe .
cp ${WRF_DIR}/run/RRTM_DATA .
cp ${WRF_DIR}/run/GENPARM.TBL .
cp ${WRF_DIR}/run/LANDUSE.TBL .

cp ${WRF_DIR}/run/SOILPARM.TBL .
cp ${WRF_DIR}/run/VEGPARM.TBL .
cp ${WRF_DIR}/run/gribmap.txt .
cp ${DA_ANALYSIS} wrfinput_d${DOMAIN}
cp ${CS_DIR}/$DATE/wrfbdy_d${DOMAIN} wrfbdy_d${DOMAIN}
cp ${CS_DIR}/$DATE/wrflowinp_d${DOMAIN} wrflowinp_d${DOMAIN}

export CCYY=`echo $DATE | cut -c1-4`
export MM=`echo $DATE | cut -c5-6`
export DD=`echo $DATE | cut -c7-8`
export HH=`echo $DATE | cut -c9-10`

export NL_START_YEAR=$CCYY
export NL_START_MONTH=$MM
export NL_START_DAY=$DD
export NL_START_HOUR=$HH

export NL_ANALYSIS_DATE=${CCYY}-${MM}-${DD}_${HH}:00:00.0000

export END_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe $DATE $FCST_RANGE`

export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
export NL_END_DAY=`echo $END_DATE | cut -c7-8`
export NL_END_HOUR=`echo $END_DATE | cut -c9-10`

. $WRF_DIR/inc/namelist_script.inc

if $DUMMY; then
   echo Dummy wrf
   echo Dummy wrf > wrfout_d${DOMAIN}_${CCYY}-${MM}-${DD}_${HH}:00:00
else
   $RUN_CMD ./wrf.exe
fi

if test -f namelist.input; then
  cp namelist.input $OUT_DIR
fi

if test -f fort.9; then
  cp fort.9 $OUT_DIR/namelist.output
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
fi


mv wrfvar_input* ../.
mv wrfout* ../.
rm ../wrfout_d${DOMAIN}_${CCYY}-${MM}-${DD}_${HH}:00:00
mv namelist.input ../.

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi

exit 0

