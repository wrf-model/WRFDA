#!/bin/ksh
#########################################################################
# Script: da_run_wpb.ksh
#
# Purpose: Provide an ensemble of WRF lateral boundary condition (wrfbdy)
# files.
#
# Description:
# 1. Run WPS/real (produces wrfinput_d01 files).
# 2. Run WRF-Var in "randomcv" mode (produces ensemble of perturbed
#    wrfinput_d01 files.
# 3. Loop over 1. and 2. for each time of tendency in wrfbdy file out to
#    forecast length (e.g. 3hourly tendency update in a 72hr forecast).
# 4. Run perturb_wrf_bc to provide perturbed wrfbdy files.
#
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#Experiment details:
export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-6}
export LBC_FREQ=${LBC_FREQ:-06}
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}                            # Domain name. 
export EXPT=${EXPT:-test}                              # Experiment name.
export SOLVER=${SOLVER:-em}
export NUM_PROCS=${NUM_PROCS:-1}
export NUM_MEMBERS=${NUM_MEMBERS:-1}
export NUM_JOBS=${NUM_JOBS:-1}
export HOSTS=${HOSTS:-$HOME/hosts}
if [[ -f $HOSTS ]]; then
   export RUN_CMD=${RUN_CMD:-mpirun -machinefile $HOSTS -np $NUM_PROCS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi
export CLEAN=${CLEAN:-false}
export RUN_GEOGRID=${RUN_GEOGRID:-true}
export RUN_UNGRIB_AFWA=${RUN_UNGRIB_AFWA:-false}
#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRF_DIR=${WRF_DIR:-$REL_DIR/WRFV2}
export WPB_DIR=${WPB_DIR:-$REL_DIR/wpb}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export GRIB_DIR=${GRIB_DIR:-$DAT_DIR/fnl}
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/$DATE/wpb}

#From WPS (namelist.wps):
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}                
export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}
export WPS_GEOG_DIR=${WPS_GEOG_DIR:-~wrfhelp/WPS_GEOG}
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export MAP_PROJ=${MAP_PROJ:-lambert}
export REF_LAT=${REF_LAT:-40.0}
export REF_LON=${REF_LON:--98.0}
export TRUELAT1=${TRUELAT1:-30.0}
export TRUELAT2=${TRUELAT2:-60.0}
export STAND_LON=${STAND_LON:--98.0}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}
export GEOG_DATA_RES=${GEOG_DATA_RES:-30s}
export FG_TYPE=${FG_TYPE:-GFS}

#From WRF real (namelist.input):
export NL_NUM_METGRID_LEVELS=${NL_NUM_METGRID_LEVELS:-27}
export NL_P_TOP_REQUESTED=${NL_P_TOP_REQUESTED:-5000}
export NL_FRAMES_PER_OUTFILE=${NL_FRAMES_PER_OUTFILE:-1}
export NL_HISTORY_INTERVAL=${NL_HISTORY_INTERVAL:-360}          # (minutes)
export NL_TIME_STEP=${NL_TIME_STEP:-360}                # Timestep (s) (dt=4-6*dx(km) recommended).
export NL_ETA_LEVELS=${NL_ETA_LEVELS:-" 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                        " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                        " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                        " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                        " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000"}
export NL_E_VERT=${NL_E_VERT:-28}                   #
export NL_SMOOTH_OPTION=${NL_SMOOTH_OPTION:-1}           # ?
export NL_MP_PHYSICS=${NL_MP_PHYSICS:-3}           #
export NL_RADT=${NL_RADT:-30}                #
export NL_SF_SFCLAY_PHYSICS=${NL_SF_SFCLAY_PHYSICS:-1}
export NL_SF_SURFACE_PHYSICS=${NL_SF_SURFACE_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_NUM_SOIL_LAYERS=${NL_NUM_SOIL_LAYERS:-5}
export NL_BL_PBL_PHYSICS=${NL_BL_PBL_PHYSICS:-1} #(1=Thermal diffusion, 2=Noah LSM).
export NL_CU_PHYSICS=${NL_CU_PHYSICS:-1}           #(1=, 2=,3=).
export NL_CUDT=${NL_CUDT:-5}           #(1=, 2=,3=).
export NL_W_DAMPING=${NL_W_DAMPING:-0}            #
export NL_DIFF_OPT=${NL_DIFF_OPT:-0}             #
export NL_DAMPCOEF=${NL_DAMPCOEF:-0.2}
export NL_TIME_STEP_SOUND=${NL_TIME_STEP_SOUND:-6}    #
export NL_SPECIFIED=${NL_SPECIFIED:-.true.}          #

#From WRF (namelist.input):
#&time_control:
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-.true.}
export NL_INPUT_OUTNAME=${NL_INPUT_OUTNAME:-'wrf_3dvar_input_d<domain>_<date>'}
export NL_INPUTOUT_INTERVAL=$NL_HISTORY_INTERVAL # Write wrfinput files at same freq. as output.
#&physics:
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}
export NL_PD_MOIST=${NL_PD_MOIST:-.false.}

#From WRF-Var:
export NL_VAR4D=${NL_VAR4D:-false}
export BE_DIR=${BE_DIR:-$REG_DIR/be}     # Background error covariance directory.
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # background errors.
export NL_OB_FORMAT=${NL_OB_FORMAT:-2} # Observation format: 1=BUFR, 2=ASCII "little_r"

#------------------------------------------------------------------------------------------

#Save these, as they are overwritten locally:
export DATE_SAVE=$DATE
export END_DATE_SAVE=$END_DATE
export FCST_RANGE_SAVE=$FCST_RANGE
export NL_RUN_HOURS_SAVE=$NL_RUN_HOURS
export RC_DIR_SAVE=$RC_DIR
export NL_ANALYSIS_TYPE_SAVE=$NL_ANALYSIS_TYPE
export RUN_DIR_SAVE=$RUN_DIR

#These are the local values:
export END_DATE=$($WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null)
export FCST_RANGE=0
export NL_RUN_HOURS=0
export RC_DIR=$RUN_DIR_SAVE/rc
export NL_ANALYSIS_TYPE="randomcv"
mkdir -p $RC_DIR

if [[ -f ${RC_DIR_SAVE}/geo_em.d01.nc ]]; then
   cp ${RC_DIR_SAVE}/geo_em.d01.nc ${RC_DIR}
   export RUN_GEOGRID=false
fi

while [[ $DATE -le $END_DATE ]]; do 
   echo "Producing wrfinput files for $DATE"

#  Run WPS:
   export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/wps/$DATE
   mkdir -p $RUN_DIR

#   $WRFVAR_DIR/scripts/da_trace.ksh da_run_wps $RUN_DIR >&! /dev/null
   ${WRFVAR_DIR}/scripts/da_run_wps.ksh > $RUN_DIR/index.html 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo $(date) "${ERR}Failed with error $RC$END"
      exit 1
   fi
   export RUN_GEOGRID=false # Only need to run it once.

#  Run real:
   export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/real/$DATE
   mkdir -p $RUN_DIR

#   $WRFVAR_DIR/scripts/da_trace.ksh da_run_real $RUN_DIR >&! /dev/null
   ${WRFVAR_DIR}/scripts/da_run_real.ksh > $RUN_DIR/index.html 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo $(date) "${ERR}Failed with error $RC$END"
      exit 1
   fi

   export NEXT_DATE=$($WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $LBC_FREQ 2>/dev/null)
   export DATE=$NEXT_DATE

done
echo ""

export FCST_RANGE=$FCST_RANGE_SAVE
export NL_ANALYSIS_TYPE=randomcv
export NL_PUT_RAND_SEED=.TRUE.

#Overwrite originals:
export DATE=$DATE_SAVE
export RC_DIR=$RC_DIR_SAVE
export RUN_DIR=$RUN_DIR_SAVE

let MEM=1
let JOB=1

while [[ $MEM -le $NUM_MEMBERS ]]; do 
   echo "Producing perturbed wrfbdy files for ensemble member $MEM"
   ${WRFVAR_DIR}/scripts/da_perturb_wrf_bc.ksh > ${RUN_DIR}/da_perturb_wrf_bc.${MEM}.out 2>&1 &

   let MEM=$MEM+1
   let JOB=$JOB+1

   if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
   sleep 1
done

export END_DATE=$END_DATE_SAVE
export FCST_RANGE=$FCST_RANGE_SAVE
export NL_RUN_HOURS=$NL_RUN_HOURS_SAVE
export NL_ANALYSIS_TYPE=$NL_ANALYSIS_TYPE_SAVE
export RUN_DIR=$RUN_DIR_SAVE

exit 0

