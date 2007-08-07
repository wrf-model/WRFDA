#!/bin/ksh
#########################################################################
# Script: da_perturb_wrf_bc.ksh
#
# Purpose: Produce a perturbed WRF lateral boundary condition (wrfbdy) file.
#
# Description:
# 1. Run WRF-Var in "randomcv" mode (produces ensemble of perturbed
#    wrfinput_d01 files.
# 2. Loop over 1. and 2. for each time of tendency in wrfbdy file out to
#    forecast length (e.g. 3hourly tendency update in a 72hr forecast).
# 3. Run perturb_wrf_bc to provide perturbed wrfbdy file.
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
export NUM_PROCS=${NUM_PROCS:-1}
export MEM=${MEM:-1}
export HOSTS=${HOSTS:-$HOME/hosts}
if [[ -f $HOSTS ]]; then
   export RUN_CMD=${RUN_CMD:-mpirun -machinefile $HOSTS -np $NUM_PROCS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi
export CLEAN=${CLEAN:-false}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRF_DIR=${WRF_DIR:-$REL_DIR/WRFV2}
export WPB_DIR=${WPB_DIR:-$REL_DIR/wpb}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/$DATE/wpb}

#From WPS (namelist.wps):
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}

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
export NL_SPECIFIED=${NL_SPECIFIED:-true}          #

#From WRF (namelist.input):
#&time_control:
export NL_WRITE_INPUT=${NL_WRITE_INPUT:-true}
export NL_INPUT_OUTNAME=${NL_INPUT_OUTNAME:-'wrf_3dvar_input_d<domain>_<date>'}
export NL_INPUTOUT_INTERVAL=$NL_HISTORY_INTERVAL # Write wrfinput files at same freq. as output.
#&physics:
export NL_RA_LW_PHYSICS=${NL_RA_LW_PHYSICS:-1}
export NL_RA_SW_PHYSICS=${NL_RA_SW_PHYSICS:-1}
export NL_MP_ZERO_OUT=${NL_MP_ZERO_OUT:-2}

#From WRF-Var:
export NL_VAR4D=${NL_VAR4D:-false}
export BE_DIR=${BE_DIR:-$REG_DIR/be}     # Background error covariance directory.
export DA_BACK_ERRORS=${DA_BACK_ERRORS:-$BE_DIR/be.dat} # background errors.
export NL_OB_FORMAT=${NL_OB_FORMAT:-2} # Observation format: 1=BUFR, 2=ASCII "little_r"

#------------------------------------------------------------------------------------------

export DATE_SAVE=$DATE
export RC_DIR_SAVE=$RC_DIR
export NL_ANALYSIS_TYPE_SAVE=$NL_ANALYSIS_TYPE
export CYCLING_SAVE=$CYCLING
export RUN_DIR_SAVE=$RUN_DIR
 
#These are the local values:
export END_DATE=$($WRFVAR_DIR/build/da_advance_time.exe $DATE $LBC_FREQ 2>/dev/null)
export RC_DIR=$RUN_DIR_SAVE/rc
mkdir -p $RC_DIR

export NL_ANALYSIS_TYPE="randomcv"
export NL_PUT_RAND_SEED=.TRUE.
export CYCLING=false

export CMEM=e$MEM
if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi

while [[ $DATE -le $END_DATE ]]; do 
   export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/wrfvar/${DATE}.${CMEM}
   mkdir -p $RUN_DIR

   export YEAR=$(echo $DATE | cut -c1-4)
   export MONTH=$(echo $DATE | cut -c5-6)
   export DAY=$(echo $DATE | cut -c7-8)
   export HOUR=$(echo $DATE | cut -c9-10)
   export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
   export NL_SEED_ARRAY1=$DATE_SAVE
   export NL_SEED_ARRAY2=$(expr $DATE + $MEM)

   echo "   Run WRF-Var in randomcv mode for date $DATE"
   export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d${DOMAIN}
   export DA_ANALYSIS=${RC_DIR}/$DATE/wrfinput_d${DOMAIN}.${CMEM}
#   $WRFVAR_DIR/scripts/da_trace.ksh da_run_wrfvar $RUN_DIR >&! /dev/null
   ${WRFVAR_DIR}/scripts/da_run_wrfvar.ksh > $RUN_DIR/index.html 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo $(date) "${ERR}Failed with error $RC$END"
      exit 1
   fi

   export NEXT_DATE=$($WRFVAR_DIR/build/da_advance_time.exe $DATE $LBC_FREQ 2>/dev/null)
   export DATE=$NEXT_DATE
done

echo "   Run pert_wrf_bc to create perturbed wrfbdy file for member $MEM"
export RUN_DIR=$RUN_DIR_SAVE/run/$DATE_SAVE/pert_wrf_bc/${DATE_SAVE}.${CMEM}
mkdir -p $RUN_DIR
cd $RUN_DIR

cp ${RC_DIR_SAVE}/${DATE_SAVE}/wrfbdy_d01 wrfbdy_this
ln -fs ${RC_DIR}/${DATE_SAVE}/wrfinput_d01.${CMEM} wrfinput_this
ln -fs ${RC_DIR}/${END_DATE}/wrfinput_d01.${CMEM} wrfinput_next
ln -fs ${WPB_DIR}/input.nml .
ln -fs ${WPB_DIR}/namelist.input .
ln -fs ${WPB_DIR}/pert_wrf_bc.mac pert_wrf_bc.exe
./pert_wrf_bc.exe > pert_wrf_bc.out.${CMEM} 2>&1

mv wrfbdy_this ${RC_DIR_SAVE}/${DATE_SAVE}/wrfbdy_d01.${CMEM}
mv ${RC_DIR}/${DATE_SAVE}/wrfinput_d01.${CMEM} ${RC_DIR_SAVE}/${DATE_SAVE}

export END_DATE=$END_DATE_SAVE
export RC_DIR=$RC_DIR_SAVE
export NL_ANALYSIS_TYPE=$NL_ANALYSIS_TYPE_SAVE
export RUN_DIR=$RUN_DIR_SAVE

exit 0

