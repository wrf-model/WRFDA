#!/bin/ksh
#########################################################################
# Script: da_run_ensmean.ksh
#
# Purpose: Calculate mean of ensemble of WRF forecasts.
#
# Description:
# WRF forecasts must be in wrfinput format.
#
#########################################################################

#export FCST_RANGE=12
#export EXPT=noda
#export NUM_MEMBERS=30
#export RUN_CMD=" "
#export REL_DIR=/smoke/dmbarker/code/trunk
#export DAT_DIR=/smoke/dmbarker/data
#export FILE_TYPE=wrf_3dvar_input

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#Experiment details:
export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-6}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}                              # Experiment name.
export NUM_MEMBERS=${NUM_MEMBERS:-1}
export HOSTS=${HOSTS:-$HOME/hosts}
export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -nolocal -machinefile $HOSTS}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export BUILD_DIR=${BUILD_DIR:-$WRFVAR_DIR/build}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/${DATE}/run_ensmean}

#Ensemble mean parameters:
export FILE_TYPE=${FILE_TYPE:-wrf_3dvar_input}
export NV=${NV:-15}                               # Number of variables to average.
export CV=${CV:-"'U'", "'V'", "'W'", "'PH'", "'T'", "'MU'", "'TSLB'", "'TSK'", \
                "'QCLOUD'", "'QRAIN'", "'QVAPOR'", "'U10'", "'V10'", "'T2'", "'Q2'"} # Variable names

#------------------------------------------------------------------------------------------

mkdir -p $RUN_DIR
cd $RUN_DIR

export NEXT_DATE=`${BUILD_DIR}/advance_cymdh.exe $DATE $FCST_RANGE`
export YYYY=`echo $NEXT_DATE | cut -c1-4`
export MM=`echo $NEXT_DATE | cut -c5-6`
export DD=`echo $NEXT_DATE | cut -c7-8`
export HH=`echo $NEXT_DATE | cut -c9-10`
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
export DA_FILE=${FC_DIR}/${DATE}/${FILE_TYPE}_d01_${FILE_DATE}

#Copy first member as template for mean:
cp ${DA_FILE}.e001 ${DA_FILE}

cat > gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    filestub = '${DA_FILE}'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV} /
EOF

#Run:
cp ${BUILD_DIR}/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

exit 0

