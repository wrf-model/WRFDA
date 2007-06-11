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

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

export RUN_DIR=$EXP_DIR/run/${DATE}/run_ensmean

#------------------------------------------------------------------------------------------

mkdir -p $RUN_DIR
cd $RUN_DIR

export NEXT_DATE=`${WRFVAR_DIR}/build/da_advance_cymdh.exe $DATE $FCST_RANGE`
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
ln -fs ${WRFVAR_DIR}/build/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

exit 0

