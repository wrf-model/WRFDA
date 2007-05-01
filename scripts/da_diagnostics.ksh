#!/bin/ksh
#-------------------------------------------------------------------------
#  Script for getting initial diagnostics for 
#  Observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#-------------------------------------------------------------------------

echo ""
echo "Running da_diagnostics.ksh"
echo ""

export REL_DIR=${REL_DIR:-$HOME/code/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export DAT_DIR=${DAT_DIR:-$HOME/data}
export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export EXPT=${EXPT:-test}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export WORK_DIR=${WORK_DIR:-$REG_DIR/diagnostics}

export START_DATE=${START_DATE:-2006100106}
export END_DATE=${END_DATE:-2006100312}
export CYCLE_PERIOD=${CYCLE_PERIOD:-6}

echo "WRFVAR_DIR    = $WRFVAR_DIR"
echo "EXP_DIR       = $EXP_DIR"
echo "WORK_DIR      = $WORK_DIR"
echo "START_DATE    = $START_DATE"
echo "END_DATE      = $END_DATE"

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

ln -fs ${WRFVAR_DIR}/build/da_diagnostics.exe .
ln -fs ${WRFVAR_DIR}/build/da_advance_cymdh.exe .
  
export DATE=$START_DATE

while test $DATE -le $END_DATE; do
   cat ${EXP_DIR}/run/${DATE}/wrfvar/working/gts_omb_oma >> diagnostics.in
   export DATE=`./da_advance_cymdh.exe $DATE $CYCLE_PERIOD`
done

echo '*end*' >> diagnostics.in

./da_diagnostics.exe > diagnostics.out 2>&1

rm diagnostics.in

echo da_diagnostics.ksh completed

exit 0
