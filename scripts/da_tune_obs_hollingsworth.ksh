#!/bin/ksh
#-------------------------------------------------------------------------
#  Script for getting initial diagnostics for 
#  Observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#-------------------------------------------------------------------------

echo ""
echo "Running da_tune_obs_hollingsworth.ksh"
echo ""

export REL_DIR=${REL_DIR:-$HOME/code/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export DAT_DIR=${DAT_DIR:-$HOME/data}
export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export EXPT=${EXPT:-test}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}

export START_DATE=${START_DATE:-2003010100}
export END_DATE=${END_DATE:-2003010200}
export CYCLE_PERIOD=${CYCLE_PERIOD:-6}

export WORK_DIR=${WORK_DIR:-$PWD/hollingsworth}

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

echo "WRFVAR_DIR    = $WRFVAR_DIR"
echo "EXP_DIR       = $EXP_DIR"
echo "WORK_DIR      = $WORK_DIR"
echo "START_DATE    = $START_DATE"
echo "END_DATE      = $END_DATE"

ln -fs ${WRFVAR_DIR}/build/da_tune_obs_hollingsworth1.exe .
ln -fs ${WRFVAR_DIR}/build/da_tune_obs_hollingsworth2.exe .
ln -fs ${WRFVAR_DIR}/build/da_advance_cymdh.exe .
  
export DATE=$START_DATE

while test $DATE -le $END_DATE; do
   cat ${EXP_DIR}/run/${DATE}/wrfvar/working/gts_omb_oma >> hollingsworth1.in
   export DATE=`./da_advance_cymdh.exe $DATE $CYCLE_PERIOD`
done

echo '*end*' >> hollingsworth1.in

./da_tune_obs_hollingsworth1.exe > hollingsworth1.log 2>&1

rm hollingsworth1.in

for FILE1 in *.dat; do
   FILE2=`basename $FILE1`
   FILE2=${FILE2%%.dat}
   ln -fs $FILE1 fort.35

   ./da_tune_obs_hollingsworth2.exe > hollingsworth2_$FILE2.log 2>&1
   if test -f fort.30; then 
      mv fort.30     $FILE2.sigma_o_b
   fi
   mv hollingsworth2.out $FILE2.out
done

rm fort.35

exit 0
echo ""
echo " da_tune_obs_hollingsworth.ksh completed"
echo ""
