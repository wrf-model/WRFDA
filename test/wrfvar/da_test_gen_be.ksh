#!/bin/ksh

. ./region.ksh

export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}

export NUM_PROCS=1

export EXPT=${ID}_gen_be
export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export CLEAN=${CLEAN:-false}
export LOCAL=true

export NUM_JOBS=1

export DAT_DIR=$PWD/fc
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR/working
export STAGE0_DIR=$RUN_DIR/stage0

export RUN_GEN_BE_STAGE0=true
export RUN_GEN_BE_STAGE1=true
export RUN_GEN_BE_STAGE2=true
export RUN_GEN_BE_STAGE2A=true
export RUN_GEN_BE_STAGE3=true
export RUN_GEN_BE_STAGE4=true
export RUN_GEN_BE_DIAGS=true
export RUN_GEN_BE_DIAGS_READ=true
export RUN_GEN_BE_MULTICOV=true

export N_SMTH_SL=2

echo "Generating $EXP_DIR"
rm -rf $EXP_DIR
mkdir -p $EXP_DIR
cd $EXP_DIR

echo "<HTML><BODY><PRE>" > index.html
$WRFVAR_DIR/scripts/gen_be/gen_be.csh >> index.html 2>&1
# Preserve the interesting log files
cp $RUN_DIR/*log $EXP_DIR
cp $RUN_DIR/stage0/*log $EXP_DIR

echo "</PRE><UL>" >>index.html
for FILE in *.log; do
   echo '<LI><A HREF="'$FILE'">'$FILE'</a>' >> index.html
done

echo  "</UL></BODY></HTML>" >> index.html

