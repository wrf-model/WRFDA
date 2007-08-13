#!/bin/ksh

export EXPT=vartest
export ID=${ID:-${MACHINE}_${COMPILER}_${TYPE}}
export RUN=${ID}_gen_be

export REL_DIR=${REL_DIR:-$HOME/code/$ID}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export REG_DIR=$PWD
export EXP_DIR=$PWD/$EXPT
export RUN_DIR=$EXP_DIR/$RUN
rm -rf $RUN_DIR

. ./setup.ksh
. $EXP_DIR/setup.ksh

export LOCAL=true
export NUM_PROCS=1
export NUM_JOBS=1

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

echo "Generating $RUN_DIR"
rm -rf $RUN_DIR
mkdir -p $RUN_DIR
cd $RUN_DIR

echo "<HTML><BODY><PRE>" > index.html
$WRFVAR_DIR/scripts/gen_be/gen_be.ksh >> index.html 2>&1

if $NL_GLOBAL; then
   # Produce graphics
   export BE_DIR=$RUN_DIR/working
   ncl $WRFVAR_DIR/graphics/ncl/gen_be/gen_be_global_evals.ncl
   ncl $WRFVAR_DIR/graphics/ncl/gen_be/gen_be_global_evecs.ncl
   echo "<A HREF="gen_be_global_evecs_"$REGION".pdf">Evecs plots</A><BR>" >> index.html
   echo "<A HREF="gen_be_global_evals_"$REGION".pdf">Evals plots</A><BR>" >> index.html
fi

# Preserve the interesting log files
# cp $RUN_DIR/*log $EXP_DIR
# cp $RUN_DIR/stage0/*log $EXP_DIR

echo "</PRE><UL>" >>index.html
for FILE in *.log; do
   echo '<LI><A HREF="'$FILE'">'$FILE'</a>' >> index.html
done

echo  "</UL></BODY></HTML>" >> index.html

