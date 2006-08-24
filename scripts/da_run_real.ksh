#!/bin/ksh

# Run real

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export SOLVER=${SOLVER:-EM}
export DUMMY=${DUMMY:-false}
export NL_USE_HTML=${NL_USE_HTML:-false}

export RUN_DIR=${RUN_DIR:-$PWD}
export OUT_DIR=${OUT_DIR:-$RUN_DIR}

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT real</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT real</H1><PRE>"
fi

mkdir -p $RUN_DIR $OUT_DIR

typeset -l LC_SOLVER
LC_SOLVER=$SOLVER

if $DUMMY; then
   echo "Dummy real"
   echo Dummy real > $CS_DIR/$DATE/wrfinput_d${DOMAIN}
   echo Dummy real > $CS_DIR/$DATE/wrfbdy_d${DOMAIN}
   echo Dummy real > $CS_DIR/$DATE/wrflowinp_d${DOMAIN}
else
   $RUN_CMD ${WRF_DIR}/main/real.exe
fi

#rm -f ${MOAD_DATAROOT}/siprd/wrf_real_input_${LC_SOLVER}*
#rm -f ${WRF_DIR}/test/${LC_SOLVER}_real/wrf_real_input_${LC_SOLVER}*

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi
