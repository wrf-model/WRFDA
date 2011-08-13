#!/bin/ksh
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

cd $RUN_DIR/working

ln -sf ${BUILD_DIR}/gen_be_cov3d.exe .

cat > gen_be_cov3d_nl.nl << EOF
  &gen_be_cov3d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    ne = ${NE},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}',
    holm_reference = $HREF,
    N_holm_bins = 10/
EOF

./gen_be_cov3d.exe > gen_be_cov3d.$VARIABLE1.$VARIABLE2.log 2>&1

exit 0

