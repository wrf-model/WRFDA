#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_graphics.ksh
#
# Purpose: Plot the background error statistics (BES) for WRF-Var
# 
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

export BE_NROW=$NUM_LEVELS

echo ""
echo "GEN_BE_GRAPHICS: Plot first five eigenvectors."

ncl ${GRAPHICS_DIR}/gen_be_global_evecs.ncl > /dev/null

echo "GEN_BE_GRAPHICS: Plot first five eigenvalues."

ncl ${GRAPHICS_DIR}/gen_be_global_evals.ncl > /dev/null

echo "GEN_BE_GRAPHICS: Plot horizontal lengthscales."

ncl ${GRAPHICS_DIR}/gen_be_lengthscales.ncl > /dev/null

echo "GEN_BE_GRAPHICS: Plot <chi_b.chi> and <t_b.t> correlation."

ncl ${GRAPHICS_DIR}/gen_be_corr_z.ncl > /dev/null

echo "GEN_BE_GRAPHICS: Plot <chi_b.chi> and <t_b.t> correlation x-section."

ncl ${GRAPHICS_DIR}/gen_be_corr_yz.ncl > /dev/null

echo "GEN_BE_GRAPHICS: Plot <ps_b.ps> correlation."

ncl ${GRAPHICS_DIR}/gen_be_corr_ps.ncl > /dev/null

exit 0

