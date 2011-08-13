#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper.ksh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

#[1] Define job by overriding default environment variables:

export RUN_GEN_BE_STAGE0=true
export RUN_GEN_BE_STAGE1=true
export RUN_GEN_BE_STAGE2=true
export RUN_GEN_BE_STAGE2A=true
export RUN_GEN_BE_STAGE3=true
export RUN_GEN_BE_STAGE4=true
export RUN_GEN_BE_DIAGS=true
export RUN_GEN_BE_DIAGS_READ=true
export RUN_GEN_BE_MULTICOV=true
export RUN_GEN_BE_GRAPHICS=true

export GEN_BE_DIR=/Users/dale/work/models/gen_be

export START_DATE=2006102800  # the first perturbation valid date
export END_DATE=2006102800    # the last perturbation valid date
export NUM_WE=121             # WE dimension - 1
export NUM_SN=109             # SN dimension - 1
export NUM_LEVELS=41          # number levels - 1
export BIN_TYPE=5
export FC_DIR=/Users/dale/work/data/t64a/test/fc #Where wrf forecasts are
export RUN_DIR=/Users/dale/work/data/t64a/test/gen_be${BIN_TYPE}
export DOMAIN=01
export FCST_RANGE1=12
export FCST_RANGE2=12
export INTERVAL=12
export STRIDE=1

#Example of changes required for "be_method=ENS":
export BE_METHOD=ENS
export NE=10
export FCST_RANGE=12

#[2] Run gen_be:
${GEN_BE_DIR}/scripts/gen_be.ksh

