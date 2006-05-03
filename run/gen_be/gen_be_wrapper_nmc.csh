#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_wrapper_ens.csh
#
# Purpose: Calculates background error statistics for WRF-Var from an
# ensemble of WRF forecasts (be_method="ENS").
#-----------------------------------------------------------------------

#set echo

#Define job by overriding default environment variables:

setenv RUN_GEN_BE_STAGE0
setenv RUN_GEN_BE_STAGE1
setenv RUN_GEN_BE_STAGE2
setenv RUN_GEN_BE_STAGE2A
setenv RUN_GEN_BE_STAGE3
setenv RUN_GEN_BE_STAGE4
setenv RUN_GEN_BE_DIAGS
setenv RUN_GEN_BE_DIAGS_READ

setenv ID gen_be.2006050310

#Now run gen_be:

./gen_be.csh


