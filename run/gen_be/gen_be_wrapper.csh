#! /bin/csh -f
#-----------------------------------------------------------------------
# Script gen_be_wrapper.csh
#
# Purpose: Calculates background error statistics for WRF-Var.
#-----------------------------------------------------------------------

#set echo

#[1] Define job by overriding default environment variables:

setenv RUN_GEN_BE_STAGE0
setenv RUN_GEN_BE_STAGE1
setenv RUN_GEN_BE_STAGE2
setenv RUN_GEN_BE_STAGE2A
setenv RUN_GEN_BE_STAGE3
setenv RUN_GEN_BE_STAGE4
setenv RUN_GEN_BE_DIAGS
setenv RUN_GEN_BE_DIAGS_READ
setenv RUN_GEN_BE_MULTICOV

setenv ID gen_be.2006082213
setenv DATA_DISK /smoke
setenv WRFVAR_DIR /smoke/dmbarker/code_development/latest/wrfvar.gen_be
setenv LOCAL true
setenv NUM_JOBS 4

#Mississippi:
#setenv START_DATE 2005043000
#setenv END_DATE   2005043000
#setenv NUM_LEVELS 34
#setenv RESOLUTION_KM 4
#setenv EXPT ense_be
#setenv REGION mississippi
#setenv STRIDE 16

#Example of changes required for "be_method=ENS":
#setenv BE_METHOD ENS
#setenv NE 30
#setenv FCST_RANGE 12

#[2] Run gen_be:
./gen_be.csh

