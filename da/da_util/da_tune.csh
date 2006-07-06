#! /bin/csh
#-------------------------------------------------------------------
#  Script for running Observation error tuning (Desroz Method)
#  07/05/2006      Syed RH Rizvi
#-------------------------------------------------------------------
#Set up directories:
  setenv REGION   kma_t63
#-------------------------------------------------------------------
setenv DA_DIR  /Volumes/mahua2/rizvi/wrfvar_update
setenv DAT_DIR  /Volumes/mahua2/rizvi/data
setenv RUN_DIR  /Volumes/mahua2/rizvi/data/desroz_results

 if ( ! -d $RUN_DIR )  mkdir $RUN_DIR
 cd ${RUN_DIR}

 setenv DIR_PREFIX  ${DAT_DIR}/${REGION}

 echo ""
 echo "Running script da_tune.csh for region: " ${REGION}
 echo ""
 echo "DA_DIR      = " $DA_DIR
 echo "DAT_DIR     = " $DAT_DIR
 echo "DIR_PREFIX  = " $DIR_PREFIX
 echo "RUN_DIR     = " $RUN_DIR


 cp ${DA_DIR}/build/da_tune.exe .

 cp  ${DIR_PREFIX}/pert/2004102712/wrfvar/fort.45     fort.45
 cat ${DIR_PREFIX}/pert/2004102800/wrfvar/fort.45 >>  fort.45
 
 cp  ${DIR_PREFIX}/pert/2004102712/wrfvar/fort.46     fort.46
 cat ${DIR_PREFIX}/pert/2004102800/wrfvar/fort.46 >>  fort.46
 
 
 cp  ${DIR_PREFIX}/ctrl/2004102712/wrfvar/fort.47     fort.47
 cat ${DIR_PREFIX}/ctrl/2004102800/wrfvar/fort.47 >>  fort.47
 
 cp  ${DIR_PREFIX}/ctrl/2004102712/wrfvar/fort.48     fort.48
 cat ${DIR_PREFIX}/ctrl/2004102800/wrfvar/fort.48 >>  fort.48
 echo "*****" >> fort.48

 cp  ${DIR_PREFIX}/ctrl/2004102712/wrfvar/rsl.out.0000     fort.49
 cat ${DIR_PREFIX}/ctrl/2004102800/wrfvar/rsl.out.0000  >> fort.49
 echo "*****" >> fort.49

 ./da_tune.exe >&! ${REGION}_errfac.dat

 echo "da_tune.csh completed"

 exit (0)

