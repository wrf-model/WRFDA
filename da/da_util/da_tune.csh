#! /bin/csh


setenv REGION   kma_t63
setenv DA_DIR  /Volumes/mahua2/rizvi/wrfvar_update
setenv DAT_DIR  /Volumes/mahua2/rizvi/data
setenv RUN_DIR  /Volumes/mahua2/rizvi/data/desroz_results

 if ( ! -d $RUN_DIR )  mkdir $RUN_DIR
 cd ${RUN_DIR}

 echo ""
 echo "Running script da_tune.csh for region: " ${REGION}
 echo ""

 setenv DIR_PREFIX  ${DAT_DIR}/${REGION}

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

