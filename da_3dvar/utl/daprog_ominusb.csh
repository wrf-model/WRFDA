#! /bin/csh -f

 echo ""
 echo "Running daprog_ominusb.exe"
 echo ""
 
 setenv RUN_DIR /mmmtmp/dmbarker/daprog_diagnostics_kma2
 setenv SRC_DIR /taiwania2/dmbarker/WRF3DVAR/da_3dvar/utl
 setenv FILENAME synopt_omb
 
#Set up RUN_DIR:
 echo "RUN_DIR = " $RUN_DIR
 cd $RUN_DIR

#Set up and run:
 cp ${SRC_DIR}/daprog_ominusb.exe ${RUN_DIR}/.
 ln -sf ${RUN_DIR}/${FILENAME}.dat fort.35

./daprog_ominusb.exe >&! daprog_ominusb_${FILENAME}.out

rm daprog_ominusb.exe fort.35

echo daprog_ominusb.csh completed!
