#! /bin/csh -f

 echo ""
 echo "Running da_ominusb.exe"
 echo ""
 
 setenv RUN_DIR /mmmtmp/dmbarker/da_diagnostics_kma2
 setenv SRC_DIR /taiwania2/dmbarker/WRF3DVAR/da_3dvar/utl
 setenv FILENAME synopt_omb
 
#Set up RUN_DIR:
 echo "RUN_DIR = " $RUN_DIR
 cd $RUN_DIR

#Set up and run:
 cp ${SRC_DIR}/da_ominusb.exe ${RUN_DIR}/.
 ln -sf ${RUN_DIR}/${FILENAME}.dat fort.35

./da_ominusb.exe >&! da_ominusb_${FILENAME}.out

rm da_ominusb.exe fort.35

echo da_ominusb.csh completed!
