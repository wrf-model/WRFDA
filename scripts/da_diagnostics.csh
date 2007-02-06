#! /bin/csh -f
#-------------------------------------------------------------------------
#  Script for getting initial diagnostics for 
#  Observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#             07/05/2006      Syed RH Rizvi
#-------------------------------------------------------------------------
 echo ""
 echo "Running da_diagnostics.exe"
 echo ""
 
#Set up directories:
 setenv REGION amps1

 setenv DA_DIR /Volumes/mahua2/rizvi/wrfvar_update
 setenv DAT_DIR /Volumes/mahua2/rizvi/data/${REGION}
 setenv DIR_PREFIX  ${DAT_DIR}/mahua_mac_1
 setenv RUN_DIR ${DAT_DIR}/da_diagnostics

 echo "DA_DIR      = " $DA_DIR
 echo "DAT_DIR     = " $DAT_DIR
 echo "DIR_PREFIX  = " $DIR_PREFIX
 echo "RUN_DIR     = " $RUN_DIR

 if ( ! -d $RUN_DIR ) then
    mkdir $RUN_DIR
 endif
 cd $RUN_DIR

 echo ""
 echo "Running script da_diagnostics.csh for region: " ${REGION}
 echo ""

 cp ${DA_DIR}/build/da_diagnostics.exe .


 setenv DIR_PREFIX  ${DAT_DIR}/mahua_mac_1
  
 echo "DAT_DIR = " $DAT_DIR

 cp  ${DIR_PREFIX}/2004050100/wrfvar/fort.50    da_diagnostics.in
 cat ${DIR_PREFIX}/2004050112/wrfvar/fort.50 >> da_diagnostics.in

 echo "*end*" > tag
cat tag >> da_diagnostics.in

./da_diagnostics.exe >&! da_diagnostics.out

rm tag

echo da_diagnostics.csh completed!

