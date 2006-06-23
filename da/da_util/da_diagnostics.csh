#! /bin/csh -f

 echo ""
 echo "Running daprog_diagnostics.exe"
 echo ""
 
 setenv APP amps
 setenv DOM 2

#Set up directories:
 setenv DAT_DIR /var/tmp/dmbarker/data/${APP}${DOM}/verif_obs
 setenv RUN_DIR ${DAT_DIR}/daprog_diagnostics
 echo "RUN_DIR = " $RUN_DIR
 if ( ! -d $RUN_DIR ) then
    mkdir $RUN_DIR
 endif
 cd $RUN_DIR

 #Set up SRC_DIR and DAT_DIR:
 setenv SRC_DIR /snowdrift/users/dmbarker/code_development/wrfvar_amv/da_3dvar/utl
 cp ${SRC_DIR}/daprog_diagnostics.exe ${RUN_DIR}/.
  
 echo "DAT_DIR = " $DAT_DIR

 cp  ${DAT_DIR}/2004050100/var/fort.50    daprog_diagnostics.in
 cat ${DAT_DIR}/2004050112/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050200/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050212/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050300/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050312/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050400/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050412/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050500/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050512/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050600/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050612/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050700/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050712/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050800/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050812/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050900/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004050912/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051000/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051012/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051100/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051112/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051200/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051212/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051300/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051312/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051400/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051412/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051500/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051512/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051600/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051612/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051700/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051712/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051800/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051812/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051900/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004051912/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052000/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052100/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052112/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052200/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052212/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052300/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052312/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052400/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052412/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052500/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052512/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052600/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052612/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052700/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052712/var/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/2004052800/var/fort.50 >> daprog_diagnostics.in

echo "*end*" > tag

echo "*end*" > tag
cat tag >> daprog_diagnostics.in

./daprog_diagnostics.exe >&! daprog_diagnostics.out

rm tag

echo daprog_diagnostics.csh completed!

