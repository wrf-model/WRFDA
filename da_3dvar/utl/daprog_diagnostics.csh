#! /bin/csh -f

 echo ""
 echo "Running daprog_diagnostics.exe"
 echo ""
 
 setenv APP kma
 setenv DOM 2

#Set up RUN_DIR:
 setenv RUN_DIR /mmmtmp/dmbarker/daprog_diagnostics_${APP}${DOM}
 echo "RUN_DIR = " $RUN_DIR
 if ( -d $RUN_DIR ) then
   rm -rf $RUN_DIR
 endif
 mkdir $RUN_DIR
 cd $RUN_DIR

 #Set up SRC_DIR and DAT_DIR:
 setenv SRC_DIR /taiwania2/dmbarker/WRF3DVAR/da_3dvar/utl
 cp ${SRC_DIR}/daprog_diagnostics.exe ${RUN_DIR}/.
  
 setenv DAT_DIR /taiwania3/dmbarker/error_tuning/omb_results/${APP}${DOM}
 echo "DAT_DIR = " $DAT_DIR

 cp  ${DAT_DIR}/${APP}${DOM}_2002091200/fort.50    daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091203/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091206/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091209/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091212/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091215/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091218/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091221/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091300/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091303/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091306/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091309/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091312/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091315/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091318/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091321/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091400/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091403/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091406/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091409/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091412/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091415/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091418/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091421/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091500/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091503/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091506/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091509/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091512/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091515/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091518/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091521/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091600/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091603/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091606/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091609/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091612/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091615/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091618/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091621/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091700/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091703/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091706/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091709/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091712/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091715/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091718/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091721/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091800/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091803/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091806/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091809/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091812/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091815/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091818/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091821/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091900/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091903/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091906/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091909/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091912/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091915/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091918/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002091921/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092000/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092003/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092006/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092009/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092012/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092015/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092018/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092021/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092100/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092103/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092106/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092109/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092112/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092115/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092118/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092121/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092200/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092203/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092206/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092209/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092212/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092215/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092218/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092221/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092300/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092303/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092306/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092309/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092312/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092315/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092318/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092321/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092400/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092403/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092406/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092409/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092412/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092415/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092418/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092421/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092500/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092503/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092506/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092509/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092512/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092515/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092518/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092521/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092600/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092603/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092606/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092609/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092612/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092615/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092618/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092621/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092700/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092703/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092706/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092709/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092712/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092715/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092718/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092721/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092800/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092803/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092806/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092809/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092812/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092815/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092818/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092821/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092900/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092903/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092906/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092909/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092912/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092915/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092918/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002092921/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093000/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093003/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093006/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093009/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093012/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093015/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093018/fort.50 >> daprog_diagnostics.in
 cat ${DAT_DIR}/${APP}${DOM}_2002093021/fort.50 >> daprog_diagnostics.in

echo "*end*" > tag
cat tag >> daprog_diagnostics.in

./daprog_diagnostics.exe >&! daprog_diagnostics.out

rm tag

echo daprog_diagnostics.csh completed!