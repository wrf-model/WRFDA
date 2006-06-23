#! /bin/csh

 echo ""
 echo "Compiling tune.f90"
 echo ""

 f90 -r8 -o tune.exe tune.f90

 echo ""
 echo "Running script tune.csh"
 echo ""

 setenv DIR_PREFIX /taiwania3/dmbarker/error_tuning/desroz_results/kma2

 cp  ${DIR_PREFIX}/2002091200_KMA2_pert/fort.45     fort.45
 cat ${DIR_PREFIX}/2002091203_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091206_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091209_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091212_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091215_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091218_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091221_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091300_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091303_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091306_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091309_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091312_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091315_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091318_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091321_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091400_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091403_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091406_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091409_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091412_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091415_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091418_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091421_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091500_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091503_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091506_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091509_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091512_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091515_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091518_KMA2_pert/fort.45  >> fort.45
 cat ${DIR_PREFIX}/2002091521_KMA2_pert/fort.45  >> fort.45
 echo "*****" >> fort.45
 
 cp  ${DIR_PREFIX}/2002091200_KMA2_pert/fort.46     fort.46
 cat ${DIR_PREFIX}/2002091203_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091206_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091209_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091212_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091215_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091218_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091221_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091300_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091303_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091306_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091309_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091312_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091315_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091318_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091321_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091400_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091403_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091406_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091409_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091412_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091415_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091418_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091421_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091500_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091503_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091506_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091509_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091512_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091515_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091518_KMA2_pert/fort.46  >> fort.46
 cat ${DIR_PREFIX}/2002091521_KMA2_pert/fort.46  >> fort.46
 echo "*****" >> fort.46
 
 cp  ${DIR_PREFIX}/2002091200_KMA2_ctrl/fort.47     fort.47
 cat ${DIR_PREFIX}/2002091203_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091206_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091209_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091212_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091215_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091218_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091221_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091300_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091303_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091306_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091309_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091312_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091315_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091318_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091321_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091400_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091403_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091406_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091409_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091412_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091415_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091418_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091421_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091500_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091503_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091506_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091509_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091512_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091515_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091518_KMA2_ctrl/fort.47  >> fort.47
 cat ${DIR_PREFIX}/2002091521_KMA2_ctrl/fort.47  >> fort.47
 echo "*****" >> fort.47

 cp  ${DIR_PREFIX}/2002091200_KMA2_ctrl/fort.48     fort.48
 cat ${DIR_PREFIX}/2002091203_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091206_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091209_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091212_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091215_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091218_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091221_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091300_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091303_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091306_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091309_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091312_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091315_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091318_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091321_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091400_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091403_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091406_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091409_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091412_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091415_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091418_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091421_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091500_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091503_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091506_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091509_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091512_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091515_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091518_KMA2_ctrl/fort.48  >> fort.48
 cat ${DIR_PREFIX}/2002091521_KMA2_ctrl/fort.48  >> fort.48
 echo "*****" >> fort.48

 cp  ${DIR_PREFIX}/2002091200_KMA2_ctrl/rsl.out.0000     fort.49
 cat ${DIR_PREFIX}/2002091203_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091206_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091209_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091212_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091215_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091218_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091221_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091300_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091303_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091306_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091309_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091312_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091315_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091318_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091321_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091400_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091403_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091406_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091409_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091412_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091415_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091418_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091421_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091500_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091503_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091506_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091509_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091512_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091515_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091518_KMA2_ctrl/rsl.out.0000  >> fort.49
 cat ${DIR_PREFIX}/2002091521_KMA2_ctrl/rsl.out.0000  >> fort.49
 echo "*****" >> fort.49

 ./tune.exe >&! kma2_errfac.dat

 echo "tune.csh completed"

 exit (0)

