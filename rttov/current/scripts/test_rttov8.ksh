#!/bin/sh
#
# tests for RTTOV-8 for all sats for TL/AD/K.
#
# ROger Saunders 2004
#
######## Edit this section for your pathnames#######
DATAIN=../data; export DATAIN
DATART=../rtcoef_rttov7; export DATART
DATART8F3=../rtcoef_rttov8/fastem3; export DATART8F3
DATART8P7=../rtcoef_rttov8/rttov7pred; export DATART8P7
DATART8P8=../rtcoef_rttov8/rttov8pred; export DATART8P8
REF_TEST=../reftest_rttov8; export REF_TEST
TEST=../test_rttov8; export TEST
###################################################

SAVE_DATART=$DATART

#for TEST_NUMBER in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
#uncomment next line and comment above to run include option airs tests if you have coeff files
for TEST_NUMBER in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
#uncomment next line and comment above to run just 1 test
#for TEST_NUMBER in 08 
do
  DATART=${SAVE_DATART}

  case ${TEST_NUMBER} in
    # Test 1 HIRS N15 Emi=1 Ang=45 Artic rttov-7 predictors
    '01') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_hirs.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 2 HIRS N15 ISEM Ang=0 tropics rttov-7 predictors
    '02') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 3 HIRS N15 Emi=1  Ang=45  arctic rttov-8 predictors
    '03') SAVE_DATART=${DATART} ;
	  DATART=${DATART8P8} ;
          PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_hirs.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 4 HIRS N15 ISEM Ang=0  tropics rttov-8 predictors
    '04') SAVE_DATART=${DATART} ;
          DATART=${DATART8P8} ;
          PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 5 HIRS N15 ISEM Ang=60 sea Arctic
    '05') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 6 HIRS N15 ISEM Ang=45 land  Arctic
    '06') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;
    
    # Test 7 HIRS N15 ISEM Ang=45 arctic sea-ice
    '07') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='2'; FULL='1';;
    
    # Test 8 AMSU-A N15 Emi=0.6 Ang=45 Tropical sea
    '08') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 9 AMSU-A N15 FASTEM-2 Ang=0 Arctic sea
    '09') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 10 AMSU-A N15 FASTEM-3 Ang=0 Arctic sea
    '10') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    	  
    # Test 11 AMSU-A N15 Ang=45 Arctic FASTEM-2 land
    '11') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;
    
    # Test 12 AMSU-A N15 Ang=45 Arctic FASTEM-3 land
    '12') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;

    # Test 13 AMSU-A N15  Ang=45 tropics FASTEM-2 CLWP sea
    '13') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_lwp_f2.dat';
	  PROFILE='prof_lwp_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 14 MSU N14 FASTEM-3 Ang=45 Tropical sea
    '14') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
          PLATFORM='noaa'; INST='msu'; SATID='14';CHANNELS='input_msu_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 15 SSM/I F-15 FASTEM-3 Ang=55. Tropical
    '15') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3}  ;
	  PLATFORM='dmsp'; INST='ssmi'; SATID='15';CHANNELS='input_ssmi_f3.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 16 SSM/I F-15 FASTEM-2 Ang=55. Tropical 
    '16') PLATFORM='dmsp'; INST='ssmi'; SATID='15';CHANNELS='input_ssmi_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 17 TMI/TRMM FASTEM-2 Ang=55. Tropical 
    '17') PLATFORM='trmm'; INST='tmi'; SATID='1';CHANNELS='input_tmi_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 18 METEOSAT-7 ISEM Ang=60 Arctic
    '18') PLATFORM='meteosat'; INST='mviri'; SATID='7';CHANNELS='input_met.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 19 SSU N14 Emi=1 Ang=60 Arctic sea
    '19') PLATFORM='noaa'; INST='ssu'; SATID='14';CHANNELS='input_ssu.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 20 AVHRR N14 ISEM Ang=60 Arctic sea
    '20') PLATFORM='noaa'; INST='avhrr'; SATID='14';CHANNELS='input_avhrr.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 21 GOES-11 IMAGER ISEM Ang=30 tropic sea
    '21') PLATFORM='goes'; INST='imager'; SATID='11';CHANNELS='input_goesim.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 22 GOES-11 SOUNDER ISEM Ang=30 tropic sea
    '22') PLATFORM='goes'; INST='sounder'; SATID='11';CHANNELS='input_goessnd.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 23 AMSU-B N15 FASTEM-2 Ang=0 Tropical
    '23') PLATFORM='noaa'; INST='amsub'; SATID='15';CHANNELS='input_amsub_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 24 AMSU-B N16  Ang=60 Arctic FASTEM-3
    '24') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
          PLATFORM='noaa'; INST='amsub'; SATID='16';CHANNELS='input_amsub_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 25 MSG-1 ISEM Ang=45 Tropical
    '25') PLATFORM='msg'; INST='seviri'; SATID='1';CHANNELS='input_msg.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 26 MODIS-1 ISEM Ang=30 Tropical
    '26') PLATFORM='eos'; INST='modis'; SATID='1';CHANNELS='input_modis.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 27 ATSR-2 ISEM Ang=55 Tropic
    '27') PLATFORM='ers'; INST='atsr'; SATID='2';CHANNELS='input_atsr.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 28 HIRS N15 tropics test emis=0.8 including overcast radiance arrays  
    '28') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_ircloud1.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;

    # Test 29 AMSR-E Aqua arctic test 55 deg FASTEM-2 sea  
    '29') PLATFORM='eos'; INST='amsr'; SATID='2';CHANNELS='input_amsre.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 30 HIRS N15 Emi=0.8 Ang=30 tropics rttov-7 predictor 60% cloud 400hPa
    '30') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_ircloud1.dat';
	  PROFILE='prof_trop_fracld.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;

    # Test 31 AMSU-A N15 Ang=30 arctic em=0.5 sea
    '31') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_em05.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;
 
    # Test 32 Windsat Coriolis Fastem-3 Ang=45 Artic
    '32') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
	  PLATFORM='coriolis'; INST='windsat'; SATID='1';CHANNELS='input_windsat.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
 
    # Test 33 AIRS ISEM Ang=30 Sea Tropical
    '33') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 34 AIRS ISEM Ang=45 Sea Tropical 100% cloud cover 400hPa
    '34') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs.dat';
	  PROFILE='prof_trop_cld.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;

    # Test 35 SSMIS Fastem-3 Ang=55 Sea Tropical 
    '35') SAVE_DATART=${DATART} ;
	  DATART=${DATART8F3} ;
          PLATFORM='dmsp'; INST='ssmis'; SATID='16';CHANNELS='input_ssmis_f3.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 36 SSMIS Fastem-2 Ang=55 Sea Tropical 
    '36') PLATFORM='dmsp'; INST='ssmis'; SATID='16';CHANNELS='input_ssmis_test.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
  esac

  single_test_full.ksh ${TEST_NUMBER} ${PLATFORM} ${INST} ${SATID} ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE} ${FULL}
  
done

exit
