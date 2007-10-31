#!/bin/sh
#
# tests for RTTOV-7/8 for all sats for TL/AD/K.
#
# P. Brunel Dec 2002
#
######## Edit this section for your pathnames#######
DATAIN=../data; export DATAIN
DATART=../rtcoef_rttov7; export DATART
DATART1=../rtcoef_rttov8; export DATART1
REF_TEST=../reftest; export REF_TEST
TEST=../test; export TEST
###################################################

SAVE_DATART=$DATART

for TEST_NUMBER in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33
#uncomment next line and comment above to run include airs tests
#for TEST_NUMBER in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
#uncomment next line and comment above to run just 1 test
#for TEST_NUMBER in 06 13 33 
do
  DATART=${SAVE_DATART}

  case ${TEST_NUMBER} in
    # Test 1 HIRS N15 Emi=1 Ang=45 Artic
    '01') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_hirs.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 2 HIRS N15 ISEM Ang=0 tropics
    '02') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 3 HIRS N15 ISEM Ang=60 arctic
    '03') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 4 HIRS N15 ISEM Ang=45 land Arctic
    '04') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;
    
    # Test 5 HIRS N15 ISEM Ang=45 arctic sea-ice
    '05') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='2'; FULL='1';;
    
    # Test 6 AMSU-A N15 Emi=0.6 Ang=45 Tropical
    '06') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 7 AMSU-A N15 FASTEM-2 Ang=0 Tropical
    '07') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 8 AMSU-A N15 FASTEM-3 Ang=55 Tropical sea
    '08') SAVE_DATART=${DATART} ;
	  DATART=${DATART1} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    	  
    # Test 9 AMSU-A N15 Ang=60 Arctic FASTEM-2
    '09') DATART=${SAVE_DATART} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 10 AMSU-A N15 Ang=45 Arctic FASTEM-2 land
    '10') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;

    # Test 11 AMSU-A N15  Ang=45 tropics FASTEM-2 CLWP
    '11') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_lwp_f2.dat';
	  PROFILE='prof_lwp_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 12 MSU N14 FASTEM-2 Ang=45 Tropical
    '12') PLATFORM='noaa'; INST='msu'; SATID='14';CHANNELS='input_msu_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 13 SSM/I F-15 FASTEM-3 Ang=55. Tropical
    '13') SAVE_DATART=${DATART} ;
	  DATART=${DATART1}  ;
	  PLATFORM='dmsp'; INST='ssmi'; SATID='15';CHANNELS='input_ssmi_f3.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
    
    # Test 14 SSM/I F-13  Ang=55. Tropical FASTEM-2
    '14') DATART=${SAVE_DATART} ;
          PLATFORM='dmsp'; INST='ssmi'; SATID='13';CHANNELS='input_ssmi_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 15 TMI/TRMM FASTEM-2 Ang=55. Tropical Land
    '15') PLATFORM='trmm'; INST='tmi'; SATID='1';CHANNELS='input_tmi_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='0'; FULL='1';;

    # Test 16 METEOSAT-7 ISEM Ang=45 Tropical
    '16') PLATFORM='meteosat'; INST='mviri'; SATID='7';CHANNELS='input_met.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 17 SSU N14 Emi=1 Ang=60 Arctic sea
    '17') PLATFORM='noaa'; INST='ssu'; SATID='14';CHANNELS='input_ssu.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 18 AVHRR N14 ISEM Ang=60 Arctic sea
    '18') PLATFORM='noaa'; INST='avhrr'; SATID='14';CHANNELS='input_avhrr.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 19 GOES-11 IMAGER ISEM Ang=30 tropic sea
    '19') PLATFORM='goes'; INST='imager'; SATID='11';CHANNELS='input_goesim.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 20 GOES-11 SOUNDER ISEM Ang=30 tropic sea
    '20') PLATFORM='goes'; INST='sounder'; SATID='11';CHANNELS='input_goessnd.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 21 AMSU-B N15 FASTEM-2 Ang=0 Tropical
    '21') PLATFORM='noaa'; INST='amsub'; SATID='15';CHANNELS='input_amsub_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 22 AMSU-B N16  Ang=60 Arctic FASTEM-2
    '22') PLATFORM='noaa'; INST='amsub'; SATID='16';CHANNELS='input_amsub_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='60.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 23 MSG-1 ISEM Ang=45 Tropical
    '23') PLATFORM='msg'; INST='seviri'; SATID='1';CHANNELS='input_msg.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 24 MODIS-1 ISEM Ang=30 Tropical
    '24') PLATFORM='eos'; INST='modis'; SATID='1';CHANNELS='input_modis.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 25 ATSR-2 ISEM Ang=55 Tropical
    '25') PLATFORM='ers'; INST='atsr'; SATID='2';CHANNELS='input_atsr.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 26 HIRS N15 tropics test including overcast radiance arrays  
    '26') PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_ircloud.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;

    # Test 27 AMSU N15 tropics test including overcast radiance arrays  
    '27') PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_mwcloud.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='2';;

    # Test 28 AIRS ISEM Ang=30 Tropical
    '28') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 29 AIRS ISEM Ang=45 Tropical
    '29') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs.dat';
	  PROFILE='prof_trop_cld.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 30 TRMM/TMI FASTEM-2 Ang=55 Tropical sea
    '30') PLATFORM='trmm'; INST='tmi'; SATID='1';CHANNELS='input_tmi_f2.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 31 EOS/AMSR FASTEM-2 Ang=55 Tropical sea
    '31') PLATFORM='eos'; INST='amsr'; SATID='2';CHANNELS='input_amsre.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='55.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;

    # Test 32 AIRS Emi=0.8 Ang=30 Tropical 60% cloud cover 400hPa
    '32') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs_em8.dat';
	       PROFILE='prof_trop_fracld.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
	  
    # Test 33 Windsat Coriolis Fastem-3 Ang=45 Artic
    '33') SAVE_DATART=${DATART} ;
	  DATART=${DATART1} ;
	  PLATFORM='coriolis'; INST='windsat'; SATID='01';CHANNELS='input_windsat.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='45.'; AZANGLE='45.'; SURF_TYPE='1'; FULL='1';;
  esac

  single_test_full.ksh ${TEST_NUMBER} ${PLATFORM} ${INST} ${SATID} ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE} ${FULL}
  
done

exit
