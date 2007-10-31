#!/bin/sh
#
# SX-6 tests for RTTOV-8_5.
#
# ROger Saunders 2004
#
######## Edit this section for your pathnames#######
DATAIN=${DATAIN:=../data}    ; export DATAIN
DATART=${DATART:=../rtcoef_rttov7}  ; export DATART
DATART1=${DATART1:=../rtcoef_rttov8} ; export DATART1
TEST_REF=${TEST_REF:=../reftest_sx6}     ; export TEST_REF
TEST=${TEST:=../test_sx6}            ; export TEST
###################################################

SAVE_DATART=$DATART

for TEST_NUMBER in 01 02 03 04 05 
#for TEST_NUMBER in 01 02 03
#uncomment next line and comment above to run just 1 test
#for TEST_NUMBER in 01
do
  DATART=${SAVE_DATART}

  case ${TEST_NUMBER} in
       
    # Test 01 AMSU-A N15 FASTEM-3 Ang=50 Arctic sea 1 profile
    '01') SAVE_DATART=${DATART} ;
	  DATART=${DATART1} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='50.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='1';;

    # Test 02 HIRS N15 ISEM Ang=0  tropics rttov-8 predictors 1 profile
    '02') SAVE_DATART=${DATART} ;
          DATART=${DATART1} ;
          PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='1';;

    # Test 03 AIRS ISEM Ang=30 Sea Tropical 1 profile
    '03') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs_sx6.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='1';;
       
    # Test 04 AMSU-A N15 FASTEM-3 Ang=50 Arctic sea 50 profile
    '04') SAVE_DATART=${DATART} ;
	  DATART=${DATART1} ;
          PLATFORM='noaa'; INST='amsua'; SATID='15';CHANNELS='input_amsua_f2.dat';
	  PROFILE='prof_arc_f2.dat'; ZENANGLE='50.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='50';;

    # Test 05 HIRS N15 ISEM Ang=0  tropics rttov-8 predictors 50 profile
    '05') SAVE_DATART=${DATART} ;
          DATART=${DATART1} ;
          PLATFORM='noaa'; INST='hirs'; SATID='15';CHANNELS='input_irem.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='0.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='50';;

    # Test 06 AIRS ISEM Ang=30 Sea Tropical 50 profile
    '06') PLATFORM='eos'; INST='airs'; SATID='2';CHANNELS='input_airs_sx6.dat';
	  PROFILE='prof_trop_f2.dat'; ZENANGLE='30.'; AZANGLE='45.'; SURF_TYPE='1'; NPROF='50';;
    
  esac

  single_test_sx6.ksh ${TEST_NUMBER} ${PLATFORM} ${INST} ${SATID} ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE} ${NPROF}
  
done

exit
