#!/bin/ksh
#
# tests for RTTOV-8 Forward model for all instruments.
#
# P. Brunel Dec 2002
#
DATAIN=${DATAIN:=../data}    ; export DATAIN
DATART=${DATART:=../rtcoef_rttov7}    ; export DATART
REF_TEST=${REF_TEST:=../reftest}     ; export REF_TEST
TEST=${TEST:=../test}	                      ; export TEST
#
#############################################################
PLATFORM='noaa'
INST='avhrr'
#
#
#Test is Tropical Nadir over Sea
CHANNELS1='input_avhrr1.dat'       # For satellites Noaa 5 6 8 10
CHANNELS='input_avhrr.dat'         # For other satellites 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
AZANGLE='0.'
SURF_TYPE='1'			# Sea

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  5 ${CHANNELS1} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  6 ${CHANNELS1} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  7 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST}  8 ${CHANNELS1} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 06 ${PLATFORM} ${INST} 10 ${CHANNELS1} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 07 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 08 ${PLATFORM} ${INST} 12 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 09 ${PLATFORM} ${INST} 14 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 10 ${PLATFORM} ${INST} 15 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 11 ${PLATFORM} ${INST} 16 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 12 ${PLATFORM} ${INST} 17 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 13 ${PLATFORM} ${INST} 18 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='noaa'
INST='hirs'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_irem.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'			# Sea

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  5 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  6 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  7 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 06 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 07 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 08 ${PLATFORM} ${INST} 12 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 09 ${PLATFORM} ${INST} 14 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 10 ${PLATFORM} ${INST} 15 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 11 ${PLATFORM} ${INST} 16 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 12 ${PLATFORM} ${INST} 17 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 13 ${PLATFORM} ${INST} 18 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='noaa'
INST='ssu'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_ssu.dat'         
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  5 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  6 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  7 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 06 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 07 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 08 ${PLATFORM} ${INST} 14 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='noaa'
INST='msu'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_msu_f2.dat'        
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  5 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  6 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  7 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 06 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 07 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 08 ${PLATFORM} ${INST} 12 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 09 ${PLATFORM} ${INST} 14 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='noaa'
INST='amsua'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_amsua_f2.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST} 15 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST} 16 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST} 17 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST} 18 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='noaa'
INST='amsub'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_amsub_f2.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST} 15 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST} 16 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST} 17 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='noaa'
INST='mhs'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_amsub_f2.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST} 18 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='dmsp'
INST='ssmi'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_allsats_ssmi_f2.dat'     
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST} 12 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 06 ${PLATFORM} ${INST} 14 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 07 ${PLATFORM} ${INST} 15 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='dmsp'
INST='ssmis'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_ssmis.dat'     
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST} 16 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='ers'
INST='atsr'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_atsr.dat'     
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  1 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  2 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

PLATFORM='envisat'
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  1 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='goes'
INST='imager'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_goesim.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 05 ${PLATFORM} ${INST} 12 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='gms'
INST='imager'
#                      GMS IMAGER is test number 11 of imagers
#
#Test is Tropical Nadir over Sea
CHANNELS='input_gms.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 11 ${PLATFORM} ${INST}  5 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='goes'
INST='sounder'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_goessnd.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  8 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  9 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST} 10 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 04 ${PLATFORM} ${INST} 11 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='eos'
INST='modis'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_modis.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  1 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  2 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='eos'
INST='airs'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_airs_281.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'
# uncomment next 2 lines to include AIRS
echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  2 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='meteosat'
INST='mviri'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_met.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  5 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  6 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 03 ${PLATFORM} ${INST}  7 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}


#############################################################
PLATFORM='msg'
INST='seviri'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_msg.dat' 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  1 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  2 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='eos'
INST='amsr'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_amsre.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='45.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  2 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

#############################################################
PLATFORM='adeos'
INST='amsr'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_amsr.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='45.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 02 ${PLATFORM} ${INST}  2 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

##############################################################
PLATFORM='coriolis'
INST='windsat'
#
#Test is Tropical Nadir over Sea
CHANNELS='input_windsat.dat'
PROFILE='prof_trop_f2.dat'
ZENANGLE='45.'
AZANGLE='0.'
SURF_TYPE='1'

echo  "Sensor   ${INST}"
single_test_fwd.ksh 01 ${PLATFORM} ${INST}  1 ${CHANNELS}  ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
#############################################################
#   MTSAT imagers is test number 12
PLATFORM='mtsat'
INST='imager'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_mtsat.dat'       # For imager 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
AZANGLE='0.'
SURF_TYPE='1'			# Sea

echo  "Sensor   ${INST}"
single_test_fwd.ksh 12 ${PLATFORM} ${INST}  1 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}
#############################################################
#   FY-2C imagers is test number 13
PLATFORM='fy2'
INST='vissr'
#
#
#Test is Tropical Nadir over Sea
CHANNELS='input_gms.dat'       # For imager 
PROFILE='prof_trop_f2.dat'
ZENANGLE='0.'
AZANGLE='0.'
SURF_TYPE='1'			# Sea

echo  "Sensor   ${INST}"
single_test_fwd.ksh 13 ${PLATFORM} ${INST}  3 ${CHANNELS} ${PROFILE} ${ZENANGLE} ${AZANGLE} ${SURF_TYPE}

exit
