#!/bin/ksh
# Script to run the EXAMPLE_FWD code
#
# The result is compared to a reference file. See users guide
# to note what are the "normal" differences.
# 
# This script runs only ONE test
# 
# P. Brunel April 2004
#

# use function for rttov ids
. rttov_id.ksh

# Test case
PLATFORM=noaa
INST=avhrr
SATID=16
FILE_PROFILE=../data/prof_trop_f2.dat
ZENANGLE=0.
AZANGLE=0.
SURF_TYPE=1	# Sea

#directory for RT coefs
DATART=../rtcoef_rttov7
# Test output directory
TEST=${TEST:=../test} ; export TEST

echo " "
echo " "
echo " Test forward  ${INST} "
echo " "

echo  "Platform   ${PLATFORM}"
echo  "Instrument ${INST}"
echo  "Satellite  ${SATID}"
echo  "File for profile  ${FILE_PROFILE}"
echo  "Zenith angle ${ZENANGLE}"
echo  "Azimuth angle ${AZANGLE}"
echo  "Surface type ${SURF_TYPE}"


# call to function rttov_id
rttov_id ${PLATFORM} ${INST}


# Profile file
rm -f prof.dat
if [ ! -s ${FILE_PROFILE} ] ; then
  echo "file ${FILE_PROFILE}  does not exist "
  exit
fi
cp ${FILE_PROFILE}  prof.dat

# Coefficient files
COEF_NAME=rtcoef_${PLATFORM}_${SATID}_${INST}.dat
COEF_NAME_BIN=rtcoef_${PLATFORM}_${SATID}_${INST}.bin
rm -f ${COEF_NAME}
rm -f ${COEF_NAME_BIN}
if [ -s  $DATART/${COEF_NAME} ] ; then
  ln -s $DATART/${COEF_NAME} ${COEF_NAME}
fi
if [ -s  $DATART/${COEF_NAME_BIN} ] ; then
  ln -s $DATART/${COEF_NAME_BIN} ${COEF_NAME_BIN}
fi


example_fwd.out << EOF
${PLATFORM_N}   , platform name
${SATID}        , satellite number (NOAAxx = xx)
${INST_N}       , instrument number
${SURF_TYPE}    , Surface type (0=land, 1=sea, 2=ice/snow)
${ZENANGLE}     , Zenith angle
${AZANGLE}      , Azimuth angle
 1, 1, 0.0      , channel 1, valid, emissivity=0.0
 2, 1, 0.0      , channel 2, valid, emissivity=0.0
 3, 1, 0.0      , channel 3, valid, emissivity=0.0
EOF

rm -f ${COEF_NAME} ${COEF_NAME_BIN}
rm -f prof.dat 
mv print.dat ${TEST}/example_fwd.lst

echo 
echo "result is in ../test/example_fwd.lst file "
echo

exit
