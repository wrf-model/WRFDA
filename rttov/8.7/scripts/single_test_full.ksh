#!/bin/ksh
# Script to run the TSTRAD code in full gradient mode (TL/AD/K)
# The result is compared to a reference file. See users guide
# to note what are the "normal" differences.
# 
# This script runs only ONE test
# 
# P. Brunel Dec 2002
#

usage(){
  echo
  echo " run one RTTOV TL/AD/K test"; 
  echo
  echo "usage  $0 Test_number platform instrument sat_id file_channels file_profile zenangle surface_type full";
  echo "  test_number            the reference test number (see test plan)";
  echo "  platform	platform name (noaa, goes...)";
  echo "  instrunemt    instrument name (hirs amsua ..)   ";
  echo "  sat_id        satellite number (xx for NOAAxx)   ";
  echo "  file_channels file containg the list of channels/emissivity  ";
  echo "  file_profile  file containg the input profile   ";
  echo "  zenangle      zenithal angle (deg)   ";
  echo "  azangle       azimuthal angle (deg)   ";
  echo "  surface_type  surface type   (0=land, 1=sea, 2=ice/snow) ";
  echo "  full          full gradient==1, radiance output ==2 ";
  echo "     ";
  echo "environment variables:"
  echo "  DATAIN  	directory for file channels and profiles "
  echo "  DATART	directory for coefficient files"
  echo "  REF_TEST	directory for reference test results"
  echo "  TEST		directory for test results "
  echo
  echo "environment variables default values:"
  echo "  DATAIN  	../data "
  echo "  DATART	../rtcoef"
  echo "  REF_TEST	../new_reftest"
  echo "  TEST		../new_test "
  echo
}
if [ $# -lt 9 ]
then
	usage
	exit 1
fi


# use function for rttov ids
. rttov_id.ksh


# Get arguments
TEST_NUMBER=$1
PLATFORM=$2
INST=$3
SATID=$4
FILE_CHANNELS=$5
FILE_PROFILE=$6
ZENANGLE=$7
AZANGLE=$8
SURF_TYPE=$9
FULL=${10}

# diff command ignoring case of letters and spaces
DIFF='diff -biw'

# Following variables should have been initialised and exported
# in the calling script, else default values are appling
DATAIN=${DATAIN:=../data}  
DATART=${DATART:=../rtcoef} 
REF_TEST=${REF_TEST:=../new_reftest}
TEST=${TEST:=../new_test}

# Create directory for test results
if [ ! -d ${TEST} ]; then
  mkdir ${TEST}
fi
#
###################################################
# The followind sed command file is to change all occurence of
# .xxx or -.yyy into 0.xxx and -0.yyy to allow a good comparison
# between the refernce test and fortran output (especially for HP)
cat <<EOF > sed.cmd
s/ \./0\./g
s/ -\./-0\./g
EOF
#

CUR_DIR=`pwd`

echo " "
echo " "
echo " Test gradient number ${TEST_NUMBER}"
echo " "

echo  "Platform   ${PLATFORM}"
echo  "Instrument ${INST}"
echo  "Satellite  ${SATID}"
echo  "File for channels ${FILE_CHANNELS}"
echo  "File for profile  ${FILE_PROFILE}"
echo  "Zenith angle ${ZENANGLE}"
echo  "Azimuth angle ${AZANGLE}"
echo  "Surface type ${SURF_TYPE}"
echo  "full gradient==1 or radiance output ==2:  ${FULL}"

# call to function rttov_id
rttov_id ${PLATFORM} ${INST}

rm -f input.dat prof.dat

if [ ! -s ${DATAIN}/${FILE_CHANNELS} ] ; then
  echo "file ${FILE_CHANNELS}  does not exist on directory  ${DATAIN}"
  exit
fi
if [ ! -s ${DATAIN}/${FILE_PROFILE} ] ; then
  echo "file ${FILE_PROFILE}  does not exist on directory  ${DATAIN}"
  exit
fi

cp ${DATAIN}/${FILE_CHANNELS} input.dat
cp ${DATAIN}/${FILE_PROFILE}  prof.dat

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
echo
echo
echo "Gradient test ${PLATFORM} ${SATID} ${INST}"
echo "About to run"

tstrad.out <<EOF
${PLATFORM_N}   , platform name
${SATID}        , satellite number (NOAAxx = xx)
${INST_N}       , instrument number
${FULL}         , forward model only == 0, full gradient==1, radiance output ==2
2               , number of profiles
${SURF_TYPE}    , Surface type (0=land, 1=sea, 2=ice/snow)
${ZENANGLE}     , Zenith angle
${AZANGLE}      , Azimuth angle
EOF

rm -f ${COEF_NAME} ${COEF_NAME_BIN}
sed -f sed.cmd print.dat > ${TEST}/tstrad_full_${TEST_NUMBER}.lst
${DIFF} ${REF_TEST}/tstrad_full_${TEST_NUMBER}.lst ${TEST}/tstrad_full_${TEST_NUMBER}.lst > ${TEST}/tstrad_full_${TEST_NUMBER}.diff

rm -f print.dat sed.cmd input.dat prof.dat

date
exit

