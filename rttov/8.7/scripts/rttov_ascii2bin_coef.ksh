#!/bin/ksh
#
# create binary coefficients for all ASCII coef files found
# in directory DATART
#
# P. Brunel Feb 2003
#

DATA=${DATA:=..}                              ; export DATA
DATART=${DATART:=$DATA/rtcoef_rttov7} 	      ; export DATART

CUR_DIR=`pwd`

# use function for rttov ids
. rttov_id.ksh

cd $DATART

for FILE in `\ls *.dat`
do
  set `echo $FILE | tr '_' ' ' | tr '.' ' '`
  PLATFORM=$2
  INST=$4
  SATID=$3

  # call to function rttov_id
  rttov_id ${PLATFORM} ${INST}

  echo "converting coef file for rttov id: ${PLATFORM} ${INST} ${SATID}"

  ${CUR_DIR}/rttov_ascii2bin_coef.out << EOF
  ${PLATFORM_N}, ${SATID}, ${INST_N}
   0
EOF

done
exit
