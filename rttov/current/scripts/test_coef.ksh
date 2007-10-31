#!/bin/ksh
# Script to test the coefficient files utilities
# 
# P. Brunel Dec 2002
#
# Part 1
# Read ascii file
# rtcoef_1_ascii.out should contain the same thing as rtcoef_noaa_16_avhrr.dat
# rtcoef_1_binary.out is the binary version of rtcoef_noaa_16_avhrr.dat
#
# rtcoef_1_ascii_2.out is the ascii version of rtcoef_1_binary.out
# So verify that rtcoef_1_ascii.out is the same as rtcoef_1_ascii_2.out
#     They can be different from original file (rtcoef_noaa_16_avhrr.dat)
#     for comments and units of reference profile and profile limits
#
# Part 2
# rtcoef_2_ascii.out should contain a selection of channels from rtcoef_noaa_15_hirs.dat
# The verification can be done manually
#
# Part 3
# run test_coef2 program in order to check memory for a
# large coefficient file. The test is performed for AIRS binary file
# 
DATART=${DATART:=../rtcoef_rttov7} 
TEST=${TEST:=../test}	                      ; export TEST


CUR_DIR=`pwd`

echo " "
echo " "
echo " Test coefficient file utilities"
echo " Part 1 and 2"

cd ${TEST}
# Part 1 and 2
cp  $DATART/rtcoef_noaa_15_hirs.dat ${TEST}
cp  $DATART/rtcoef_noaa_16_avhrr.dat ${TEST}

${CUR_DIR}/test_coef.out <<EOF
1, 16, 5   , NOAA 16 AVHRR
1, 15, 0   , NOAA 15 HIRS
10
1, 3, 5, 7, 9, 11, 13, 15, 17, 19
EOF
rm -f  ${TEST}/rtcoef_noaa_15_hirs.dat
rm -f  ${TEST}/rtcoef_noaa_16_avhrr.dat


echo " "
echo " Control ascii files are identical"
diff -s ${TEST}/rtcoef_1_ascii.out ${TEST}/rtcoef_1_ascii_2.out
EXIT=$?
if [ ${EXIT} -eq 0 ] ; then
  echo " Files are identical "
else
  echo " Please control manualy the files contents"
  echo "   of  ${TEST}/rtcoef_1_ascii.out "
  echo "   and ${TEST}/rtcoef_1_ascii_2.out"
fi

# Part 3
rm -f  ${TEST}/rtcoef_eos_2_airs.bin
rm -f  ${TEST}/rtcoef_eos_2_airs.dat
ln -s  $DATART/rtcoef_eos_2_airs.dat ${TEST}
ln -s  $DATART/rtcoef_eos_2_airs.bin ${TEST}

# This part is interactive !!!!!!!!!! to allow the user to check memory
echo ' '
echo 'Part 3'
echo 'This part is interactive !!!!!!!!!! to allow the user to check memory'
echo 'The inputs you have to enter are '
echo ' 9, 2, 11      for EOS 2 AIRS '
echo ' 1             for binary file'
echo '-200           for the first 200 channels'
echo ' then you can check the memory used by the program test_2_coef.out and press return'
${CUR_DIR}/test_2_coef.out
rm -f  ${TEST}/rtcoef_eos_2_airs.bin
rm -f  ${TEST}/rtcoef_eos_2_airs.dat


exit
