#!/bin/ksh
################################################################################
#
# Test of RTTOVSCATT (TL/AD/K) 
# Author : F Chevallier - 10/10/2003
#
################################################################################
################################################################################
#
# From output files, the user can check:
#    - The Taylor test (correctnes of the tangent linear)
#      The radiance and Tb increments should converge toward 1
#        when lambda goes to zero (actually the machine accuracy
#        usually prevents the smallest lambdas to converge)
#      Example of correct output:
#         Lambda       Clear Rad      Cloudy Rad        Clear Tb       Cloudy Tb
#   0.0000000001    1.0000455292    1.0000122434    0.9999679400    0.9999566268
#   0.0000000010    1.0000005101    0.9999989064    1.0000014735    1.0000011534
#   0.0000000100    0.9999998175    1.0000000495    1.0000003557    1.0000000403
#   0.0000001000    0.9999999907    0.9999999352    1.0000000204    1.0000000403
#   0.0000010000    1.0000000253    1.0000000210    0.9999999980    0.9999999957
#   0.0000100000    1.0000001964    1.0000002233    0.9999999913    1.0000000047
#   0.0001000000    1.0000019611    1.0000022357    0.9999999170    1.0000000562
#   0.0010000000    1.0000196105    1.0000223562    0.9999991693    1.0000005610
#   0.0100000000    1.0001961108    1.0002235821    0.9999916923    1.0000056164
#   0.1000000000    1.0019617263    1.0022378853    0.9999168790    1.0000568351
#   1.0000000000    1.0196752508    1.0226002561    0.9991619563    1.0006477333
#
#    - The norm equality (correctnes of adjoint) 
#      The difference between the two norms should be about the order of the 
#        machine
#      Example of correct output:
#        The difference is     5.977 times the zero of the machine
#
#    - That the K is correct
#      The output should say: K is ok
#  
################################################################################
# define directories
#
DATAIN=${DATAIN:=../data}    ; export DATAIN
TEST=${TEST:=../test}	                      ; export TEST
TEST_REF8=${TEST_REF8:=../reftest_rttov8}       ; export TEST_REF8
DATART8=${DATART8:=../rtcoef_rttov8} 	      ; export DATART8
TEST_REF7=${TEST_REF7:=../reftest}       ; export TEST_REF7
DATART7=${DATART7:=../rtcoef_rttov7} 	      ; export DATART7
DATARTSC=${DATARTSC:=../rtcoef_scatt} 	      ; export DATARTSC
#
# The followind sed command file is to change all occurence of
# .xxx or -.yyy into 0.xxx and -0.yyy to allow a good comparison
# between the refernce test and fortran output (especially for HP)
cat <<EOF > sed.cmd
s/ \./0\./g
s/ -\./-0\./g
EOF
#
if [ ! -d ${TEST} ]; then
    mkdir ${TEST}
fi
#
# diff command ignoring case of letters and spaces
DIFF='diff -biw'
#
date
rm -f refprof.dat
rm -f rtcoef_*.dat
rm -f mietable_*.dat
rm -f profiles2_fmt
#
ln -s $DATAIN/refprof.dat refprof.dat
#########SSMI RTTOV-8####################################
# Tbs
tb=2
# DMSP F-15
satid=15

rm -f rtcoef_dmsp_${satid}_ssmi.dat profiles2_fmt mietable_dmsp_ssmi.dat
ln -s $DATART8/rtcoef_dmsp_${satid}_ssmi.dat rtcoef_dmsp_${satid}_ssmi.dat 
ln -s $DATARTSC/mietable_dmsp_ssmi.dat mietable_dmsp_ssmi.dat
ln -s $DATAIN/profiles2_fmt profiles2_fmt

echo '===========================================================================  '
echo '= Start of RTTOVSCATT test'
echo '= '
echo '= Tests are performed for SSMI onboard DMSP-15,'
echo '=   but any microwave instrument could be used by RTTOVSCATT'
echo '===========================================================================  '

rttovscatt_test.out << eof
2
15
6
53.1
eof
mv outputscatt.ascii ${TEST}/outputscatt_ssmi8.lst
diff ${TEST}/outputscatt_ssmi8.lst ${TEST_REF8}/outputscatt_ssmi8.lst > ${TEST}/outputscatt_ssmi8.diff

rm -f rtcoef_dmsp_${satid}_ssmi.dat
rm -f mietable_dmsp_ssmi.dat
rm -f profiles2_fmt

######SSM/I RTTOV-7 ##########################################################################
# Tbs
tb=2
# DMSP F-15
satid=15

rm -f rtcoef_dmsp_${satid}_ssmi.dat profiles2_fmt mietable_dmsp_ssmi.dat
ln -s $DATART7/rtcoef_dmsp_${satid}_ssmi.dat rtcoef_dmsp_${satid}_ssmi.dat 
ln -s $DATARTSC/mietable_dmsp_ssmi.dat mietable_dmsp_ssmi.dat
ln -s $DATAIN/profiles2_fmt profiles2_fmt

echo '===========================================================================  '
echo '= Start of RTTOVSCATT test'
echo '= '
echo '= Tests are performed for SSMI onboard DMSP-15,'
echo '=   but any microwave instrument could be used by RTTOVSCATT'
echo '===========================================================================  '

rttovscatt_test.out << eof
2
15
6
53.1
eof
mv outputscatt.ascii ${TEST}/outputscatt_ssmi7.lst
diff ${TEST}/outputscatt_ssmi7.lst ${TEST_REF7}/outputscatt_ssmi7.lst > ${TEST}/outputscatt_ssmi7.diff

rm -f rtcoef_dmsp_${satid}_ssmi.dat
rm -f mietable_dmsp_ssmi.dat
rm -f profiles2_fmt
#######AMSU-A RTTOV-8#########################################################################
# Tbs
tb=2
# NOAA-15
satid=15

rm -f rtcoef_noaa_${satid}_amsua.dat profiles2_fmt mietable_noaa_amsua.dat
ln -s $DATART8/rtcoef_noaa_${satid}_amsua.dat rtcoef_noaa_${satid}_amsua.dat 
ln -s $DATARTSC/mietable_noaa_amsua.dat mietable_noaa_amsua.dat
ln -s $DATAIN/profiles2_fmt profiles2_fmt

echo '===========================================================================  '
echo '= Start of RTTOVSCATT test'
echo '= '
echo '= Tests are performed for AMSU-A onboard NOAA-15,'
echo '=   but any microwave instrument could be used by RTTOVSCATT'
echo '===========================================================================  '

rttovscatt_test.out << eof
1
15
3
45
eof
mv outputscatt.ascii ${TEST}/outputscatt_amsu8.lst
diff ${TEST}/outputscatt_amsu8.lst ${TEST_REF8}/outputscatt_amsu8.lst > ${TEST}/outputscatt_amsu8.diff

rm -f rtcoef_noaa_${satid}_amsua.dat
rm -f mietable_noaa_amsua.dat
rm -f profiles2_fmt
###################AMSU-A RTTOV-7##############################################
# Tbs
tb=2
# NOAA-15
satid=15

rm -f rtcoef_noaa_${satid}_amsua.dat profiles2_fmt mietable_noaa_amsua.dat
ln -s $DATART7/rtcoef_noaa_${satid}_amsua.dat rtcoef_noaa_${satid}_amsua.dat 
ln -s $DATARTSC/mietable_noaa_amsua.dat mietable_noaa_amsua.dat
ln -s $DATAIN/profiles2_fmt profiles2_fmt

echo '===========================================================================  '
echo '= Start of RTTOVSCATT test'
echo '= '
echo '= Tests are performed for AMSU-A onboard NOAA-15,'
echo '=   but any microwave instrument could be used by RTTOVSCATT'
echo '===========================================================================  '

rttovscatt_test.out << eof
1
15
3
45
eof
mv outputscatt.ascii ${TEST}/outputscatt_amsu7.lst
diff ${TEST}/outputscatt_amsu7.lst ${TEST_REF7}/outputscatt_amsu7.lst > ${TEST}/outputscatt_amsu7.diff

rm -f rtcoef_noaa_${satid}_amsua.dat
rm -f mietable_noaa_amsua.dat
rm -f profiles2_fmt
#############################################################################################
exit
