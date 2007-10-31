#!/bin/ksh
################################################################################
#
# Systematic test of RTTOVCLD (TL/AD/K) : loop over input choices
# Author : F Chevallier - 10/10/2003
#
################################################################################
################################################################################
#
# From output files print.dat_*, the user can check:
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
DATAIN=${DATAIN:=../data}               ; export DATAIN
DATART=${DATART:=../rtcoef_rttov7} 	      ; export DATART
TEST_REF=${TEST_REF:=../reftest}        ; export TEST_REF
TEST=${TEST:=../test}	                      ; export TEST

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
rm -f rtcoef_*.bin
#
ln -s $DATAIN/refprof.dat refprof.dat

rm -f profiles_fmt

echo
echo
echo " HIRS/AMSU NOAA15 IR cloud Ad "
#ln -s $DATAIN/input_ircloud.dat input.dat
ln -s $DATAIN/profiles_fmt profiles_fmt

# Tbs
tb=2
# NOAA-15
satid=15

echo '===========================================================================  '
echo '= Start of RTTOVCLD test'
echo '= '
echo '= Tests are performed for HIRS and AMSU-A onboard NOAA-15,'
echo '=   but any infrared or microwave instrument could be used by RTTOVCLD'
echo '=      Note that RTTOVCLD does not take into account scattering'
echo '=      For scattering in the microwave, use RTTOVSCATT'
echo '=  This test takes some time to run '
echo '===========================================================================  '

for instr in 0 3
#for instr in 3
do

if [ $instr -eq 0 ]; then

ln -s $DATART/rtcoef_noaa_${satid}_hirs.dat rtcoef_noaa_${satid}_hirs.dat 

for radip in 0 1 2 3
do
for kice  in 0 1
do
rttovcld_test.out << eof
$tb
$satid
$instr
$radip
$kice
eof
mv outputcld.ascii ${TEST}/outputcld_m${tb}_s${satid}_i${instr}_r${radip}_k${kice}.lst
diff ${TEST}/outputcld_m${tb}_s${satid}_i${instr}_r${radip}_k${kice}.lst       \
     ${TEST_REF}/outputcld_m${tb}_s${satid}_i${instr}_r${radip}_k${kice}.lst > \
     ${TEST}/outputcld_m${tb}_s${satid}_i${instr}_r${radip}_k${kice}.diff

done
done

rm -f  rtcoef_noaa_${satid}_hirs.dat

else

ln -s $DATART/rtcoef_noaa_${satid}_amsua.dat rtcoef_noaa_${satid}_amsua.dat

rttovcld_test.out << eof
$tb
$satid
$instr
eof
mv outputcld.ascii ${TEST}/outputcld_m${tb}_s${satid}_i${instr}.lst
diff ${TEST}/outputcld_m${tb}_s${satid}_i${instr}.lst ${TEST_REF}/outputcld_m${tb}_s${satid}_i${instr}.lst > \
     ${TEST}/outputcld_m${tb}_s${satid}_i${instr}.diff

rm -f rtcoef_noaa_${satid}_amsua.dat

fi

done

rm -f profiles_fmt
echo '### Tests completed ###'

################################################################################
################################################################################
