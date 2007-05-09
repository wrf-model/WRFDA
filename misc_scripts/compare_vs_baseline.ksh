#!/bin/ksh
#
# Compare results with baseline for a serial run:  
#   compare_vs_baseline.ksh
#
# Compare results with baseline for a parallel run that used "N" tasks:  
#   compare_vs_baseline.ksh N
#

#set -xu 
set -u 

# set up functions
function ErrorExit {
  typeset error_message=$1 ; shift
  print "ERROR:  ${error_message}"
  exit 30
}

# parse command line
numArgs=$#
num_tasks=1   # default for serial run
if (( $numArgs == 1 )) ; then
  num_tasks=$1 ; shift
  testtype="parallel"
elif (( $numArgs == 0 )) ; then
  testtype="serial"
else
  ErrorExit "requires zero or one arguments, you provided ${numArgs}"
fi

baseline_dir="cwb_BASELINE"
dut_dir="cwb"

baseline_ad_dir="${baseline_dir}/test/run/2005071600/wrfvar/working/ad"
dut_ad_dir="${dut_dir}/test/run/2005071600/wrfvar/working/ad"

adout="ad_d01_2005-07-16_00:00:00"
baseline_adout="${baseline_ad_dir}/${adout}"
dut_adout="${dut_ad_dir}/${adout}"
if [[ $testtype = "serial" ]] ; then
  ad_stdout="wrf_ad.out"
else
  ad_stdout="rsl.out.0000"
fi
baseline_ad_stdout="${baseline_ad_dir}/${ad_stdout}"
dut_ad_stdout="${dut_ad_dir}/${ad_stdout}"
wrfout="wrfout_d01_2005-07-16_00:00:00"
baseline_wrfout="${baseline_ad_dir}/${wrfout}"
dut_wrfout="${dut_ad_dir}/${wrfout}"

# check for files
for fil in $dut_adout $dut_ad_stdout ; do
  thiswait=1
  maxwait=10
  until (( thiswait > $maxwait )) ; do
    if [[ ! -f ${fil} ]] ; then
      print "Waiting for $fil , wait count = ${thiswait}"
      sleep 3
    else
      thiswait=$maxwait
    fi
    (( thiswait += 1 ))
  done
  if [[ ! -f ${fil} ]] ; then
    ErrorExit "failed to find file ${fil}"
  fi
done

baseline_stdout="baseline_${ad_stdout}"
dut_stdout="dut_${ad_stdout}"

\rm -f $baseline_stdout $dut_stdout

# diff stdout files
print "=================="
print "Diff stdout files:"
print "=================="
print "grep -v Timing ${baseline_ad_stdout} > $baseline_stdout"
print "grep -v Timing ${dut_ad_stdout} > $dut_stdout"
grep -v Timing ${baseline_ad_stdout} > $baseline_stdout
grep -v Timing ${dut_ad_stdout} > $dut_stdout
diffcmd="diff ${baseline_stdout} ${dut_stdout}"
print $diffcmd
${diffcmd}

# cmp netCDF output files
print "\n===================="
print "cmp ad_d01* files:"
print "===================="
printcmd="ls -al $baseline_adout $dut_adout"
print $printcmd
$printcmd
cmpcmd="cmp -l ${baseline_adout} ${dut_adout}"
print "\n${cmpcmd} | wc"
${cmpcmd} | wc
if [[ $testtype = "serial" ]] ; then
  print "\n===================="
  print "cmp wrfout_d01* files:"
  print "===================="
  printcmd="ls -al $baseline_wrfout $dut_wrfout"
  print $printcmd
  $printcmd
  cmpcmd="cmp -l ${baseline_wrfout} ${dut_wrfout}"
  print "\n${cmpcmd} | wc"
  ${cmpcmd} | wc
fi

