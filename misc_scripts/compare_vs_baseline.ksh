#!/bin/ksh
baseline_dir="cwb"
dut_dir="cwb_BASELINE"

baseline_ad_dir="${baseline_dir}/test/run/2005071600/wrfvar/working/ad"
dut_ad_dir="${dut_dir}/test/run/2005071600/wrfvar/working/ad"

ad_stdout="wrf_ad.out"
baseline_stdout="baseline_wrf_ad.out"
dut_stdout="dut_wrf_ad.out"

adout="ad_d01_2005-07-16_00:00:00"
baseline_adout="${baseline_ad_dir}/${adout}"
dut_adout="${dut_ad_dir}/${adout}"
wrfout="wrfout_d01_2005-07-16_00:00:00"
baseline_wrfout="${baseline_ad_dir}/${wrfout}"
dut_wrfout="${dut_ad_dir}/${wrfout}"

\rm -f $baseline_stdout $dut_stdout

# diff stdout files
print "=================="
print "Diff stdout files:"
print "=================="
print "grep -v Timing ${baseline_ad_dir}/${ad_stdout} > $baseline_stdout"
print "grep -v Timing ${dut_ad_dir}/${ad_stdout} > $dut_stdout"
grep -v Timing ${baseline_ad_dir}/${ad_stdout} > $baseline_stdout
grep -v Timing ${dut_ad_dir}/${ad_stdout} > $dut_stdout
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
print "\n===================="
print "cmp wrfout_d01* files:"
print "===================="
printcmd="ls -al $baseline_wrfout $dut_wrfout"
print $printcmd
$printcmd
cmpcmd="cmp -l ${baseline_wrfout} ${dut_wrfout}"
print "\n${cmpcmd} | wc"
${cmpcmd} | wc

