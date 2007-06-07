#!/bin/ksh
#
# Hack to automate running all four parallel tests and put results in a 
# "standard" location:  
#
#   1-task without memory vs. re-computation optimization
#  16-task without memory vs. re-computation optimization
#   1-task with memory vs. re-computation optimization
#  16-task with memory vs. re-computation optimization
#
#  Usage:  
#    run4tests.ksh parallel_HALO_3903_OPT B
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
if (( $numArgs == 2 )) ; then
  test_name=$1 ; shift
  test_version=$1 ; shift
else
  ErrorExit "requires two arguments, you provided ${numArgs}"
fi

targetdir="/loquat2/hender/Tasks/4DVAR_Optimization/TEST_RESULTS/${test_name}"

opt16dir="${targetdir}/16task_12345${test_version}"
opt1dir="${targetdir}/1task_12345${test_version}"
noopt16dir="${targetdir}/16task_${test_version}"
noopt1dir="${targetdir}/1task_${test_version}"

# print command to type on loquat (automate this later too)
print
print "type the following mkdir command on loquat:"
print
print "mkdir -p $opt16dir $opt1dir $noopt16dir $noopt1dir"
print

# optimized
/usr/bin/cp -f namelist.input.OPT namelist.input
# 16-task
test_and_scp.ksh $opt16dir 16
# 1-task
test_and_scp.ksh $opt1dir 1

# unoptimized
/usr/bin/cp -f namelist.input.NO_OPT namelist.input
# 16-task
test_and_scp.ksh $noopt16dir 16
# 1-task
test_and_scp.ksh $noopt1dir 1

