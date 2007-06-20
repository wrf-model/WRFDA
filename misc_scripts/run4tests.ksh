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
#    run4tests.ksh compiler_optimization parallel_HALO_3903_OPT B
# Allowed values for compiler_optimization are:  
#   "o3"       Use executable built with "O3" compiler optimzation.  
#   "default"  Use executable built with default compiler optimization.  
#   "nobench"  Use executable built with default optimization and 
#              benchmark timers turned off.  
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
if (( $numArgs == 3 )) ; then
  compiler_optimization=$1 ; shift
  test_name=$1 ; shift
  test_version=$1 ; shift
else
  ErrorExit "requires three arguments, you provided ${numArgs}"
fi

# remove duplication with test_and_scp.ksh
if [[ $compiler_optimization = "o3" ]] ; then
  print "Using executable built with -O3 -qhot ..."
elif [[ $compiler_optimization = "default" ]] ; then
  print "Using executable built with default compiler optimization ..."
elif [[ $compiler_optimization = "nobench" ]] ; then
  print "Using executable built with default compiler optimization and benchmark timers turned off..."
else
  ErrorExit "valid values of compiler_optimization are \"o3\", \"default\", and \"nobench\" you provided \"${compiler_optimization}\""
fi

targetdir="/loquat2/hender/Tasks/4DVAR_Optimization/TEST_RESULTS/${test_name}/${compiler_optimization}_opt"

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
test_and_scp.ksh $compiler_optimization $opt16dir 16
# 1-task
test_and_scp.ksh $compiler_optimization $opt1dir 1

# unoptimized
/usr/bin/cp -f namelist.input.NO_OPT namelist.input
# 16-task
test_and_scp.ksh $compiler_optimization $noopt16dir 16
# 1-task
test_and_scp.ksh $compiler_optimization $noopt1dir 1

