#!/bin/ksh
#
# Runs serial or parallel test and stuffs results into specified directory on 
# loquat.  Directory must already exist.  
#
# Usage for serial test:  
#   test_and_scp.ksh target_directory_on_loquat
# Usage for parallel test:  
#   test_and_scp.ksh target_directory_on_loquat num_tasks
#

#set -xu 
set -u 

# set up functions
function ErrorExit {
  typeset error_message=$1 ; shift
  print "ERROR:  \"${error_message}\""
  exit 30
}
function ErrorNoExit {
  typeset error_message=$1 ; shift
  print "ERROR:  \"${error_message}\"  CONTINUING..."
}


# parse command line
numArgs=$#
num_tasks=1   # default for serial run
if (( $numArgs == 2 )) ; then
  targetdir=$1 ; shift
  num_tasks=$1 ; shift
  testtype="parallel"
elif (( $numArgs == 1 )) ; then
  targetdir=$1 ; shift
  testtype="serial"
else
  ErrorExit "requires one or two arguments, you provided ${numArgs}"
fi

initdir=$( pwd )

adout="ad_d01_2005-07-16_00:00:00"
wrfout="wrfout_d01_2005-07-16_00:00:00"
namelist="namelist.input"
outfilescp="wrf_ad.out"
errfilescp="wrf_ad.error"
testoutscp="compare_vs_baseline.out"
if [[ $testtype = "serial" ]] ; then
  outfile=$outfilescp
  errfile=$errfilescp
  testout=$testoutscp
  jobscript="run_wrfplus.serial.ksh"
  job_name="wrfplus.serial"
else
  outfile="wrf_ad.${num_tasks}.out"
  errfile="wrf_ad.${num_tasks}.error"
  testout="compare_vs_baseline.${num_tasks}.out"
  jobscript="run_wrfplus.${num_tasks}.ksh"
  job_name="wrfplus.${num_tasks}"
fi
class="regular"
wall_clock_limit="00:08"
account="64000400"

# prepare for run
/bin/rm -f $adout $wrfout $outfile $errfile $testout $jobscript
if [[ $testtype = "parallel" ]] ; then
  /bin/rm -f rsl.out.???? rsl.error.???? field.*.*
fi

# write LSF job script
cat >> $jobscript << EOF4
#!/bin/ksh
#
#BSUB -J ${job_name}
#BSUB -x
#BSUB -q ${class}
#BSUB -n ${num_tasks}
#BSUB -W ${wall_clock_limit}
#BSUB -P ${account}
#BSUB -o ${outfile}
#BSUB -e ${errfile}
cd ${initdir}
EOF4
if [[ $testtype = "serial" ]] ; then
  print "wrfplus.exe" >> $jobscript
else
  print "mpirun.lsf wrfplus.exe" >> $jobscript
fi
chmod a+x ${jobscript} || ErrorExit "failed to chmod job script ${jobscript}"

# run wrfplus
bsub -K < ${jobscript}

# test
cd ../../../../../../..
if [[ $testtype = "parallel" ]] ; then
  compare_vs_baseline.ksh $num_tasks > ${initdir}/${testout} 2>&1 || \
    ErrorNoExit "compare_vs_baseline.ksh failed"
else
  compare_vs_baseline.ksh > ${initdir}/${testout} 2>&1 || \
    ErrorNoExit "compare_vs_baseline.ksh failed"
fi
cd $initdir

# transfer ASCII output files
scp $errfile hender@loquat.mmm.ucar.edu:${targetdir}/${errfilescp} || \
  ErrorNoExit "failed to scp file ${errfile}"
scp $outfile hender@loquat.mmm.ucar.edu:${targetdir}/${outfilescp} || \
  ErrorNoExit "failed to scp file ${outfile}"
scp $testout hender@loquat.mmm.ucar.edu:${targetdir}/${testoutscp} || \
  ErrorNoExit "failed to scp file ${testout}"
scp $namelist hender@loquat.mmm.ucar.edu:${targetdir} || \
  ErrorNoExit "failed to scp file ${namelist}"
if [[ $testtype = "parallel" ]] ; then
  scp $jobscript hender@loquat.mmm.ucar.edu:${targetdir} || \
    ErrorNoExit "failed to scp file ${jobscript}"
  scp rsl.out.0000 hender@loquat.mmm.ucar.edu:${targetdir} || \
    ErrorNoExit "failed to scp file rsl.out.0000"
  scp rsl.error.0000 hender@loquat.mmm.ucar.edu:${targetdir} || \
    ErrorNoExit "failed to scp file rsl.error.0000"
  for fieldfile in $( ls -1 field.* ) ; do
    scp $fieldfile hender@loquat.mmm.ucar.edu:${targetdir} || \
      ErrorNoExit "failed to scp file ${fieldfile}"
  done
fi

