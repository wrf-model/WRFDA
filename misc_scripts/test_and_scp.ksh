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
  print "ERROR:  ${error_message}"
  exit 30
}


# parse command line
numArgs=$#
num_tasks=1
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
else
  outfile="wrf_ad.${num_tasks}.out"
  errfile="wrf_ad.${num_tasks}.error"
  testout="compare_vs_baseline.${num_tasks}.out"
  jobscript="run_wrfplus.${num_tasks}.ksh"
  parexechome="../../../../../../../../wrfplus_WORK_parallel/main/wrfplus.exe"
  parexeclocal="wrfplus_parallel.exe"
  job_name="wrfplus.${num_tasks}"
  class="share"
  wall_clock_limit="00:08"
  account="64000400"
fi

# prepare for run
/bin/rm -f $adout $wrfout $outfile $errfile $testout
if [[ $testtype = "parallel" ]] ; then
  /bin/rm -f rsl.out.???? rsl.error.???? $parexeclocal $jobscript
  # link in parallel executable
  if [[ -f $parexechome ]] ; then
    ln -s $parexechome $parexeclocal || \
      ErrorExit "failed to link parallel executable"
  else
    ErrorExit "cannot find parallel executable ${parexechome}"
  fi
  # write LSF job script
cat >> $jobscript << EOF4
#!/bin/ksh
#
#BSUB -J ${job_name}
#BSUB -q ${class}
#BSUB -n ${num_tasks}
#BSUB -W ${wall_clock_limit}
#BSUB -P ${account}
#BSUB -o ${outfile}
#BSUB -e ${errfile}
cd ${initdir}
mpirun.lsf wrfplus_parallel.exe
EOF4
  chmod a+x ${jobscript} || ErrorExit "failed to chmod job script ${jobscript}"
fi

# run wrfplus
if [[ $testtype = "serial" ]] ; then
  print "BEGIN wrfplus.exe at $( date )"
  wrfplus.exe > $outfile 2> $errfile
  print "END wrfplus.exe at $( date )"
else
  touch wrf_go_ahead
  print "execute \"touch wrf_stop_now\" to stop wrfplus once it has finished"
  bsub -K < ${jobscript}
fi

# test
cd ../../../../../../..
compare_vs_baseline.ksh > ${initdir}/${testout} 2>&1 || \
  ErrorExit "compare_vs_baseline.ksh failed"
cd $initdir

# transfer ASCII output files
scp $errfile hender@loquat.mmm.ucar.edu:${targetdir}/${errfilescp} || \
  ErrorExit "failed to scp file ${errfile}"
scp $outfile hender@loquat.mmm.ucar.edu:${targetdir}/${outfilescp} || \
  ErrorExit "failed to scp file ${outfile}"
scp $testout hender@loquat.mmm.ucar.edu:${targetdir}/${testoutscp} || \
  ErrorExit "failed to scp file ${testout}"
scp $namelist hender@loquat.mmm.ucar.edu:${targetdir}/${namelist} || \
  ErrorExit "failed to scp file ${namelist}"
if [[ $testtype = "parallel" ]] ; then
  scp $jobscript hender@loquat.mmm.ucar.edu:${targetdir}/${jobscript} || \
    ErrorExit "failed to scp file ${jobscript}"
fi

