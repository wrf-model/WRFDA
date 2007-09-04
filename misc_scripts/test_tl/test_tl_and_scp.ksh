#!/bin/ksh
#
# Runs Xin's canned parallel TL test and stuffs results into specified 
# directory on loquat.  Directory must already exist.  
#
# Usage:  
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
else
  ErrorExit "requires two arguments, you provided ${numArgs}"
fi

initdir=$( pwd )

tlout="tl_d01_2005-07-16"
wrfout="wrfout_d01_2005-07-16_00:00:00"
namelist="namelist.input"
namelistout="namelist.output"
outfile="wrf_tl.${num_tasks}.out"
errfile="wrf_tl.${num_tasks}.error"
outfilescp="wrf_tl.out"
errfilescp="wrf_tl.error"
jobscript="run_tl.${num_tasks}.ksh"
job_name="wrf_tl.${num_tasks}"
class="debug"
wall_clock_limit="00:08"
account="64000400"

baselinedir="${num_tasks}proc_me"

# prepare for run
/bin/rm -f ${tlout}* $wrfout $outfile $errfile $jobscript $namelistout
/bin/rm -f rsl.out.???? rsl.error.???? field.*.*
# remove this later...
/bin/rm -f auxhist3_d01_2005-07-16_06:00:00

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
mpirun.lsf wrfplus.exe
EOF4
chmod a+x ${jobscript} || ErrorExit "failed to chmod job script ${jobscript}"

# run wrfplus
bsub -K < ${jobscript}

# compare vs. baseline run with same number of tasks (even if baseline is wrong)
print
print "compare vs. baseline ${num_tasks}-task(s) run"
for tlfile in $( ls -1 ${tlout}* ) ; do
  cmd="cmp -l ${baselinedir}/${tlfile} ${tlfile}"
  print $cmd
  $( $cmd ) | wc
done
print
print "compare vs. baseline 1-task run"
# compare vs. 1-task baseline run
for tlfile in $( ls -1 ${tlout}* ) ; do
  cmd="cmp -l 1proc_me/${tlfile} ${tlfile}"
  print $cmd
  $( $cmd ) | wc
done

# transfer ASCII output files
scp $errfile hender@loquat.mmm.ucar.edu:${targetdir}/${errfilescp} || \
  ErrorNoExit "failed to scp file ${errfile}"
scp $outfile hender@loquat.mmm.ucar.edu:${targetdir}/${outfilescp} || \
  ErrorNoExit "failed to scp file ${outfile}"
scp $namelist hender@loquat.mmm.ucar.edu:${targetdir} || \
  ErrorNoExit "failed to scp file ${namelist}"
  scp $jobscript hender@loquat.mmm.ucar.edu:${targetdir} || \
    ErrorNoExit "failed to scp file ${jobscript}"
  for rslfile in $( ls -1 rsl.* ) ; do
    scp $rslfile hender@loquat.mmm.ucar.edu:${targetdir} || \
      ErrorNoExit "failed to scp file ${rslfile}"
  done
  for fieldfile in $( ls -1 field.* ) ; do
    scp $fieldfile hender@loquat.mmm.ucar.edu:${targetdir} || \
      ErrorNoExit "failed to scp file ${fieldfile}"
  done

