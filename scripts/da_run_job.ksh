#!/bin/ksh
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export WRFPLUS_DIR=${WRFPLUS_DIR:-$REL_DIR/wrfplus}
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run}
export WORK_DIR=$RUN_DIR/working
export SCRIPT=${SCRIPT:-$WRFVAR_DIR/scripts/da_run_wrfvar.ksh}
export SUBMIT=${SUBMIT:-none}
export CHECK_SVNVERSION=${CHECK_SVNVERSION:-false}

export PLATFORM=`uname`
export MACHINE=${MACHINE:-`hostname`}
export NUM_PROCS=${NUM_PROCS:-1}

export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-60} # minutes
export LSF_EXCLUSIVE=${LSF_EXCLUSIVE:--x}
export LL_WALL_CLOCK_LIMIT=${LL_WALL_CLOCK_LIMIT:-01:30:00}
export LL_NODE_USAGE=${LL_NODE_USAGE:-not_shared}
export LL_PTILE=${LL_PTILE:-8}
export LL_CLASS=${LL_CLASS:-com_rg8}
export QUEUE=${QUEUE:-regular}
export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-YES}
export RUN_CMD=`eval echo $RUN_CMD`

mkdir -p $RUN_DIR
cd $RUN_DIR

if test $SUBMIT = LoadLeveller; then 
   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1

   cat > job.ksh <<EOF
#!/bin/ksh
# @ job_type         = parallel
# @ environment      = COPY_ALL
# @ job_name         = $EXPT
# @ output           = $EXPT.e\$(jobid)
# @ error            = $EXPT.o\$(jobid)
# @ node             = $NODES
# @ notification     = never
# @ network.MPI      = css0,shared,ip
# @ total_tasks      = $NUM_PROCS
# @ node_usage       = $LL_NODE_USAGE
# @ checkpoint       = no
# @ wall_clock_limit = $LL_WALL_CLOCK_LIMIT
# NCEP IBM=dev
# @ class            = ${LL_CLASS}
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
# @ queue            = $QUEUE

export RUN_CMD="$DEBUGGER "

. $SCRIPT > $EXP_DIR/index.html 2>&1
EOF

elif test $SUBMIT = LSF; then
   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB $LSF_EXCLUSIVE     
#BSUB -a mpich_gm      
#BSUB -n $NUM_PROCS              
#BSUB -J $EXPT                   
#BSUB -o $EXPT.out               
#BSUB -e $EXPT.err               
#BSUB -q $QUEUE 
#BSUB -W $LSF_MAX_RUNTIME
#BSUB -R "span[ptile=$LL_PTILE]"
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3

export RUN_CMD=mpirun.lsf

. $SCRIPT > $EXP_DIR/index.html 2>&1

EOF
else
   if test $HOSTS'.' != '.'; then
      RUN_CMD="mpirun -v -np $NUM_PROCS -nolocal -machinefile $HOSTS"
   else
      RUN_CMD="mpirun -v -np $NUM_PROCS -all-local"
   fi
   cat > job.ksh <<EOF
#!/bin/ksh
$SCRIPT > $EXP_DIR/index.html 2>&1
EOF
fi

if $CHECK_SVNVERSION; then
   export WRF_VN=`svnversion -n \$WRF_DIR 2>/dev/null`
   export WRFVAR_VN=`svnversion -n \$WRFVAR_DIR 2>/dev/null`
   export WRFPLUS_VN=`svnversion -n \$WRFPLUS_DIR 2>/dev/null`
   export WPS_VN=`svnversion -n \$WPS_DIR 2>/dev/null`
fi

cat >> job.ksh <<EOF
RC=\$?
if test \$RC = 0; then
   echo Succeeded
else
   echo Failed with error \$RC
fi
EOF

chmod +x job.ksh

echo "Running with $NUM_PROCS processors, output to $EXP_DIR"
if test $SUBMIT = LoadLeveller; then
   llsubmit job.ksh
elif test $SUBMIT = LSF; then
   bsub -q $QUEUE -n $NUM_PROCS < $PWD/job.ksh
else
   ./job.ksh
fi

exit 0
