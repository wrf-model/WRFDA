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
export POE=false
export CHECK_SVNVERSION=${CHECK_SVNVERSION:-true}

export PLATFORM=`uname`
export HOSTNAME=`hostname`
export NUM_PROCS=${NUM_PROCS:-1}

export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-60} # minutes
export LSF_EXCLUSIVE=${LSF_EXCLUSIVE:--x}
export LL_WALL_CLOCK_LIMIT=${LL_WALL_CLOCK_LIMIT:-01:30:00}
export LL_NODE_USAGE=${LL_NODE_USAGE:-shared}
export LL_PTILE=${LL_PTILE:-8}
export QUEUE=${QUEUE:-regular}
export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
export HOSTS=${HOSTS:-$PWD/hosts}

mkdir -p $RUN_DIR
cd $RUN_DIR

if test $HOSTNAME = "bs1101en" -o $HOSTNAME = "bs1201en"; then 
   # bluesky uses loadleveller

   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1
   export POE=true

   cat > job.ksh <<EOF
#!/bin/ksh
# @ job_type         = parallel
# @ environment      = COPY_ALL
# @ job_name         = $EXPT
# @ output           = job.output
# @ error            = job.error
# @ node             = $NODES
# @ notification     = never
# @ network.MPI      = css0,shared,ip
# @ total_tasks      = $NUM_PROCS
# @ node_usage       = $LL_NODE_USAGE
# @ checkpoint       = no
# @ wall_clock_limit = $LL_WALL_CLOCK_LIMIT
# NCEP IBM=dev
# NCAR IBM(bluesky)=com_rg8:
# @ class      =  share
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
# @ queue

export RUN_CMD="$DEBUGGER " # Space important
. $SCRIPT > $EXP_DIR/index.html 2>&1
EOF
elif test $HOSTNAME = "ln0126en" -o $HOSTNAME = "ln0127en" \
   -o $HOSTNAME = "bv1103en.ucar.edu" \
   -o $HOSTNAME = "bv1203en.ucar.edu" ; then 
   # lightning and bluesky use lsf

   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB $LSF_EXCLUSIVE     
#BSUB -a mpich_gm      
#BSUB -n $NUM_PROCS              
#BSUB -J $EXPT                   
#BSUB -o job.output               
#BSUB -e job.error               
#BSUB -q $QUEUE 
#BSUB -W $LSF_MAX_RUNTIME
#BSUB -R "span[ptile=$LL_PTILE]"
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3


# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="mpirun.lsf"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
. $SCRIPT > $EXP_DIR/index.html 2>&1

EOF
elif test $HOSTNAME = willow -o $HOSTNAME = hazel -o $HOSTNAME = goldenrain ; then
   cat > job.ksh <<EOF
#!/bin/ksh
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT=" "
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
$SCRIPT > $EXP_DIR/index.html 2>&1
EOF
elif test $HOSTNAME = ocotillo -o $HOSTNAME = snowdrift ; then
   cat > job.ksh <<EOF
#!/bin/ksh
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="mpirun -v -np $NUM_PROCS -nolocal -machinefile $HOSTS"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
$SCRIPT > $EXP_DIR/index.html 2>&1
EOF
else
   cat > job.ksh <<EOF
#!/bin/ksh
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="mpirun -v -np $NUM_PROCS -all-local -machinefile $HOSTS"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
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
if test $HOSTNAME = "bs1101en" -o $HOSTNAME = "bs1201en"; then 
   llsubmit job.ksh
elif test $HOSTNAME = "ln0126en" -o $HOSTNAME = "ln0127en" \
     -o $HOSTNAME = "bv1103en.ucar.edu" \
     -o $HOSTNAME = "bv1203en.ucar.edu"; then 
   bsub -q $QUEUE -n $NUM_PROCS < $PWD/job.ksh
elif test $HOSTNAME = ocotillo; then
   ./job.ksh
else
   ./job.ksh
fi

exit 0
