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

export SUBMIT=${SUBMIT:-LSF}
export NUM_PROCS=${NUM_PROCS:-1}
export HOSTS=${HOSTS:-$HOME/hosts}

export QUEUE=${QUEUE:-regular}
export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}

mkdir -p $RUN_DIR
cd $RUN_DIR

if test $SUBMIT = "LoadLeveller"; then 
   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1
   export POE=true

   cat > job.ksh <<EOF
#!/bin/ksh
# @ job_name         = ${REGION}_${EXPT}
# @ total_tasks      = $NUM_PROCS
# @ node             = $NODES
# @ output           = job.output
# @ error            = job.error
# @ wall_clock_limit = $WALLCLOCK
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10
# @ queue            = $QUEUE

export RUN_CMD="$DEBUGGER " # Space important
$SCRIPT > $EXP_DIR/index.html 2>&1
EOF
elif test $SUBMIT = "LSF"; then 
   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB -J ${REGION}_${EXPT}             
#BSUB -q $QUEUE 
#BSUB -n $NUM_PROCS              
#BSUB -o job.output               
#BSUB -e job.error   
#BSUB -W $WALLCLOCK       
#BSUB -P $PROJECT        
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10

# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="mpirun.lsf"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
$SCRIPT > $EXP_DIR/index.html 2>&1

EOF
elif test $SUBMIT = "PBS"; then 
   # Rather simplistic node calculation
   export TEMP=$NUM_PROCS
   if test $TEMP -gt 4; then
      TEMP=4
   fi
   typeset -L15 JOBNAME=${REGION}_${EXPT}
   cat > job.ksh <<EOF
#!/bin/ksh
#
# PBS batch script
#
#PBS -N JOBNAME
##PBS -q $QUEUE
#PBS -l mppe=$NUM_PROCS
#PBS -l walltime=$WALLCLOCK
#PBS -o job.output
#PBS -e job.error
#PBS -V
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4
$SUBMIT_OPTIONS5
$SUBMIT_OPTIONS6
$SUBMIT_OPTIONS7
$SUBMIT_OPTIONS8
$SUBMIT_OPTIONS9
$SUBMIT_OPTIONS10

# Options for Cray X1 
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="aprun -m exclusive -N$TEMP -n$NUM_PROCS"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
$SCRIPT > $EXP_DIR/index.html 2>&1

EOF
elif test $SUBMIT = none; then
   if test -f $HOSTS; then
      export RUN_CMD_DEFAULT="mpirun -np $NUM_PROCS -machinefile $HOSTS"
   else
      export RUN_CMD_DEFAULT="mpirun -np $NUM_PROCS"
   fi
   cat > job.ksh <<EOF
#!/bin/ksh
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="$RUN_CMD_DEFAULT"
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
echo `date +'%D %T'` "Ended $RC"
EOF

chmod +x job.ksh

echo "Running with $NUM_PROCS processors, output to $EXP_DIR"
if test $SUBMIT = "LoadLeveller"; then 
   llsubmit job.ksh
elif test $SUBMIT = "LSF"; then 
   bsub < $PWD/job.ksh
elif test $SUBMIT = "PBS"; then 
   qsub $PWD/job.ksh
else
   ./job.ksh
fi

exit 0
