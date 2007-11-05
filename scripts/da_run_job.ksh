#!/bin/ksh

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

export WORK_DIR=$RUN_DIR/working

rm -rf $RUN_DIR
mkdir -p $RUN_DIR
cd $RUN_DIR

if test $SUBMIT = "LoadLeveller"; then 
   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1
   export POE=true

   cat > job.ksh <<EOF
#!/bin/ksh
# @ job_name         = ${RELEASE}_${REGION}_${EXPT}_${RUN}
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
$SCRIPT > $RUN_DIR/index.html 2>&1
EOF
elif test $SUBMIT = "LSF"; then 
   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB -J ${RELEASE}_${REGION}_${EXPT}_${RUN}          
#BSUB -q $QUEUE 
#BSUB -n $NUM_PROCS              
#BSUB -o job.output               
#BSUB -e job.error   
#BSUB -W $WALLCLOCK       
#BSUB -P $PROJECT        
#BSUB -R "span[ptile=$LL_PTILE]"
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
$SCRIPT > $RUN_DIR/index.html 2>&1

EOF
elif test $SUBMIT = "PBS"; then 
   # Rather simplistic node calculation
   export TEMP=$NUM_PROCS
   if test $TEMP -gt 4; then
      TEMP=4
   fi
   typeset -L15 JOBNAME=${RELEASE}_${REGION}_${EXPT}_${RUN}
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
$SCRIPT > $RUN_DIR/index.html 2>&1

EOF
elif test $SUBMIT = none; then
   if test -f $HOSTS; then
      export RUN_CMD_DEFAULT="mpirun -machinefile $HOSTS -np $NUM_PROCS"
   else
      export RUN_CMD_DEFAULT="mpirun -np $NUM_PROCS"
   fi
   cat > job.ksh <<EOF
#!/bin/ksh
# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="$RUN_CMD_DEFAULT"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
$SCRIPT > $RUN_DIR/index.html 2>&1
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

echo "Running with $NUM_PROCS processors, output to $RUN_DIR"
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
