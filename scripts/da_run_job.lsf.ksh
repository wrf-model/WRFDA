#!/bin/ksh

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

if test ! -d $EXP_DIR; then mkdir -p $EXP_DIR; fi

#-----------------------------------------------------------------------
# [2] Setup run:
#-----------------------------------------------------------------------

cat > job_${JOBNAME}.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB $LSF_EXCLUSIVE     
############BSUB -a mpich_gm      
#BSUB -a poe 
#BSUB -n $NUM_PROCS              
#BSUB -J $JOBNAME
#BSUB -o $JOBNAME.out               
#BSUB -e $JOBNAME.err               
#BSUB -q $QUEUE 
#BSUB -P $PROJECT_ID
#BSUB -W $LSF_MAX_RUNTIME
#BSUB -R "span[ptile=$LL_PTILE]"
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3
$SUBMIT_OPTIONS4

export RUN_CMD="mpirun.lsf"
. $SCRIPT > $EXP_DIR/index_${JOBNAME}.html 2>&1

EOF

chmod +x job_${JOBNAME}.ksh

bsub -q $QUEUE -n $NUM_PROCS < $PWD/job_${JOBNAME}.ksh > $PWD/bsubjob_${JOBNAME}.log

exit 0

