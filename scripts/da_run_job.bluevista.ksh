#!/bin/ksh

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

if test ! -d $EXP_DIR; then mkdir -p $EXP_DIR; fi

#-----------------------------------------------------------------------
# [2] Setup run:
#-----------------------------------------------------------------------

cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
#BSUB $LSF_EXCLUSIVE     
############BSUB -a mpich_gm      
#BSUB -a poe 
#BSUB -n $NUM_PROCS              
#BSUB -J $EXPT                   
#BSUB -o $EXPT.out               
#BSUB -e $EXPT.err               
#BSUB -q $QUEUE 
#BSUB -P $PROJECT_ID
#BSUB -W $LSF_MAX_RUNTIME
#BSUB -R "span[ptile=$LL_PTILE]"
############BSUB -w \"done(${PREV_JOBID})\"

export RUN_CMD="mpirun.lsf"
. $SCRIPT > $EXP_DIR/index.html 2>&1

EOF

chmod +x job.ksh

bsub -q $QUEUE -n $NUM_PROCS < $PWD/job.ksh > $PWD/bsubjob_${EXPT}.log

exit 0

