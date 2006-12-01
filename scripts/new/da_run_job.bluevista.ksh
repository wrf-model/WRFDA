#!/bin/ksh
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
if test ! -d $EXP_DIR; then mkdir -p $EXP_DIR; fi

export SCRIPT=${SCRIPT:-$WRFVAR_DIR/scripts/da_run_wrfvar.ksh}

export LSF_EXCLUSIVE=${LSF_EXCLUSIVE:--x}
export NUM_PROCS=${NUM_PROCS:-1}
export QUEUE=${QUEUE:-regular}
export LSF_MAX_RUNTIME=${LSF_MAX_RUNTIME:-10} # minutes
export LL_PTILE=${LL_PTILE:-1} # minutes

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
#BSUB -P $PROJECT_ID
#BSUB -W $LSF_MAX_RUNTIME
#BSUB -R "span[ptile=$LL_PTILE]"

export RUN_CMD="mpirun.lsf"
. $SCRIPT > $EXP_DIR/index.html 2>&1

EOF

chmod +x job.ksh

bsub -q $QUEUE -n $NUM_PROCS < $PWD/job.ksh

exit 0

