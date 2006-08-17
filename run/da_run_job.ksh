#!/bin/ksh

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export SCRIPT=${SCRIPT:-$WRFVAR_DIR/run/da_run_wrfvar.ksh}

export PLATFORM=`uname`
export HOSTNAME=`hostname`
export NUM_PROCS=${NUM_PROCS:-1}

export MP_SHARED_MEMORY=${MP_SHARED_MEMORY:-yes}
export MPI_RUN=${MPI_RUN:-mpirun}
export HOSTS=${HOSTS:-$PWD/hosts}
export MPI_OPTS=${MPI_OPTS:--v -np $NUM_PROCS -nolocal -machinefile $HOSTS}

rm -rf $EXP_DIR
mkdir $EXP_DIR
cd $EXP_DIR

if test $HOSTNAME = "bs1101en" -o $HOSTNAME = "bs1201en"; then 
   # bluesky used loadleveller

   # Rather simplistic node calculation 
   let TEMP=$NUM_PROCS-1
   let NODES=$TEMP/8+1

   cat > job.ksh <<EOF
#!/bin/ksh
##@ network.MPI=csss,shared,us
#IBM:
# @ job_type   = parallel
# @ environment = COPY_ALL
# @ job_name   = $EXPT
# @ output     = $EXPT.e\$(jobid)
# @ error      = $EXPT.o\$(jobid)
# @ node       = $NODES
# @ notification = never
## @ network.MPI    = css0,shared,us
# @ network.MPI    = css0,shared,ip
# @ total_tasks = $NUM_PROCS
# @ node_usage = shared
# @ checkpoint = no
# @ wall_clock_limit = 01:30:00
# NCEP IBM=dev
# NCAR IBM(bluesky)=com_rg8:
# NCAR IBM(blackforest)=com_reg:
# NCAR IBM(blackforest_nighthawk)=com_nh:
# @ class      =  share
## @ class      =  com_rg8
# @ queue
#
#FSL JET (Alpha/Linux):
#PBS -V -A sfmlidar
#PBS -lnodes=4:comp -lwalltime=1000
#Uncomment for JET: source /usr/local/bin/setup-mpi.csh
export RUN_CMD="$DEBUGGER " # Space important
. $SCRIPT
EOF

   llsubmit job.ksh
elif test $HOSTNAME = "ln0126en" -o $HOSTNAME = "ln0127en"; then 
   # lightning uses lsf
   cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script to run a serial code
#
##BSUB -P 93300070  
#BSUB -x       
#BSUB -a mpich_gm      
#BSUB -n $NUM_PROCS              
#BSUB -J $EXPT                   
#BSUB -o $EXPT.out               
#BSUB -e $EXPT.err               
#BSUB -q regular  

export RUN_CMD="mpirun.lsf -v -np $NUM_PROCS}"
. $SCRIPT

EOF
   chmod +x job.ksh
   bsub -q regular -n $NUM_PROCS $PWD/job.ksh

elif test $PLATFORM = "Darwin"; then
   export RUN_CMD="mpirun -v -np $NUM_PROCS -all-local}"
   $SCRIPT

else
   $SCRIPT
fi

exit 0
