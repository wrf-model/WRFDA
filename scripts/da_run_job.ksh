#!/bin/ksh

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export WRF_DIR=${WRF_DIR:-$REL_DIR/wrf}
export WRF_NL_DIR=${WRF_NL_DIR:-$REL_DIR/wrfvar_wrf_nl}
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
# @ output           = $EXPT.e\$(jobid)
# @ error            = $EXPT.o\$(jobid)
# @ node             = $NODES
# @ notification     = never
# @ network.MPI      = css0,shared,ip
# @ total_tasks      = $NUM_PROCS
# @ node_usage       = shared
# @ checkpoint       = no
# @ wall_clock_limit = 01:30:00
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
# LSF batch script to run a serial code
#
#BSUB -x       
#BSUB -a mpich_gm      
#BSUB -n $NUM_PROCS              
#BSUB -J $EXPT                   
#BSUB -o $EXPT.out               
#BSUB -e $EXPT.err               
#BSUB -q regular  
#BSUB -W 60
$SUBMIT_OPTIONS1
$SUBMIT_OPTIONS2
$SUBMIT_OPTIONS3


# Cannot put - options inside default substitution
export RUN_CMD_DEFAULT="mpirun.lsf"
export RUN_CMD="${RUN_CMD:-\$RUN_CMD_DEFAULT}"
. $SCRIPT > $EXP_DIR/index.html 2>&1

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
   export WRF_NL_VN=`svnversion -n \$WRF_NL_DIR 2>/dev/null`
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
   bsub -q regular -n $NUM_PROCS < $PWD/job.ksh
elif test $HOSTNAME = ocotillo; then
   ./job.ksh
else
   ./job.ksh
fi

exit 0
