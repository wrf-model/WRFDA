#!/bin/ksh

export RELEASE=${RELEASE:-WRF_V2.1.2}
export REL_DIR=${REL_DIR:-$HOME/code_development/$RELEASE}
export DA_DIR=${DA_DIR:-$REL_DIR/wrfvar}
export REGION=${REGION:-con200}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-/data7/da/bray/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}

export PLATFORM=`uname`
export HOSTNAME=`hostname`
export NUM_PROCS=${NUM_PROCS:-1}

rm -rf $EXP_DIR
mkdir $EXP_DIR
cd $EXP_DIR

if test $HOSTNAME = "bs1101en" -o $HOSTNAME = "bs1201en"; then # bluesky

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
$DA_DIR/run/DA_Run_WRF-Var.csh
EOF

  llsubmit job.ksh
else
  $DA_DIR/run/DA_Run_WRF-Var.csh
fi

exit 0
