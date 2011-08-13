#! /bin/ksh
#-----------------------------------------------------------------------
# Script gen_be_wrapper_um.ksh for GEN_BE v1.0.0
#
# Purpose: Master script to calculate background error statistics for 
#          the UK MetO VAR system in limited area mode using forecast 
#          data from the Unified Model in LAM mode (uniform grid only).
#
# Note:    The parameters specified here override the default 
#          environment variables in 
#          $GEN_BE_DIR/scripts/gen_be_set_defaults.ksh
#-----------------------------------------------------------------------

export GEN_BE_DIR=$HOME/gen_be/v1.0.0
export MODEL="UM"

#[1] Define which jobs/components to run
export RUN_GEN_BE_STAGE0=true
export RUN_GEN_BE_STAGE1=true
export RUN_GEN_BE_STAGE2=true
export RUN_GEN_BE_STAGE2A=true
export RUN_GEN_BE_STAGE3=true
export RUN_GEN_BE_STAGE4=true
export RUN_GEN_BE_DIAGS=true
export RUN_GEN_BE_DIAGS_UKMO=true     # If true make sure the selected gen_be's options 
                                      # are compatible with VAR
export RUN_GEN_BE_DIAGS_READ=true
export RUN_GEN_BE_MULTICOV=true
export RUN_GEN_BE_GRAPHICS=true

#[2] Define background error data info
export BE_METHOD=NMC            # NMC or ENS
export FC_DIR=/home/mm0100/frfc/local/gen_be/nmc_hrtm/6h-3h/data  # Where UM forecasts are 
                                                                  # This local disk is on rld033

export START_DATE=2010031612    # the first perturbation validity time
export END_DATE=2010031718      # the last perturbation validity time

export FCST_RANGE1=06           # Long forecast lead time (in hour)
export FCST_RANGE2=03           # Short forecast lead time (in hour). Discard if using ENS
export INTERVAL=06              # Time interval (in hour) between forecast differences
#export NE=24                   # Ensemble Size (uncomment only in ENS mode)

# The variable below are adapted for forecast filenames :
# NMC: YYYYMMDD_${UMRUN}HH_${UMTAG}${FCST_RANGE1/2}${UMMN}.nc
# ENS: YYYYMMDD_${UMRUN}HH_${UMTAG}${FCST_RANGE1}${UMMN}.eMMM.nc                 
export UMRUN=Q1                 # UM cycle (Q1, Q4, EY, etc)
export UMTAG=qwainga_ca         # UM forecast tag name
export UMMN=30                  # UM forecast tag minute

export REGION=SUK               # Tag name for plots

export UM_NI=360                # UM grid WE dimension
export UM_NJ=288                # UM grid NS dimension
export NUM_LEVELS=70            # Number of vertical levels
export RESOLUTION_KM=1.5011854  # Grid resolution in KM

export UM_NPOLE_LAT=37.5        # UM rotated lat-lon grid parameters
export UM_NPOLE_LON=177.5       # UM rotated lat-lon grid parameters
export UM_LAT_SW_CORNER=-2.63   # UM rotated lat-lon grid parameters
export UM_LON_SW_CORNER=357.8   # UM rotated lat-lon grid parameters
export UM_DX_DEG=0.0135         # UM grid resolution in degree

export FILEPLEV="/home/mm0100/frfc/SUK_plevel.txt"  # Text file with pressure level heights
export FILETLEV="/home/mm0100/frfc/SUK_tlevel.txt"  # Text file with theta level heights

export CUT=8                    # Number of grid point to remove the edges
                                # Usually set = frame points + blending points

NIC=`bc << EOF
      scale=2
      $UM_NI - 2 * $CUT
EOF
     `
NJC=`bc << EOF
      scale=2
      $UM_NJ - 2 * $CUT
EOF
     `
export NUM_WE=$NIC             # don't modify
export NUM_SN=$NJC             # don't modify

#[3] Set gen_be options

# Options to match VAR in limited area mode 
export BIN_TYPE=5              # Homogeneous BE (i.e. domain average)
export MASSCV="pres"           # Pressure is used as CV for the mass field
export HUMCV="rh"              # Relative Humidity is used as CV for the humidity field
export BALPRES="linestats"     # Balance pressure derived from linear regression between
                               # pressure and a linear balance pressure (derived from Psi)
export NOBALDIV=".true."       # No balance component for divergence. i.e., Chi_u = Chi 
export VERTICAL_IP=1           # Inner product used in EOF (1: yes (default in VAR), 0: no)
export HORIZVAR="correl"       # Variable used in horizontal length scale computation       
export HORIZFUNCT="soar"       # Horizontal correlation function used

# Other options
export RUN_DIR=$HOME/local/gen_be/nmc_hrtm/6h-3h/gen_be${BIN_TYPE}_v1.0.0_demo  # Output dir
export STRIDE=40               # Point skipped in the computation of horiz correlations
export UKMO_BE_IN=/home/mm0100/frfc/UKV_CovStats/CovStats    # Input UKMO CovStats file.
                                                             # This is only use to set the size 
                                                             # of some pointes. No data from this
                                                             # file is use.

#[4] Run gen_be:
${GEN_BE_DIR}/scripts/gen_be.ksh   # don't modify
