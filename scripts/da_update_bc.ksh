#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_update_bc.ksh
#
# Purpose: Update WRF lateral boundary conditions to be consistent with 
# WRFVAR analysis.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

#Experiment details:
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}                            # Domain name.
export EXPT=${EXPT:-test}                              # Experiment name.
export CLEAN=${CLEAN:-true}
export CYCLING=${CYCLING:-false}

#Time info:
export DATE=${DATE:-2003010100}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRF_BC_DIR=${WRF_BC_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data} # Data directory.
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} # Data directory for region.
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT} #Run directory.
export RC_DIR=${RC_DIR:-$REG_DIR/rc}     # Reconfiguration directory
export FC_DIR=${FC_DIR:-$EXP_DIR/fc}     # Forecast directory

export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/$DATE/update_bc}
export WORK_DIR=$RUN_DIR/working

echo "<HTML><HEAD><TITLE>$EXPT update_bc</TITLE></HEAD><BODY>"
echo "<H1>$EXPT update_bc</H1><PRE>"

date

mkdir -p ${RUN_DIR}

export DA_REAL_OUTPUT=$RC_DIR/$DATE/wrfinput_d$DOMAIN      # Input (needed only if cycling).
export BDYIN=${BDYIN:-$RC_DIR/$DATE/wrfbdy_d$DOMAIN}       # Input bdy.
export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
export BDYOUT=${BDYOUT:-$FC_DIR/$DATE/wrfbdy_d$DOMAIN}     # Output bdy.

rm -rf ${WORK_DIR}
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}

echo 'REL_DIR        <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRF_BC_DIR     <A HREF="'$WRF_BC_DIR'">'$WRF_BC_DIR'</a>'
echo "DATE           $DATE"
echo "DA_REAL_OUTPUT $DA_REAL_OUTPUT"
echo "BDYIN          $BDYIN"
echo "DA_ANALYSIS    $DA_ANALYSIS"
echo "BDYOUT         $BDYOUT"
echo 'WORK_DIR       <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

cp $DA_REAL_OUTPUT real_output
cp $BDYIN wrfbdy_d$DOMAIN
cp $DA_ANALYSIS wrfvar_output

cat > parame.in << EOF
&control_param
 wrf_3dvar_output_file = 'wrfvar_output'
 wrf_bdy_file          = 'wrfbdy_d${DOMAIN}'
 wrf_input_from_si     = 'real_output'

 cycling = .${CYCLING}.
 debug   = .true.
 low_bdy_only = .false. /
EOF

if $DUMMY; then
   echo "Dummy update_bc"
   echo Dummy update_bc > wrfbdy_d$DOMAIN
else
   cp $WRF_BC_DIR/update_wrf_bc.exe .
   ./update_wrf_bc.exe

   RC=$?
   if test $RC != 0; then
      echo "Update_bc failed with error $RC"
      exit 1
   fi
fi

cp wrfbdy_d${DOMAIN} $BDYOUT

if $CLEAN; then
   rm -rf ${WORK_DIR}
fi

date

echo "</BODY></HTML>"

exit 0
