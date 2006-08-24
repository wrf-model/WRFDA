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

export DATE=${DATE:-2004051300}
export CYCLING=${CYCLING:-false}
export DOMAIN=${DOMAIN:-01}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export DA_DIR=${DA_DIR:-$REG_DIR/da}
export CS_DIR=${CS_DIR:-$REG_DIR/cs}
export EXPT=${EXPT:-test}
export RUN_DIR=${RUN_DIR:-$REG_DIR/$EXPT}
export WORK_DIR=${WORK_DIR:-$RUN_DIR/working}

export DUMMY=${DUMMY:-false}
export CLEAN=${CLEAN:-true}

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT update_bc</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT update_bc</H1><PRE>"
fi

date

if test ! -d ${RUN_DIR}; then
   mkdir ${RUN_DIR}
fi

export REL_DIR=${REL_DIR:-$HOME/trunk} # Code directory.
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$CS_DIR/$DATE/wrfvar_input_d$DOMAIN}
export DA_ANALYSIS=${DA_ANALYSIS:-$DA_DIR/$DATE/wrfvar_output}
export BDYIN=${BDYIN:-$CS_DIR/$DATE/wrfbdy_d$DOMAIN}
export BDYOUT=${BDYOUT:-$CS_DIR/$DATE/wrfbdy_d$DOMAIN}

rm -rf ${WORK_DIR}
mkdir  ${WORK_DIR}
cd ${WORK_DIR}

cp $BDYIN wrfbdy_d$DOMAIN
ln -sf $DA_ANALYSIS wrfvar_output
ln -sf $DA_FIRST_GUESS wrfvar_input

cat > parame.in << EOF
&control_param
 wrf_3dvar_output_file = 'wrfvar_output'
 wrf_bdy_file          = 'wrfbdy_d${DOMAIN}'
 wrf_input_from_si     = 'wrfvar_input'

 cycling = .${CYCLING}.
 debug   = .true.
 low_bdy_only = .false. /
EOF

if test $DUMMY; then
   echo "Dummy update_bc"
   echo Dummy update_bc > wrfbdy_d$DOMAIN
else
   $WRFVAR_DIR/main/da_update_bc.exe
fi

mv wrfbdy_d${DOMAIN} $BDYOUT

if $CLEAN; then
   rm -rf ${WORK_DIR}
fi

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi

exit 0
