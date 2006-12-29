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
export FC_DIR=${FC_DIR:-$REG_DIR/fc}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export EXPT=${EXPT:-test}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DATE/update_bc}
export WORK_DIR=$RUN_DIR/working

export DUMMY=${DUMMY:-false}
export CLEAN=${CLEAN:-true}

echo "<HTML><HEAD><TITLE>$EXPT update_bc</TITLE></HEAD><BODY>"
echo "<H1>$EXPT update_bc</H1><PRE>"

date

mkdir -p ${RUN_DIR}

export REL_DIR=${REL_DIR:-$HOME/trunk} # Code directory.
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DA_FIRST_GUESS=${DA_FIRST_GUESS:-$FC_DIR/$DATE/wrfvar_input_d$DOMAIN}
export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}
export BDYIN=${BDYIN:-$RC_DIR/$DATE/wrfbdy_d$DOMAIN}
export BDYOUT=${BDYOUT:-$FC_DIR/$DATE/wrfbdy_d$DOMAIN}

rm -rf ${WORK_DIR}
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}

echo 'REL_DIR        <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR     <A HREF="'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>'
echo "DATE           $DATE"
echo "DA_ANALYSIS    $DA_ANALYSIS"
echo "DA_FIRST_GUESS $DA_FIRST_GUESS"
echo "BDYIN          $BDYIN"
echo "BDYOUT         $BDYOUT"
echo 'WORK_DIR       <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

cp $BDYIN wrfbdy_d$DOMAIN
ln -sf $DA_ANALYSIS wrfvar_output
ln -sf $DA_FIRST_GUESS wrf_input

cat > parame.in << EOF
&control_param
 wrfvar_output_file = 'wrfvar_output'
 wrf_bdy_file       = 'wrfbdy_d${DOMAIN}'
 wrf_input          = 'wrf_input'

 cycling = .${CYCLING}.
 debug   = .true.
 low_bdy_only = .false. /
EOF

if $DUMMY; then
   echo "Dummy update_bc"
   echo Dummy update_bc > wrfbdy_d$DOMAIN
else
   $WRFVAR_DIR/build/da_update_bc.exe
   RC=$?
   if test $RC != 0; then
      echo "Update_bc failed with error $RC"
      exit 1
   fi
fi

mv wrfbdy_d${DOMAIN} $BDYOUT

if $CLEAN; then
   rm -rf ${WORK_DIR}
fi

date

echo "</BODY></HTML>"

exit 0
