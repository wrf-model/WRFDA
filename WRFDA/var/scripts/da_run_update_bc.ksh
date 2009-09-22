#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_run_update_bc.ksh
#
# Purpose: Update WRF lateral boundary conditions to be consistent with 
# WRFVAR analysis.
#
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/update_bc}
export WORK_DIR=$RUN_DIR/working

echo "<HTML><HEAD><TITLE>$EXPT update_bc</TITLE></HEAD><BODY>"
echo "<H1>$EXPT update_bc</H1><PRE>"

date

mkdir -p ${RUN_DIR}

export DA_REAL_OUTPUT=${DA_REAL_OUTPUT:-$RC_DIR/$DATE/wrfinput_d01} # Input (needed only if cycling).
export BDYIN=${BDYIN:-$RC_DIR/$DATE/wrfbdy_d01}       # Input bdy.
if $NL_VAR4D ; then
   if $NL_VAR4D_LBC; then
      export BDYOUT_TL=$FC_DIR/$DATE/wrfbdy_tl01 
   fi
   if $CYCLING; then
      if [[ $CYCLE_NUMBER -gt 0 ]]; then
         if $PHASE; then
            export YEAR=$(echo $DATE | cut -c1-4)
            export MONTH=$(echo $DATE | cut -c5-6)
            export DAY=$(echo $DATE | cut -c7-8)
            export HOUR=$(echo $DATE | cut -c9-10)
            export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$CYCLE_PERIOD 2>/dev/null)
            export ANALYSIS_DATE=${YEAR}-${MONTH}-${DAY}_${HOUR}:00:00
            export DA_ANALYSIS=${FC_DIR}/${PREV_DATE}/wrfinput_d01_${ANALYSIS_DATE}
         else
            export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
         fi
      else
         export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
      fi
   fi
else
   export DA_ANALYSIS=${DA_ANALYSIS:-$FC_DIR/$DATE/analysis}  # Input analysis.
fi
export BDYOUT=${BDYOUT:-$FC_DIR/$DATE/wrfbdy_d01}     # Output bdy.

rm -rf ${WORK_DIR}
mkdir -p ${WORK_DIR}
cd ${WORK_DIR}

echo 'REL_DIR        <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'WRFVAR_DIR     <A HREF="'$WRFVAR_DIR'">'$WRFVAR_DIR'</a>'
echo "DATE           $DATE"
echo "DA_REAL_OUTPUT $DA_REAL_OUTPUT"
echo "BDYIN          $BDYIN"
echo "DA_ANALYSIS    $DA_ANALYSIS"
echo "BDYOUT         $BDYOUT"
if $NL_VAR4D_LBC; then
   echo "BDYOUT_TL      $BDYOUT_TL"
fi
echo 'WORK_DIR       <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

cp -f $DA_REAL_OUTPUT real_output 
cp -f $BDYIN wrfbdy_d01
ln -sf $DA_ANALYSIS wrfvar_output

cat > parame.in << EOF
&control_param
 wrfvar_output_file = 'wrfvar_output'
 wrf_bdy_file       = 'wrfbdy_d01'
 wrf_input          = 'real_output'

 cycling = .${CYCLING}.
 debug   = .true.
 low_bdy_only = .${NL_LOW_BDY_ONLY}. 
 update_lsm = .${NL_UPDATE_LSM}. /
EOF

if $DUMMY; then
   echo "Dummy update_bc"
   echo Dummy update_bc > wrfbdy_d01
else

   if $NL_VAR4D_LBC; then
      echo "ncks -v G_U_BXS,G_U_BTXS,G_U_BXE,G_U_BTXE...",$BDYOUT_TL,"  wrfbdy_tmp"
      ncks -v G_U_BXS,G_U_BTXS,G_U_BXE,G_U_BTXE,G_U_BYS,G_U_BTYS,G_U_BYE,G_U_BTYE,G_V_BXS,G_V_BTXS,G_V_BXE,G_V_BTXE,G_V_BYS,G_V_BTYS,G_V_BYE,G_V_BTYE,G_PH_BXS,G_PH_BTXS,G_PH_BXE,G_PH_BTXE,G_PH_BYS,G_PH_BTYS,G_PH_BYE,G_PH_BTYE,G_T_BXS,G_T_BTXS,G_T_BXE,G_T_BTXE,G_T_BYS,G_T_BTYS,G_T_BYE,G_T_BTYE,G_MU_BXS,G_MU_BTXS,G_MU_BXE,G_MU_BTXE,G_MU_BYS,G_MU_BTYS,G_MU_BYE,G_MU_BTYE,G_QVAPOR_BXS,G_QVAPOR_BTXS,G_QVAPOR_BXE,G_QVAPOR_BTXE,G_QVAPOR_BYS,G_QVAPOR_BTYS,G_QVAPOR_BYE,G_QVAPOR_BTYE $BDYOUT_TL wrfbdy_tmp
      echo "ncrename -v G_U_BTXS,U_BTXE...  wrfbdy_tmp"
      ncrename -v G_U_BXS,U_BXS -v G_U_BTXS,U_BTXS -v G_U_BXE,U_BXE -v G_U_BTXE,U_BTXE -v G_U_BYS,U_BYS -v G_U_BTYS,U_BTYS -v G_U_BYE,U_BYE -v G_U_BTYE,U_BTYE -v G_V_BXS,V_BXS -v G_V_BTXS,V_BTXS -v G_V_BXE,V_BXE -v G_V_BTXE,V_BTXE -v G_V_BYS,V_BYS -v G_V_BTYS,V_BTYS -v G_V_BYE,V_BYE -v G_V_BTYE,V_BTYE -v G_PH_BXS,PH_BXS -v G_PH_BTXS,PH_BTXS -v G_PH_BXE,PH_BXE -v G_PH_BTXE,PH_BTXE -v G_PH_BYS,PH_BYS -v G_PH_BTYS,PH_BTYS -v G_PH_BYE,PH_BYE -v G_PH_BTYE,PH_BTYE -v G_T_BXS,T_BXS -v G_T_BTXS,T_BTXS -v G_T_BXE,T_BXE -v G_T_BTXE,T_BTXE -v G_T_BYS,T_BYS -v G_T_BTYS,T_BTYS -v G_T_BYE,T_BYE -v G_T_BTYE,T_BTYE -v G_MU_BXS,MU_BXS -v G_MU_BTXS,MU_BTXS -v G_MU_BXE,MU_BXE -v G_MU_BTXE,MU_BTXE -v G_MU_BYS,MU_BYS -v G_MU_BTYS,MU_BTYS -v G_MU_BYE,MU_BYE -v G_MU_BTYE,MU_BTYE -v G_QVAPOR_BXS,QVAPOR_BXS -v G_QVAPOR_BTXS,QVAPOR_BTXS -v G_QVAPOR_BXE,QVAPOR_BXE -v G_QVAPOR_BTXE,QVAPOR_BTXE -v G_QVAPOR_BYS,QVAPOR_BYS -v G_QVAPOR_BTYS,QVAPOR_BTYS -v G_QVAPOR_BYE,QVAPOR_BYE -v G_QVAPOR_BTYE,QVAPOR_BTYE  wrfbdy_tmp
      echo "ncflint -w 1, 1  wrfbdy_d01 wrfbdy_tmp wrfbdy_final"
      cp wrfbdy_d01 wrfbdy_final
      ncflint -A -v U_BXS,U_BTXS,U_BXE,U_BTXE,U_BYS,U_BTYS,U_BYE,U_BTYE,V_BXS,V_BTXS,V_BXE,V_BTXE,V_BYS,V_BTYS,V_BYE,V_BTYE,PH_BXS,PH_BTXS,PH_BXE,PH_BTXE,PH_BYS,PH_BTYS,PH_BYE,PH_BTYE,T_BXS,T_BTXS,T_BXE,T_BTXE,T_BYS,T_BTYS,T_BYE,T_BTYE,MU_BXS,MU_BTXS,MU_BXE,MU_BTXE,MU_BYS,MU_BTYS,MU_BYE,MU_BTYE,QVAPOR_BXS,QVAPOR_BTXS,QVAPOR_BXE,QVAPOR_BTXE,QVAPOR_BYS,QVAPOR_BTYS,QVAPOR_BYE,QVAPOR_BTYE -w 1,1 wrfbdy_tmp wrfbdy_d01 wrfbdy_final
      mv wrfbdy_final wrfbdy_d01
   else
      ln -fs $BUILD_DIR/da_update_bc.exe .
      ./da_update_bc.exe
   fi

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Update_bc failed with error $RC"
      exit 1
   else
      cp wrfbdy_d01 $BDYOUT
   fi
fi

if $CLEAN; then
   rm -rf ${WORK_DIR}
fi

date

echo "</BODY></HTML>"

exit 0
