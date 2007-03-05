#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_run_obsproc.ksh
#
# Purpose: Creates observation file for input to WRFVAR (ob_format_2).
#-----------------------------------------------------------------------

#--------------------------------------------
# [1] Set up various environment variables:
#--------------------------------------------

export EXPT=${EXPT:-test}
export DATE=${DATE:-2004051300}
export MAX_OB_RANGE=${MAX_OB_RANGE:-2}             # Maximum difference O, B (hours)

export DOMAIN=${DOMAIN:-01}
export REGION=${REGION:-con200}
export REL_DIR=${REL_DIR:-$HOME/trunk}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/$DATE/obsproc}
export WORK_DIR=$RUN_DIR/working
export OB_DIR=${OB_DIR:-$REG_DIR/ob}
export DUMMY=${DUMMY:-false}

mkdir -p $RUN_DIR $OB_DIR/$DATE

export OBSPROC_DIR=${OBSPROC_DIR:-$REL_DIR/3DVAR_OBSPROC} # Observation preprocessing

# Namelist variables used in obs. preprocessor:

export NL_E_WE=${NL_E_WE:-110}
export NL_E_SN=${NL_E_SN:-145}
export MAP_PROJ=${MAP_PROJ:-polar}
export REF_LAT=${REF_LAT:--87.396970}
export REF_LON=${REF_LON:-180.0}
export TRUELAT1=${TRUELAT1:--90.0}
export TRUELAT2=${TRUELAT2:--90.0}
export STAND_LON=${STAND_LON:-180.0}
export NL_DX=${NL_DX:-90000}
export PTOP_PA=${PTOP_PA:-5000.0}

echo "<HTML><HEAD><TITLE>$EXPT obsproc</TITLE></HEAD><BODY><H1>$EXPT obsproc</H1><PRE>"

date

echo 'REL_DIR      <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'OBSPROC_DIR  <A HREF="'$OBSPROC_DIR'">'$OBSPROC_DIR'</a>'
echo 'OB_DIR       <A HREF="'$OB_DIR'">'$OB_DIR'</a>'
echo 'RUN_DIR      <A HREF="'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR     <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

mkdir -p $WORK_DIR
cd $WORK_DIR

   export NL_DX_KM=`expr $NL_DX \/ 1000`

   # MM5 variables (not in WRF):
   export PS0=${PS0:-100000.0}
   export TS0=${TS0:-273.0}
   export TLP=${TLP:-50.0}

   if test $MAP_PROJ = lambert; then
      export PROJ=1
   elif test $MAP_PROJ = polar;  then
      export PROJ=2
   elif test $MAP_PROJ = mercator; then
      export PROJ=3
   else
      echo "   Unknown MAP_PROJ = $MAP_PROJ."
      exit 1
   fi

   export FCST_RANGE_SAVE=$FCST_RANGE
   export FCST_RANGE=-$MAX_OB_RANGE
   . $WRFVAR_DIR/scripts/da_get_date_range.ksh
   export TIME_WINDOW_MIN=${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00
   export FCST_RANGE=$MAX_OB_RANGE
   . $WRFVAR_DIR/scripts/da_get_date_range.ksh
   export TIME_WINDOW_MAX=${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00
   export FCST_RANGE=0
   . $WRFVAR_DIR/scripts/da_get_date_range.ksh
   export TIME_ANALYSIS=${NL_START_YEAR}-${NL_START_MONTH}-${NL_START_DAY}_${NL_START_HOUR}:00:00
   export FCST_RANGE=$FCST_RANGE_SAVE

   export OB_FILE=obs.${NL_START_YEAR}${NL_START_MONTH}${NL_START_DAY}${NL_START_HOUR}

   ln -fs $OB_DIR/$DATE/$OB_FILE .

   if test -f $OB_DIR/$DATE/${OB_FILE}.gz; then
      # If compressed, unpack
      gunzip -f $OB_DIR/$DATE/${OB_FILE}.gz
   fi

   #Namelist notes:
   #1. x,y reversed in namelist as MM5 i=y.
   #2. Modified namelist 2 in fortran code to be lime -DBKG.

   cat > namelist.3dvar_obs << EOF
&record1
 obs_gts_filename = '$OB_FILE',
 fg_format        = 'WRF',
 obs_err_filename = 'obserr.txt',
/

&record2
 time_window_min  = '${TIME_WINDOW_MIN}',
 time_analysis    = '${TIME_ANALYSIS}',
 time_window_max  = '${TIME_WINDOW_MAX}',
/

&record3
 max_number_of_obs        = 70000,
 fatal_if_exceed_max_obs  = .TRUE.,
/

&record4
 qc_test_vert_consistency = .TRUE.,
 qc_test_convective_adj   = .TRUE.,
 qc_test_above_lid        = .TRUE.,
 remove_above_lid         = .TRUE.,
 domain_check_h           = .true.,
 Thining_SATOB            = .FALSE.,
 Thining_SSMI             = .FALSE.,
 Thining_QSCAT            = .FALSE.,
/

&record5
 print_gts_read           = .TRUE.,
 print_gpspw_read         = .TRUE.,
 print_recoverp           = .TRUE.,
 print_duplicate_loc      = .TRUE.,
 print_duplicate_time     = .TRUE.,
 print_recoverh           = .TRUE.,
 print_qc_vert            = .TRUE.,
 print_qc_conv            = .TRUE.,
 print_qc_lid             = .TRUE.,
 print_uncomplete         = .TRUE.,
/

&record6
 ptop =  ${PTOP_PA},
 ps0  =  ${PS0},
 ts0  =  ${TS0},
 tlp  =  ${TLP},
/

&record7
 IPROJ = ${PROJ},
 PHIC  = ${REF_LAT},
 XLONC = ${REF_LON},
 TRUELAT1= ${TRUELAT1},
 TRUELAT2= ${TRUELAT2},
 MOAD_CEN_LAT = ${REF_LAT},
 STANDARD_LON = ${STAND_LON},
/

&record8
 IDD    =   1,
 MAXNES =   2,
 NESTIX =  ${NL_E_SN},  200,  136,  181,  211,
 NESTJX =  ${NL_E_WE},  200,  181,  196,  211,
 DIS    =  ${NL_DX_KM},  10.,  3.3,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   40,  28,    35,   45,
 NESTJ  =    1,   60,  25,    65,   55,
 / 

&record9
 PREPBUFR_OUTPUT_FILENAME = 'prepbufr_output_filename',
 PREPBUFR_TABLE_FILENAME = 'prepbufr_table_filename',
 OUTPUT_OB_FORMAT = 2
 /

EOF

   cp namelist.3dvar_obs $RUN_DIR

   echo "Converting $OB_DIR/$DATE/$OB_FILE to"
   echo "$OB_DIR/$DATE/ob.ascii"
   echo '<A HREF="namelist.3dvar_obs">Namelist input</a>'
   if $DUMMY; then
      echo "Dummy obsproc"
      echo "Dummy obsproc" > obs_gts.3dvar
   else
      ln -fs $OBSPROC_DIR/obserr.txt .
      ln -fs $OBSPROC_DIR/prepbufr_table_filename .
      $OBSPROC_DIR/3dvar_obs.exe
      RC=$?
      if test $RC = 0; then
         echo "${OK}Suceeded${END}"
      else
         echo "${ERR}Failed${END} with error $RC"
      fi
   fi
   mv obs_gts.3dvar $OB_DIR/$DATE/ob.ascii

date

echo '</PRE></BODY></HTML>'

exit $RC

