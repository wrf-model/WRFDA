#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_run_obsproc.ksh
#
# Purpose: Creates observation file for input to WRFVAR (ob_format_2).
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

export WORK_DIR=$RUN_DIR/working
mkdir -p $RUN_DIR $OB_DIR/$DATE

export OBSPROC_DIR=${OBSPROC_DIR:-$REL_DIR/3DVAR_OBSPROC} # Observation preprocessing
echo "<HTML><HEAD><TITLE>$EXPT obsproc</TITLE></HEAD><BODY><H1>$EXPT obsproc</H1><PRE>"

date

echo 'REL_DIR      <A HREF="'$REL_DIR'">'$REL_DIR'</a>'
echo 'OBSPROC_DIR  <A HREF="'$OBSPROC_DIR'">'$OBSPROC_DIR'</a>'
echo 'RTOBS_DIR    <A HREF="'$RTOBS_DIR'">'$RTOBS_DIR'</a>'
echo 'OB_DIR       <A HREF="'$OB_DIR'">'$OB_DIR'</a>'
echo 'RUN_DIR      <A HREF="'$RUN_DIR'">'$RUN_DIR'</a>'
echo 'WORK_DIR     <A HREF="'$WORK_DIR'">'$WORK_DIR'</a>'

mkdir -p $WORK_DIR
cd $WORK_DIR

   export NL_DX_KM=`expr $NL_DX \/ 1000`

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
   . ${SCRIPTS_DIR}/da_get_date_range.ksh
   export TIME_WINDOW_MIN=${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00
   export FCST_RANGE=$MAX_OB_RANGE
   . ${SCRIPTS_DIR}/da_get_date_range.ksh
   export TIME_WINDOW_MAX=${NL_END_YEAR}-${NL_END_MONTH}-${NL_END_DAY}_${NL_END_HOUR}:00:00
   export FCST_RANGE=0
   . ${SCRIPTS_DIR}/da_get_date_range.ksh
   export TIME_ANALYSIS=${NL_START_YEAR}-${NL_START_MONTH}-${NL_START_DAY}_${NL_START_HOUR}:00:00
   export FCST_RANGE=$FCST_RANGE_SAVE

   export OB_FILE=obs.${NL_START_YEAR}${NL_START_MONTH}${NL_START_DAY}${NL_START_HOUR}

   #ln -fs $OB_DIR/$DATE/$OB_FILE . 

   if test -f $RTOBS_DIR/$DATE/${OB_FILE}.gz; then
      # If compressed, unpack
      cp $RTOBS_DIR/$DATE/$OB_FILE.gz .
      gunzip -f ${OB_FILE}.gz
     else 
      #cmd   
      cp $RTOBS_DIR/$DATE/$OB_FILE .
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
 max_number_of_obs        = ${MAX_NUMBER_OF_OBS},
 fatal_if_exceed_max_obs  = .TRUE.,
/
 max_number_of_obs        = 70000,

&record4
 qc_test_vert_consistency = .TRUE.,
 qc_test_convective_adj   = .TRUE.,
 qc_test_above_lid        = .TRUE.,
 remove_above_lid         = .TRUE.,
 domain_check_h           = .true.,
 Thining_SATOB            = ${THINING_SATOB},
 Thining_SSMI             = ${THINING_SSMI},
 Thining_QSCAT            = ${THINING_QSCAT},
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
 ptop =  ${NL_P_TOP_REQUESTED},
 base_pres  =  100000.,
 base_temp  =  ${NL_BASE_TEMP},
 base_lapse  =  50.,
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

   echo "Converting $WORK_DIR/$OB_FILE to"
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
      echo "Ended %$RC"
   fi
   mv obs_gts.3dvar $OB_DIR/$DATE/ob.ascii

date

echo '</PRE></BODY></HTML>'

exit $RC

