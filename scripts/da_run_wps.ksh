#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wps.ksh
#
# Purpose: Run WPS

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wps}
export WORK_DIR=$RUN_DIR/working

export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export WPS_OUTPUT_DIR=${WPS_OUTPUT_DIR:-$RC_DIR}

if [[ ! -d $REG_DIR ]]; then mkdir $REG_DIR; fi 
if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi 
if [[ ! -d $WPS_OUTPUT_DIR/$DATE ]]; then mkdir -p $WPS_OUTPUT_DIR/$DATE; fi 
if [[ ! -d $WORK_DIR ]]; then mkdir $WORK_DIR; fi 

cd $WORK_DIR

echo "<HTML><HEAD><TITLE>$EXPT wps</TITLE></HEAD><BODY><H1>$EXPT wps</H1><PRE>"

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh

date

echo 'WPS_DIR        <A HREF="'$WPS_DIR'"</a>'$WPS_DIR'</a>'
echo "DATE           $DATE"
echo "END_DATE       $END_DATE"
echo 'WPS_INPUT_DIR  <A HREF="file:'$WPS_INPUT_DIR'"</a>'$WPS_INPUT_DIR'</a>'
echo 'WPS_OUTPUT_DIR <A HREF="file:'$WPS_OUTPUT_DIR'"</a>'$WPS_OUTPUT_DIR'</a>'
echo 'RUN_DIR        <A HREF="file:'$RUN_DIR'"</a>'$RUN_DIR'</a>'
echo 'WORK_DIR       <A HREF="file:'$WORK_DIR'"</a>'$WORK_DIR'</a>'

cat >namelist.wps <<EOF
&share
 wrf_core = 'ARW',
 max_dom = 1,
 start_year = $NL_START_YEAR,
 start_month = $NL_START_MONTH,
 start_day = $NL_START_DAY,
 start_hour = $NL_START_HOUR,
 end_year = $NL_END_YEAR,
 end_month = $NL_END_MONTH,
 end_day = $NL_END_DAY,
 end_hour = $NL_END_HOUR,
 interval_seconds = $LBC_FREQ_SS,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$WPS_OUTPUT_DIR',
 debug_level = 0
/

&geogrid
 parent_id =           1,
 parent_grid_ratio =   1,
 i_parent_start =      1,
 j_parent_start =      1,
 s_we           = 1,
 e_we           = $NL_E_WE,
 s_sn           = 1,
 e_sn           = $NL_E_SN,
 geog_data_res  = '$GEOG_DATA_RES',
 dx = $NL_DX,
 dy = $NL_DY,
 map_proj = '$MAP_PROJ',
 ref_lat   = $REF_LAT,
 ref_lon   = $REF_LON,
 truelat1  = $TRUELAT1,
 truelat2  = $TRUELAT2,
 stand_lon = $STAND_LON,
 geog_data_path = '$WPS_GEOG_DIR',
 opt_geogrid_tbl_path = '$WPS_DIR/geogrid'
/

&ungrib
 out_format = 'SI'
/

&metgrid
 fg_name = './FILE'
 io_form_metgrid = 2, 
 opt_output_from_metgrid_path = '$WPS_OUTPUT_DIR/$DATE',
 opt_metgrid_tbl_path         = '$WPS_DIR/metgrid',
 opt_ignore_dom_center        = .false.
/
EOF

cp $WORK_DIR/namelist.wps $RUN_DIR

echo '<A HREF="namelist.wps">namelist.wps</a>'

#-----------------------------------------------------------------------
# [3.0] Run WPS:
#-----------------------------------------------------------------------

if $DUMMY; then
   echo "Dummy wps"
   LOCAL_DATE=$DATE
   while [[ $LOCAL_DATE -le $END_DATE ]]; do
      export L_YEAR=$(echo $LOCAL_DATE | cut -c1-4)
      export L_MONTH=$(echo $LOCAL_DATE | cut -c5-6)
      export L_DAY=$(echo $LOCAL_DATE | cut -c7-8)
      export L_HOUR=$(echo $LOCAL_DATE | cut -c9-10)
      for DOMAIN in $DOMAINS; do
         echo Dummy wps > met_em.d${DOMAIN}.${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00.nc
      done
      LOCAL_DATE=$($WRFVAR_DIR/build/da_advance_time.exe ${LOCAL_DATE} 1 2>/dev/null)
   done
else
   if $RUN_GEOGRID; then
      # Run geogrid:
      ln -fs $WPS_DIR/geogrid.exe .
      ${RUN_CMD} ./geogrid.exe

      RC=$?
      mv geogrid.log* $RUN_DIR
      if [[ -f $RUN_DIR/geogrid.log.0000 ]]; then
         echo '<A HREF="geogrid.log.0000">geogrid.log.0000</a>'
      else
         echo '<A HREF="geogrid.log">geogrid.log</a>'
      fi

      if [[ $RC != 0 ]]; then
         echo geogrid failed with error $RC
         exit $RC
      fi
   fi

   # Run ungrib:
   ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
   LOCAL_DATE=$DATE
   FILES=''
   while [[ $LOCAL_DATE -le $END_DATE ]]; do
      FILES="$FILES $WPS_INPUT_DIR/$LOCAL_DATE/*"
      LOCAL_DATE=$($WRFVAR_DIR/build/da_advance_time.exe ${LOCAL_DATE} ${LBC_FREQ} 3>/dev/null)
   done
   $WPS_DIR/link_grib.csh $FILES

   ln -fs $WPS_DIR/ungrib.exe .

   ./ungrib.exe > ungrib.log 2>&1

   RC=$?
   mv ungrib.log $RUN_DIR
   echo '<A HREF="ungrib.log">ungrib.log</a>'

   if [[ $RC != 0 ]]; then
      echo ungrib failed with error $RC
      exit $RC
   fi

   # Run metgrid:
   ln -fs $WPS_DIR/metgrid.exe .
   ${RUN_CMD} ./metgrid.exe

   RC=$?

   mv metgrid.log* $RUN_DIR
   if [[ -f $RUN_DIR/metgrid.log.0000 ]]; then
      echo '<A HREF="metgrid.log.0000">metgrid.log.0000</a>'
   else
      echo '<A HREF="metgrid.log">metgrid.log</a>'
   fi

   if [[ $RC != 0 ]]; then
      echo metgrid failed with error $RC
      exit $RC
   fi

fi

cd $OLDPWD

if $CLEAN; then rm -rf $WORK_DIR; fi

date

echo "</BODY></HTML>"

exit 0
