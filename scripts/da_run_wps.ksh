#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wps.ksh
#
# Purpose: Run WPS

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010100}
export CYCLE_PERIOD=${CYCLE_PERIOD:-06}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}
export DUMMY=${DUMMY:-false}

# Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}

export WPS_GEOG_DIR=${WPS_GEOG_DIR:-$DAT_DIR/wps_geog}

export NL_E_WE=${NL_E_WE:-110}
export NL_E_SN=${NL_E_SN:-145}
export MAP_PROJ=${MAP_PROJ:-polar}
export REF_LAT=${REF_LAT:--87.396970}
export REF_LON=${REF_LON:-180.0}
export TRUELAT1=${TRUELAT1:--90.0}
export TRUELAT2=${TRUELAT2:--90.0}
export STAND_LON=${STAND_LON:-180.0}
export NL_DX=${NL_DX:-90000}
export NL_DY=${NL_DY:-90000}
export LBC_FREQ=${LBC_FREQ:-06}
export GEOG_DATA_RES=${GEOG_DATA_RES:-10m}
export FG_TYPE=${FG_TYPE:-GFS}

export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${RUN_DIR:-$REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wps}

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT wps</TITLE></HEAD><BODY><H1>$EXPT wps</H1><PRE>"
fi

date

if test ! -d $RUN_DIR; then
   mkdir $RUN_DIR
fi

cd $RUN_DIR

let LBC_FREQ_SS=$LBC_FREQ*3600

# Calculate end dates

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh $DATE $CYCLE_PERIOD

cat >namelist.wps <<EOF
&share
 wrf_core = 'ARW',
 max_dom = 1,
 start_year = $START_YEAR,
 start_month = $START_MONTH,
 start_day = $START_DAY,
 start_hour = $START_HOUR,
 end_year = $END_YEAR,
 end_month = $END_MONTH,
 end_day = $END_DAY,
 end_hour = $END_HOUR,
 interval_seconds = $LBC_FREQ_SS,
 io_form_geogrid = 2,
 opt_output_from_geogrid_path = '$RUN_DIR',
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
 opt_output_from_metgrid_path = '$RUN_DIR',
 opt_metgrid_tbl_path         = '$WPS_DIR/metgrid',
 opt_ignore_dom_center        = .false.
/
EOF

cp namelist.wps $OUT_DIR

#-----------------------------------------------------------------------
# [3.0] Run WPS:
#-----------------------------------------------------------------------

export CCYY=`echo $DATE | cut -c1-4`
export MM=`echo $DATE | cut -c5-6`
export DD=`echo $DATE | cut -c7-8`
export HH=`echo $DATE | cut -c9-10`

if test ! -f $CS_DIR/$DATE/met_em.d${DOMAIN}.${CCYY}-${MM}-${DD}_${HH}:00:00.nc; then
   if $DUMMY; then
      echo "Dummy wps"
      LOCAL_DATE=$DATE
      while test $LOCAL_DATE -le $END_DATE; do
         export CCYY=`echo $LOCAL_DATE | cut -c1-4`
         export MM=`echo $LOCAL_DATE | cut -c5-6`
         export DD=`echo $LOCAL_DATE | cut -c7-8`
         export HH=`echo $LOCAL_DATE | cut -c9-10`
         echo Dummy wps > met_em.d${DOMAIN}.${CCYY}-${MM}-${DD}_${HH}:00:00
         LOCAL_DATE=`$WRFVAR_DIR/main/advance_cymdh.exe ${LOCAL_DATE} 1 2>/dev/null`
      done
   else
      ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
      $WPS_DIR/link_grib.csh $CS_DIR/$START_DATE/fn* $CS_DIR/$END_DATE/fn*
      $WPS_DIR/geogrid.exe
      RC=$?
      if test $RC != 0; then
         echo geogrid failed with error $RC
         exit 1
      fi
      $WPS_DIR/ungrib.exe
      RC=$?
      if test $RC != 0; then
         echo ungrib failed with error $RC
         exit 1
      fi
      $WPS_DIR/metgrid.exe
      RC=$?
      if test $RC != 0; then
         echo metgrid failed with error $RC
         exit 1
      fi
   fi
   mv met_em.d${DOMAIN}* $CS_DIR/$DATE
else
   echo "$CS_DIR/$DATE/met_em.d${DOMAIN}* files exist, skipping"
fi

cd $OLDPWD

if $CLEAN; then
   rm -rf $RUN_DIR
fi

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi

exit 0

