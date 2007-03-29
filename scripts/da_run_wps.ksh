#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wps.ksh
#
# Purpose: Run WPS

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-6}
export LBC_FREQ=${LBC_FREQ:-06}
export DUMMY=${DUMMY:-false}
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}
export EXPT=${EXPT:-test}
export HOSTS=${HOSTS:-$HOME/hosts}
if test -f $HOSTS; then
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS -machinefile $HOSTS}
else
   export RUN_CMD=${RUN_CMD:-mpirun -np $NUM_PROCS}
fi
export CLEAN=${CLEAN:-false}
export RUN_GEOGRID=${RUN_GEOGRID:-true}

#Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export GRIB_DIR=${GRIB_DIR:-$DAT_DIR/fnl}
export WPS_DIR=${WPS_DIR:-$REL_DIR/wps}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-$REG_DIR/$EXPT}
export RC_DIR=${RC_DIR:-$REG_DIR/rc}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/run/$DATE/wps}
export WORK_DIR=$RUN_DIR/working

#WPS:
export WPS_INPUT_DIR=${WPS_INPUT_DIR:-$GRIB_DIR}
export OPT_GEOGRID_TBL_PATH=${OPT_GEOGRID_TBL_PATH:-$WPS_DIR/geogrid}
export OPT_METGRID_TBL_PATH=${OPT_METGRID_TBL_PATH:-$WPS_DIR/metgrid}
export WPS_GEOG_DIR=${WPS_GEOG_DIR:-~wrfhelp/WPS_GEOG}
export NL_E_WE=${NL_E_WE:-45}
export NL_E_SN=${NL_E_SN:-45}
export MAP_PROJ=${MAP_PROJ:-lambert}
export REF_LAT=${REF_LAT:-40.0}
export REF_LON=${REF_LON:--98.0}
export TRUELAT1=${TRUELAT1:-30.0}
export TRUELAT2=${TRUELAT2:-60.0}
export STAND_LON=${STAND_LON:--98.0}
export NL_DX=${NL_DX:-200000}
export NL_DY=${NL_DY:-200000}
export GEOG_DATA_RES=${GEOG_DATA_RES:-30s}
export FG_TYPE=${FG_TYPE:-GFS}

let LBC_FREQ_SS=$LBC_FREQ*3600

if test ! -d $REG_DIR; then mkdir $REG_DIR; fi 
if test ! -d $RUN_DIR; then mkdir -p $RUN_DIR; fi 
if test ! -d $RC_DIR/$DATE; then mkdir -p $RC_DIR/$DATE; fi 
if test ! -d $WORK_DIR; then mkdir $WORK_DIR; fi 

cd $WORK_DIR

echo "<HTML><HEAD><TITLE>$EXPT wps</TITLE></HEAD><BODY><H1>$EXPT wps</H1><PRE>"

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh

date

echo 'WPS_DIR       <A HREF="'$WPS_DIR'"</a>'$WPS_DIR'</a>'
echo "DATE          $DATE"
echo "END_DATE      $END_DATE"
echo 'WPS_INPUT_DIR <A HREF="file:'$WPS_INPUT_DIR'"</a>'$WPS_INPUT_DIR'</a>'
echo 'RUN_DIR       <A HREF="file:'$RUN_DIR'"</a>'$RUN_DIR'</a>'
echo 'WORK_DIR      <A HREF="file:'$WORK_DIR'"</a>'$WORK_DIR'</a>'
echo 'RC_DIR        <A HREF="file:'$RC_DIR'"</a>'$RC_DIR'</a>'

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
 opt_output_from_geogrid_path = '$RC_DIR',
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
 opt_output_from_metgrid_path = '$WORK_DIR',
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
      while test $LOCAL_DATE -le $END_DATE; do
         export L_YEAR=`echo $LOCAL_DATE | cut -c1-4`
         export L_MONTH=`echo $LOCAL_DATE | cut -c5-6`
         export L_DAY=`echo $LOCAL_DATE | cut -c7-8`
         export L_HOUR=`echo $LOCAL_DATE | cut -c9-10`
         echo Dummy wps > met_em.d${DOMAIN}.${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00.nc
         LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe ${LOCAL_DATE} 1 2>/dev/null`
      done
   else
      if $RUN_GEOGRID; then
         # Run geogrid:
	 ln -fs $WPS_DIR/geogrid.exe .
	 ${RUN_CMD} ./geogrid.exe

	 RC=$?
	 mv geogrid.log* $RUN_DIR
	 if test -f $RUN_DIR/geogrid.log.0000; then
            echo '<A HREF="geogrid.log.0000">geogrid.log.0000</a>'
	 else
            echo '<A HREF="geogrid.log">geogrid.log</a>'
	 fi

	 if test $RC != 0; then
            echo geogrid failed with error $RC
            exit $RC
	 fi
	 cp $WORK_DIR/geo_em.d01.nc $RC_DIR
      fi

      # Run ungrib:
      ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
      LOCAL_DATE=$DATE
      FILES=''
      while test $LOCAL_DATE -le $END_DATE; do
         FILES="$FILES $WPS_INPUT_DIR/$LOCAL_DATE/*"
         LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 3>/dev/null`
      done
      $WPS_DIR/link_grib.csh $FILES

      ln -fs $WPS_DIR/ungrib.exe .

      ${RUN_CMD} ./ungrib.exe > ungrib.log 2>&1

      RC=$?
      mv ungrib.log $RUN_DIR
      echo '<A HREF="ungrib.log">ungrib.log</a>'

      if test $RC != 0; then
         echo ungrib failed with error $RC
         exit $RC
      fi

      # Run metgrid:
      ln -fs $WPS_DIR/metgrid.exe .
      ${RUN_CMD} ./metgrid.exe

      RC=$?
      
      mv metgrid.log* $RUN_DIR
      if test -f $RUN_DIR/metgrid.log.0000; then
         echo '<A HREF="metgrid.log.0000">metgrid.log.0000</a>'
      else
         echo '<A HREF="metgrid.log">metgrid.log</a>'
      fi
      
      if test $RC != 0; then
         echo metgrid failed with error $RC
         exit $RC
      fi
      
   fi
   mv met_em.d${DOMAIN}* $RC_DIR/$DATE

cd $OLDPWD

if $CLEAN; then rm -rf $WORK_DIR; fi

date

echo "</BODY></HTML>"

exit 0
