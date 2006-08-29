#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wrfsi.ksh
#
# Purpose: Run WRFSI.

#-----------------------------------------------------------------------

#TODO: set VERTICAL_LEVELS via environment variables (export doesn't work). 
#set echo

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export RUN_GRID_GEN=${RUN_GRID_GEN:-true}
export USE_OTHER_STATIC_DATA=${USE_OTHER_STATIC_DATA:-false} 
export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-06}
export LBC_FREQ=${LBC_FREQ:-06} 
export REGION=${REGION:-con200}
export DOMAIN=${DOMAIN:-01}

export RUN_ID=${REGION}_${DATE}
export EXPT=${EXPT:-test}
export DAT_DIR=${DAT_DIR:-$HOME/data}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION}
export EXP_DIR=${EXP_DIR:-REG_DIR/$EXPT}
export RUN_DIR=${RUN_DIR:-$EXP_DIR/wrfsi}

# Directories:
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFSI_DIR=${WRFSI_DIR:-$REL_DIR/wrfsi}
export SOURCE_ROOT=${SOURCE_ROOT:-$WRFSI_DIR}
export INSTALLROOT=${INSTALLROOT:-$SOURCE_ROOT}             # Where SI binaries created.
export GEOG_DATAROOT=${GEOG_DATAROOT:-$WRFSI_DIR/GEOG} # Where case directories reside.
export EXT_DATAROOT=${EXT_DATAROOT:-$RUN_DIR/extdata}    # GRIB files processed in grib_prep here.
export EXTPRD=${EXTPRD:-$EXT_DATAROOT/extprd}
export STATIC=${STATIC:-$EXT_DATAROOT/static}
export DATAROOT=${DATAROOT:-$INSTALLROOT/domains}
export MOAD_DATAROOT=${MOAD_DATAROOT:-$DATAROOT/$REGION}
export TEMPLATE_DIR=${TEMPLATE_DIR:-$RUN_DIR/templates}
export AVN_DIR=${AVN_DIR:-$RUN_DIR/fnl}

#export OTHER_STATIC_DATA=${OTHER_STATIC_DATA:-$DAT_DIR/amps60.static/static/static.wrfsi.d$DOMAIN}

# WRFSI Namelist variables:

export NL_USE_HTML=${NL_USE_HTML:-false}
export NL_E_WE=${NL_E_WE:-110}
export NL_E_SN=${NL_E_SN:-145}
export MAP_PROJ=${MAP_PROJ:-polar}
export CENTRAL_LAT=${CENTRAL_LAT:--87.396970}
export CENTRAL_LON=${CENTRAL_LON:-180.0}
export STAND_LATS1=${STAND_LATS1:--90.0}
export STAND_LATS2=${STAND_LATS2:--90.0}
export STAND_LONS=${STAND_LONS:-180.0}
export NL_DX=${NL_DX:-90000}
export NL_DY=${NL_DY:-90000}
export PTOP_PA=${PTOP_PA:-5000.0}
export HINTERP_METHOD=${HINTERP_METHOD:-1}
export SILAVWT_PARM_WRF=${SILAVWT_PARM_WRF:-2.0}

export VERTICAL_LEVELS=" 1.000, 0.990, 0.978, 0.964, 0.946, "\
" 0.922, 0.894, 0.860, 0.817, 0.766, "\
" 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
" 0.324, 0.273, 0.228, 0.188, 0.152,"\
" 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000"

#echo "Hardwired default Indian domain vertical levels."
#export VERTICAL_LEVELS = ( " 1.000, 0.993, 0.980, 0.966, 0.950, "\
#        " 0.933, 0.913, 0.892, 0.869, 0.844, "\
#        " 0.816, 0.786, 0.753, 0.718, 0.680, "\
#        " 0.639, 0.596, 0.550, 0.501, 0.451, "\
#        " 0.398, 0.345, 0.290, 0.236, 0.188, "\
#        " 0.145, 0.108, 0.075, 0.046, 0.021, 0.000" )

#echo "Hardwired default T4b domain vertical levels."
#export VERTICAL_LEVELS = ( " 1.000, 0.995, 0.992, 0.983, 0.975, "\
#        " 0.961, 0.949, 0.932, 0.917, 0.897, "\
#        " 0.878, 0.855, 0.832, 0.806, 0.778, "\
#        " 0.749, 0.718, 0.687, 0.654, 0.623, "\
#        " 0.590, 0.559, 0.526, 0.495, 0.462, "\
#        " 0.431, 0.398, 0.367, 0.334, 0.304, "\
#        " 0.272, 0.244, 0.213, 0.187, 0.158, "\
#        " 0.134, 0.107, 0.085, 0.060, 0.040, "\
#        " 0.018 0.000 " )

export MOAD_KNOWN_LAT=$CENTRAL_LAT
export MOAD_KNOWN_LON=$CENTRAL_LON
export MOAD_STAND_LATS1=$STAND_LATS1
export MOAD_STAND_LATS2=$STAND_LATS2
export MOAD_STAND_LONS=$STAND_LONS
export MOAD_DX=$NL_DX
export MOAD_DY=$NL_DY

if $NL_USE_HTML; then
   echo "<HTML><HEAD><TITLE>$EXPT wrfsi</TITLE></HEAD><BODY>"
   echo "<H1>$EXPT wrfsi</H1><PRE>"
fi

date

set -x

mkdir -p $RUN_DIR $TEMPLATE_DIR $EXT_DATAROOT $EXTPRD $STATIC

cd $RUN_DIR

# Note - also need to modify levels if want to change defaults below!

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh $DATE $FCST_RANGE

export LBC_FREQ_SS=`expr $LBC_FREQ \* 3600`

if $RUN_GRID_GEN; then
   echo "Configure WRF domain (run grid_gen once for each domain)"

   cd $TEMPLATE_DIR
   cp -r $WRFSI_DIR/templates/default $REGION
   chmod -R u+w $REGION

   # Create wrfsi.nl namelist:

   cat > ${TEMPLATE_DIR}/${REGION}/wrfsi.nl << EOF
&project_id
 SIMULATION_NAME = '${RUN_ID}'
 USER_DESC = '${user}'
/
&filetimespec
 START_YEAR = $NL_START_YEAR 
 START_MONTH = $NL_START_MONTH
 START_DAY = $NL_START_DAY
 START_HOUR = $NL_START_HOUR
 START_MINUTE = $NL_START_MINUTE
 START_SECOND = $NL_START_SECOND
 END_YEAR = $NL_END_YEAR
 END_MONTH = $NL_END_MONTH
 END_DAY = $NL_END_DAY
 END_HOUR = $NL_END_HOUR
 END_MINUTE = $NL_END_MINUTE
 END_SECOND = $NL_END_SECOND
 INTERVAL = $LBC_FREQ_SS   
/

&hgridspec
 NUM_DOMAINS = 1
 XDIM = ${NL_E_WE}
 YDIM = ${NL_E_SN}
 PARENT_ID =         1,   1,   2,   3,   2,
 RATIO_TO_PARENT =   1,   3,   3,   3,   3,
 DOMAIN_ORIGIN_LLI = 1,   18,  90,  28,  150,
 DOMAIN_ORIGIN_LLJ = 1,   38,  125, 31,  35,
 DOMAIN_ORIGIN_URI = ${NL_E_WE}, 91,  116, 56,  207,
 DOMAIN_ORIGIN_URJ = ${NL_E_SN}, 107, 156, 69,  85,
 MAP_PROJ = '${MAP_PROJ}',
 MOAD_KNOWN_LAT = ${MOAD_KNOWN_LAT},
 MOAD_KNOWN_LON = ${MOAD_KNOWN_LON},
 MOAD_STAND_LATS = ${MOAD_STAND_LATS1}, ${MOAD_STAND_LATS2},
 MOAD_STAND_LONS = ${MOAD_STAND_LONS},
 MOAD_DX = ${MOAD_DX}
 MOAD_DY = ${MOAD_DY}
 SILAVWT_PARM_WRF = ${SILAVWT_PARM_WRF}
 TOPTWVL_PARM_WRF = 2.
/
&sfcfiles
 TOPO_30S =         '${GEOG_DATAROOT}/topo_30s',
 LANDUSE_30S =      '${GEOG_DATAROOT}/landuse_30s',
 SOILTYPE_TOP_30S = '${GEOG_DATAROOT}/soiltype_top_30s',
 SOILTYPE_BOT_30S = '${GEOG_DATAROOT}/soiltype_bot_30s',
 GREENFRAC =        '${GEOG_DATAROOT}/greenfrac',
 SOILTEMP_1DEG =    '${GEOG_DATAROOT}/soiltemp_1deg',
 ALBEDO_NCEP =      '${GEOG_DATAROOT}/albedo_ncep',
 MAXSNOWALB =       '${GEOG_DATAROOT}/maxsnowalb',
 ISLOPE =           '${GEOG_DATAROOT}/islope',
/
&interp_control
 NUM_ACTIVE_SUBNESTS = 0,
 ACTIVE_SUBNESTS = 2,3,4,
 PTOP_PA = ${PTOP_PA},
 HINTERP_METHOD = ${HINTERP_METHOD},
 LSM_HINTERP_METHOD = 1,
 NUM_INIT_TIMES = 1, 
 INIT_ROOT = 'AVN',
 LBC_ROOT = 'AVN',
 LSM_ROOT = '',
 CONSTANTS_FULL_NAME = '',
 VERBOSE_LOG = .FALSE.,
 OUTPUT_COORD = 'ETAP',
 LEVELS = ${VERTICAL_LEVELS}
/
&si_paths
 ANALPATH = '${EXTPRD}',
 LBCPATH = '${EXTPRD}',
 LSMPATH = '${EXTPRD}',
 CONSTANTS_PATH = '${EXTPRD}',
/
EOF

   cd ${INSTALLROOT}/etc

   if $DUMMY; then
      echo "Dummy window_domain_rt.pl"
   else
      ./window_domain_rt.pl -c -w wrfsi -t ${TEMPLATE_DIR}/${REGION} \
#                         > window_domain_rt.pl.out 2>&1
   fi

else
   echo "grid_gen bypassed."
fi

#-----------------------------------------------------------------------
# [3] Configure grib_prep:
#-----------------------------------------------------------------------

cat > ${STATIC}/grib_prep.nl << EOF
&filetimespec
 START_YEAR = ${NL_START_YEAR}
 START_MONTH = ${NL_START_MONTH}
 START_DAY = ${NL_START_DAY}
 START_HOUR = ${NL_START_HOUR}
 START_MINUTE = ${NL_START_MINUTE}
 START_SECOND = ${NL_END_SECOND}
 END_YEAR = ${NL_END_YEAR}
 END_MONTH = ${NL_END_MONTH}
 END_DAY = ${NL_END_DAY}
 END_HOUR = ${NL_END_HOUR}
 END_MINUTE = ${NL_END_MINUTE}
 END_SECOND = ${NL_END_SECOND}
 INTERVAL = 10800
/
&gpinput_defs
 SRCNAME = 'ETA', 'GFS', 'AVN', 'RUCH', 'NNRP', 'NNRPSFC', 'SST'
 SRCVTAB = 'ETA', 'GFS', 'AVN', 'RUCH', 'NNRPSFC', 'NNRPSFC', 'SST'
 SRCPATH = '/public/data/grids/eta/40km_eta212_isobaric/grib',
                '/public/data/grids/avn/global-65160/grib',
                '${CS_DIR}/$DATE',
                '/rt0/rucdev/nrelwind/run/maps_fcst',
                '/path/to/nnrp/grib',
                '/path/to/nnrp/sfc/grib',
                '/data1/bresch/May04/sst'
 SRCCYCLE = 6, 6, 6, 6, 12, 12, 24
 SRCDELAY = 3, 4, 4, 3, 0, 0, 36
/
EOF

LOCAL_DATE=$DATE
while test $LOCAL_DATE != $END_DATE; do
   YY=`echo $LOCAL_DATE | cut -c3-4`
   MM=`echo $LOCAL_DATE | cut -c5-6`
   DD=`echo $LOCAL_DATE | cut -c7-8`
   HH=`echo $LOCAL_DATE | cut -c9-10`

   AVN_FILE=fnl_${YY}${MM}${DD}_${HH}_00

#   ln -sf ${CS_DIR}/$LOCAL_DATE/${AVN_FILE} ${CS_DIR}/$LOCAL_DATE/${AVN_FILE}
#   echo "   File ${CSDIR}/$LOCAL_DATE/$AVN_FILE created as link."

   LOCAL_DATE=`${WRFVAR_DIR}/main/advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 2>/dev/null`
done

#Copy in another static file:
if ( $USE_OTHER_STATIC_DATA == .TRUE. ) then
   echo "Use another static file:"
   ls -l ${OTHER_STATIC_DATA}
   ln -sf ${OTHER_STATIC_DATA} ${MOAD_DATAROOT}/static/static.wrfsi.d$DOMAIN
fi

# Run grib_prep:
cd ${INSTALLROOT}/etc

if $DUMMY; then
   echo "Dummy grib_prep.pl"
else
   ./grib_prep.pl -s ${DATE} -l ${FCST_RANGE} AVN > grib_prep.out 2>&1
fi

#-----------------------------------------------------------------------
# [3.0] Run WRFSI:
#-----------------------------------------------------------------------

cd ${INSTALLROOT}/etc

if $DUMMY; then
   echo "Dummy wrfprep.pl"
else
   ./wrfprep.pl -s ${DATE} -f ${FCST_RANGE} > ./run_wrfsi.out 2>&1
fi

rm -rf ${MOAD_DATAROOT}/siprd/hinterp*
rm -rf ${MOAD_DATAROOT}/log/*
rm -rf ${EXT_DATAROOT}/extprd/*-* # Remove everything other than README file!

date

if $NL_USE_HTML; then
   echo "</BODY></HTML>"
fi

exit 0

