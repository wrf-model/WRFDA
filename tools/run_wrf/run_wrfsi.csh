#! /bin/csh -f
#-----------------------------------------------------------------------
# Script: run_wrfsi.csh
#
# Purpose: Run WRFSI.
#
# Author: Dale Barker, MMM Division, NCAR.
#
# History:
# 06/06/2003:  First Version.                                      Dale Barker
#-----------------------------------------------------------------------

#TODO: set VERTICAL_LEVELS via environment variables (setenv doesn't work). 
#set echo

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

if ( ! $?RUN_GRID_GEN )     setenv RUN_GRID_GEN .FALSE.
if ( ! $?USE_OTHER_STATIC_DATA ) setenv USE_OTHER_STATIC_DATA .FALSE. 
if ( ! $?START_DATE )       setenv START_DATE 2003010100
if ( ! $?FCST_RANGE )       setenv FCST_RANGE 06 
if ( ! $?LBC_FREQ )         setenv LBC_FREQ   06 
if ( ! $?DOMAIN )           setenv DOMAIN 1
if ( ! $?REGION )           setenv REGION con200
setenv RUN_ID ${REGION}_${START_DATE}

#Directories:
if ( ! $?BIN_DIR )          setenv BIN_DIR ${HOME}/bin
if ( ! $?DATA_DISK )        setenv DATA_DISK /data3
if ( ! $?SRC_DIR )          setenv SRC_DIR ${HOME}/code_development/WRF_V2.1 # Code directory.
if ( ! $?WRFSI_DIR )        setenv WRFSI_DIR ${SRC_DIR}/wrfsi
if ( ! $?SOURCE_ROOT )      setenv SOURCE_ROOT ${WRFSI_DIR}
if ( ! $?INSTALLROOT )      setenv INSTALLROOT ${SOURCE_ROOT}             # Where SI binaries created.

if ( ! $?GEOG_DATAROOT )    setenv GEOG_DATAROOT /data2/powers/wrfsi/extdata/GEOG # Where case directories reside.
if ( ! $?EXT_DATAROOT )     setenv EXT_DATAROOT ${INSTALLROOT}/extdata    # GRIB files processed in grib_prep here.
if ( ! $?DATAROOT )         setenv DATAROOT ${INSTALLROOT}/domains
if ( ! $?MOAD_DATAROOT )    setenv MOAD_DATAROOT ${DATAROOT}/$REGION

if ( ! $?DAT_DIR )          setenv DAT_DIR ${DATA_DISK}/${user}/data
if ( ! $?AVN_DIR )          setenv AVN_DIR ${DAT_DIR}/fnl
if ( ! $?OTHER_STATIC_DATA ) setenv OTHER_STATIC_DATA ${DAT_DIR}/amps60.static/static/static.wrfsi.d0${DOMAIN}

#WRFSI Namelist variables:
if ( ! $?XDIM )             setenv XDIM 45                   # 
if ( ! $?YDIM )             setenv YDIM 45                   # 
if ( ! $?MAP_PROJ_NAME )    setenv MAP_PROJ_NAME 'lambert'
if ( ! $?CENTRAL_LAT )      setenv CENTRAL_LAT 40.0          # 
if ( ! $?CENTRAL_LON )      setenv CENTRAL_LON -98.0         # 
if ( ! $?STAND_LATS1 )      setenv STAND_LATS1 30.0          # 
if ( ! $?STAND_LATS2 )      setenv STAND_LATS2 60.0          # 
if ( ! $?STAND_LONS )       setenv STAND_LONS -98.0          # 
if ( ! $?DELTA_X )          setenv DELTA_X 200000.0          # Resolution (m). 
if ( ! $?DELTA_Y )          setenv DELTA_Y 200000.0          # Resolution (m).
if ( ! $?SILAVWT_PARM_WRF ) setenv SILAVWT_PARM_WRF 0.0      # ?
if ( ! $?PTOP_PA )          setenv PTOP_PA 5000.0            # Model top pressure (Pa).
if ( ! $?HINTERP_METHOD )   setenv HINTERP_METHOD 1          #
if ( ! $?VERTICAL_LEVELS )  set  VERTICAL_LEVELS = ( " 1.000, 0.990, 0.978, 0.964, 0.946, "\
                                                        " 0.922, 0.894, 0.860, 0.817, 0.766, "\
                                                        " 0.707, 0.644, 0.576, 0.507, 0.444, 0.380,"\
                                                        " 0.324, 0.273, 0.228, 0.188, 0.152,"\
                                                        " 0.121, 0.093, 0.069, 0.048, 0.029, 0.014, 0.000" )

setenv MOAD_KNOWN_LAT $CENTRAL_LAT
setenv MOAD_KNOWN_LON $CENTRAL_LON
setenv MOAD_STAND_LATS1 $STAND_LATS1
setenv MOAD_STAND_LATS2 $STAND_LATS2
setenv MOAD_STAND_LONS $STAND_LONS
setenv MOAD_DELTA_X $DELTA_X
setenv MOAD_DELTA_Y $DELTA_Y

source ${BIN_DIR}/get_date_range.csh $START_DATE $FCST_RANGE

setenv LBC_FREQ_SS `expr $LBC_FREQ \* 3600`

if ( $RUN_GRID_GEN == .TRUE. ) then

#-----------------------------------------------------------------
echo "   Configure WRF domain (run grid_gen once for each domain)"
#-----------------------------------------------------------------

cd $INSTALLROOT/templates
cp -r default $REGION
chmod -R u+w $REGION

# Create wrfsi.nl namelist:

cat >! ${INSTALLROOT}/templates/${REGION}/wrfsi.nl << EOF
&project_id
 SIMULATION_NAME = '${RUN_ID}'
 USER_DESC = '${user}'
/
&filetimespec
 START_YEAR = $START_YEAR 
 START_MONTH = $START_MONTH
 START_DAY = $START_DAY
 START_HOUR = $START_HOUR
 START_MINUTE = 00                              
 START_SECOND = 00
 END_YEAR = $END_YEAR
 END_MONTH = $END_MONTH
 END_DAY = $END_DAY
 END_HOUR = $END_HOUR
 END_MINUTE = 00
 END_SECOND = 00
 INTERVAL = $LBC_FREQ_SS   
/

&hgridspec
 NUM_DOMAINS = 1
 XDIM = ${XDIM}
 YDIM = ${YDIM}
 PARENT_ID =         1,   1,   2,   3,   2,
 RATIO_TO_PARENT =   1,   3,   3,   3,   3,
 DOMAIN_ORIGIN_LLI = 1,   18,  90,  28,  150,
 DOMAIN_ORIGIN_LLJ = 1,   38,  125, 31,  35,
 DOMAIN_ORIGIN_URI = ${XDIM}, 91,  116, 56,  207,
 DOMAIN_ORIGIN_URJ = ${YDIM}, 107, 156, 69,  85,
 MAP_PROJ_NAME = '${MAP_PROJ_NAME}',
 MOAD_KNOWN_LAT = ${MOAD_KNOWN_LAT},
 MOAD_KNOWN_LON = ${MOAD_KNOWN_LON},
 MOAD_STAND_LATS = ${MOAD_STAND_LATS1}, ${MOAD_STAND_LATS2},
 MOAD_STAND_LONS = ${MOAD_STAND_LONS},
 MOAD_DELTA_X = ${MOAD_DELTA_X}
 MOAD_DELTA_Y = ${MOAD_DELTA_Y}
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
 ANALPATH = '${EXT_DATAROOT}/extprd',
 LBCPATH = '${EXT_DATAROOT}/extprd',
 LSMPATH = '${EXT_DATAROOT}/extprd',
 CONSTANTS_PATH = '${EXT_DATAROOT}/extprd',
/
EOF

cd ${INSTALLROOT}/etc

./window_domain_rt.pl -c -w wrfsi -t ${INSTALLROOT}/templates/${REGION} \
                      >&! window_domain_rt.pl.out

else
   echo "   grid_gen bypassed."
endif
echo ""

#-----------------------------------------------------------------------
# [3] Configure grib_prep:
#-----------------------------------------------------------------------

cat >! ${EXT_DATAROOT}/static/grib_prep.nl << EOF
&filetimespec
 START_YEAR = ${START_YEAR}
 START_MONTH = ${START_MONTH}
 START_DAY = ${START_DAY}
 START_HOUR = ${START_HOUR}
 START_MINUTE = 00
 START_SECOND = 00
 END_YEAR = ${END_YEAR}
 END_MONTH = ${END_MONTH}
 END_DAY = ${END_DAY}
 END_HOUR = ${END_HOUR}
 END_MINUTE = 00
 END_SECOND = 00
 INTERVAL = 10800
/
&gpinput_defs
 SRCNAME = 'ETA', 'GFS', 'AVN', 'RUCH', 'NNRP', 'NNRPSFC', 'SST'
 SRCVTAB = 'ETA', 'GFS', 'AVN', 'RUCH', 'NNRPSFC', 'NNRPSFC', 'SST'
 SRCPATH = '/public/data/grids/eta/40km_eta212_isobaric/grib',
                '/public/data/grids/avn/global-65160/grib',
                '${AVN_DIR}/tmp',
                '/rt0/rucdev/nrelwind/run/maps_fcst',
                '/path/to/nnrp/grib',
                '/path/to/nnrp/sfc/grib',
                '/data1/bresch/May04/sst'
 SRCCYCLE = 6, 6, 6, 6, 12, 12, 24
 SRCDELAY = 3, 4, 4, 3, 0, 0, 36
/
EOF

# Prepare input AVN data:
rm -rf ${AVN_DIR}/tmp >&! /dev/null
mkdir $AVN_DIR/tmp

set DATE = $START_DATE
while ( $DATE <= $END_DATE )
   set YY = `echo $DATE | cut -c3-4`
   set MM = `echo $DATE | cut -c5-6`
   set DD = `echo $DATE | cut -c7-8`
   set HH = `echo $DATE | cut -c9-10`

   set AVN_FILE = fnl_${YY}${MM}${DD}_${HH}_00

#DALE06   ln -sf ${AVN_DIR}/${AVN_FILE} ${AVN_DIR}/tmp/${AVN_FILE}
   ln -sf ${AVN_DIR}/${DATE}/${AVN_FILE} ${AVN_DIR}/tmp/${AVN_FILE}
   echo "   File ${AVN_DIR}/tmp/$AVN_FILE created as link."

   set DATE=`${BIN_DIR}/advance_cymdh.exe ${DATE} ${LBC_FREQ}`
end
echo ""

#Copy in another static file:
if ( $USE_OTHER_STATIC_DATA == .TRUE. ) then
   echo "   Use another static file:"
   ls -l ${OTHER_STATIC_DATA}
   ln -sf ${OTHER_STATIC_DATA} ${MOAD_DATAROOT}/static/static.wrfsi.d01
endif

# Run grib_prep:
cd ${INSTALLROOT}/etc

./grib_prep.pl -s ${START_DATE} -l ${FCST_RANGE} AVN >&! grib_prep.out

#-----------------------------------------------------------------------
# [3.0] Run WRFSI:
#-----------------------------------------------------------------------

cd ${INSTALLROOT}/etc

echo "   Running wrfsi.pl ${START_DATE} ${FCST_RANGE} AVN"
./wrfprep.pl -s ${START_DATE} -f ${FCST_RANGE} >& ./run_wrfsi.out

#rm -rf ${MOAD_DATAROOT}/siprd/hinterp* >&! /dev/null
#rm -rf ${MOAD_DATAROOT}/log/* >&! /dev/null
#rm ${EXT_DATAROOT}/extprd/*-* >&! /dev/null # Remove everything other than README file!
echo ""

exit (0)

