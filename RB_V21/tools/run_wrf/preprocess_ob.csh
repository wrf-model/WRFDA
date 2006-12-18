#! /bin/csh -f
#-----------------------------------------------------------------------
# Script preprocess_ob.csh
#
# Purpose: Creates observation file for input to 3DVAR (ob_format_2).
#
#-----------------------------------------------------------------------

#--------------------------------------------
# [1] Set up various environment variables:
#--------------------------------------------
#set echo

if ( ! $?EXPT )             setenv EXPT wrftest
if ( ! $?START_DATE )       setenv START_DATE 2004051300
if ( ! $?MAX_OB_RANGE )     setenv MAX_OB_RANGE 2             # Maximum difference O, B (hours)

if ( ! $?DOMAIN )           setenv DOMAIN 1
if ( ! $?REGION )           setenv REGION amps${DOMAIN}
if ( ! $?BIN_DIR )          setenv BIN_DIR ${HOME}/bin
if ( ! $?DATA_DISK )        setenv DATA_DISK /data3
if ( ! $?DAT_DIR )          setenv DAT_DIR ${DATA_DISK}/${user}/data
if ( ! $?REG_DIR )          setenv REG_DIR ${DAT_DIR}/${REGION}
if ( ! $?RUN_DIR )          setenv RUN_DIR ${REG_DIR}/$EXPT/${START_DATE}
if ( ! -d ${RUN_DIR} )      mkdir ${RUN_DIR}

if ( ! $?OBSPROC_DIR )      setenv OBSPROC_DIR ${DAT_DIR}/3DVAR_OBSPROC # Observation preprocessing
if ( ! $?DA_OBSERVATIONS )  setenv DA_OBSERVATIONS $RUN_DIR/obs_gts.3dvar.${START_DATE}

#Namelist (wrfsi.nl) variables used in obs. preprocessor:
if ( ! $?XDIM )             setenv XDIM 110
if ( ! $?YDIM )             setenv YDIM 145
if ( ! $?MAP_PROJ_NAME )    setenv MAP_PROJ_NAME 'polar'
if ( ! $?CENTRAL_LAT )      setenv CENTRAL_LAT -87.396970
if ( ! $?CENTRAL_LON )      setenv CENTRAL_LON 180.0
if ( ! $?STAND_LATS1 )      setenv STAND_LATS1 -90.0
if ( ! $?STAND_LATS2 )      setenv STAND_LATS2 -90.0
if ( ! $?STAND_LONS )       setenv STAND_LONS 180.0
if ( ! $?DELTA_X )          set DELTA_X = 90000
if ( ! $?PTOP_PA )          setenv PTOP_PA 5000.0

set DELTA_X_KM = `expr $DELTA_X \/ 1000`

#MM5 variables (not in WRF):
if ( ! $?PS0 )              setenv PS0 100000.0
if ( ! $?TS0 )              setenv TS0 273.0
if ( ! $?TLP )              setenv TLP 50.0

if ( $MAP_PROJ_NAME == 'lambert' ) then
   setenv PROJ 1
else if ( $MAP_PROJ_NAME == 'polar' ) then
   setenv PROJ 2
else if ( $MAP_PROJ_NAME == 'mercator' ) then
   setenv PROJ 3
else
   echo "   Unknown MAP_PROJ_NAME = $MAP_PROJ_NAME."
   exit(1)
endif

source ${BIN_DIR}/get_date_range.csh $START_DATE -$MAX_OB_RANGE
setenv TIME_WINDOW_MIN ${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00
source ${BIN_DIR}/get_date_range.csh $START_DATE $MAX_OB_RANGE
setenv TIME_WINDOW_MAX ${END_YEAR}-${END_MONTH}-${END_DAY}_${END_HOUR}:00:00
source ${BIN_DIR}/get_date_range.csh $START_DATE 00
setenv TIME_ANALYSIS ${START_YEAR}-${START_MONTH}-${START_DAY}_${START_HOUR}:00:00

#--------------------------------------------
# [2] Retrieve "little_r format" ob files:
#--------------------------------------------

setenv OB_FILE obs.${START_YEAR}${START_MONTH}${START_DAY}${START_HOUR}

setenv RTOBS_DIR ${DAT_DIR}/${START_YEAR}${START_MONTH}/mm5rt
#echo "DALE Use Aqua"
#setenv RTOBS_DIR ${DAT_DIR}/${START_YEAR}${START_MONTH}/airs
#echo "DALE Use Alain's obs"
#setenv RTOBS_DIR ${DAT_DIR}/${START_YEAR}${START_MONTH}/raobs_only
#setenv OB_FILE little-r.dat_jan${START_DAY}${START_YEAR}

if ( -s ${RTOBS_DIR}/${OB_FILE}.gz ) then
   cp ${RTOBS_DIR}/${OB_FILE}.gz  ${OBSPROC_DIR}/.
else
   echo "   Error - file ${RTOBS_DIR}/${OB_FILE}.gz not found."
   exit
endif
echo ""

#-------------------------------------------
# [3] Run MM5 3DVAR obs preprocessor:
#--------------------------------------------

cd ${OBSPROC_DIR}

gunzip -f ${OB_FILE}.gz >&! /dev/null

#Namelist notes:
#1. x,y reversed in namelist as MM5 i=y.
#2. Modified namelist 2 in fortran code to be lime -DBKG.

cat >! namelist.3dvar_obs << EOF
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
 PHIC  = ${CENTRAL_LAT},
 XLONC = ${CENTRAL_LON},
 TRUELAT1= ${STAND_LATS1},
 TRUELAT2= ${STAND_LATS2},
 MOAD_CEN_LAT = ${CENTRAL_LAT},
 STANDARD_LON = ${STAND_LONS},
/

&record8
 IDD    =   1,
 MAXNES =   2,
 NESTIX =  ${YDIM},  200,  136,  181,  211,
 NESTJX =  ${XDIM},  200,  181,  196,  211,
 DIS    =  ${DELTA_X_KM},  10.,  3.3,  1.1,  1.1,
 NUMC   =    1,    1,   2,     3,    4,
 NESTI  =    1,   40,  28,    35,   45,
 NESTJ  =    1,   60,  25,    65,   55,
 /

EOF

./3dvar_obs.exe

mv obs_gts.3dvar ${DA_OBSERVATIONS} 
rm ${OB_FILE}

exit (0)

