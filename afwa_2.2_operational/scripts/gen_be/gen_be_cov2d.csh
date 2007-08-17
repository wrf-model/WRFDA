#! /bin/csh -f
#-----------------------------------------------------------------------

#set echo

if ( ! $?START_DATE )    setenv START_DATE    2003010200 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2003012812 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
if ( ! $?NE )            setenv NE            1          # Number of ensemble members (for ENS).
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      5          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?LAT_MIN )       setenv LAT_MIN       -90.0      # Used if BIN_TYPE = 2.
if ( ! $?LAT_MAX )       setenv LAT_MAX       90.0       # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
if ( ! $?HGT_MIN )       setenv HGT_MIN       0.0        # Used if BIN_TYPE = 2.
if ( ! $?HGT_MAX )       setenv HGT_MAX       20000.0    # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
if ( ! $?VARIABLE1 )     setenv VARIABLE1     ps_u       # Experiment ID
if ( ! $?VARIABLE2 )     setenv VARIABLE2     ps         # Experiment ID (normalizing field)

if ( ! $?EXPT )          setenv EXPT noobs
if ( ! $?ID )            setenv ID gen_be
if ( ! $?RELEASE )       setenv RELEASE WRF_V2.1.2
if ( ! $?REL_DIR )       setenv REL_DIR ${HOME}/code_development/${RELEASE}
if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${REL_DIR}/wrfvar
if ( ! $?BUILD_DIR )     setenv BUILD_DIR ${WRFVAR_DIR}/build
if ( ! $?DATA_DISK )     setenv DATA_DISK /ocotillo1
if ( ! $?REGION )        setenv REGION con200
if ( ! $?DAT_DIR )       setenv DAT_DIR ${DATA_DISK}/${user}/data/${REGION}/${EXPT}
if ( ! $?RUN_DIR )       setenv RUN_DIR ${DAT_DIR}/$ID

cd ${RUN_DIR}

echo "---------------------------------------------------------------"
echo "Run gen_be_cov2d."
echo "---------------------------------------------------------------"

ln -sf ${BUILD_DIR}/gen_be_cov2d.exe .

cat >! gen_be_cov2d_nl.nl << EOF
  &gen_be_cov2d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT}, 
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}' /
EOF

./gen_be_cov2d.exe

exit(0)
