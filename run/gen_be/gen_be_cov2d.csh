#! /bin/csh -f
#-----------------------------------------------------------------------

set echo

if ( ! $?START_DATE )    setenv START_DATE    2004030312 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2004033112 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      24         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      1          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?LAT_MIN )       setenv LAT_MIN       -90.0      # Used if BIN_TYPE = 2.
if ( ! $?LAT_MAX )       setenv LAT_MAX       90.0       # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
if ( ! $?HGT_MIN )       setenv HGT_MIN       0.0        # Used if BIN_TYPE = 2.
if ( ! $?HGT_MAX )       setenv HGT_MAX       20000.0    # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
if ( ! $?VARIABLE1 )     setenv VARIABLE1     ps_u       # Experiment ID
if ( ! $?VARIABLE2 )     setenv VARIABLE2     ps         # Experiment ID (normalizing field)
if ( ! $?EXPT )          setenv EXPT 2004-03.T63         # Experiment ID

if ( ! $?SRC_DIR )       setenv SRC_DIR /tara/dmbarker/wrfvar/gen_be
if ( ! $?DAT_DIR )       setenv DAT_DIR /tara/dmbarker/kma_stats/$EXPT
if ( ! $?RUN_DIR )       setenv RUN_DIR /tara2/dmbarker/kma_stats

if ( ! -d ${RUN_DIR} ) mkdir ${RUN_DIR}
if ( ! -d ${RUN_DIR}/$EXPT ) mkdir ${RUN_DIR}/$EXPT

cd ${RUN_DIR}/$EXPT

echo "---------------------------------------------------------------"
echo "Run gen_be_cov2d."
echo "---------------------------------------------------------------"

ln -sf ${SRC_DIR}/gen_be_cov2d.exe .

cat >! gen_be_cov2d_nl.nl << EOF
  &gen_be_cov2d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT}, 
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}',
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

./gen_be_cov2d.exe
#   rm -rf gen_be_stage1.exe

exit(0)
