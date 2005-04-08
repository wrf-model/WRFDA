#! /bin/csh -f
#-----------------------------------------------------------------------

setenv VARIABLE1 ps_u
setenv VARIABLE2 ps

if ( ! $?START_DATE )    setenv START_DATE    2004030312 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2004033112 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      24         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      1          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?NUM_BINS_HGT )  setenv NUM_BINS_HGT  30         # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
if ( ! $?VARIABLE1 )     setenv VARIABLE1     ps_u       # Experiment ID
if ( ! $?VARIABLE2 )     setenv VARIABLE2     ps         # Experiment ID (normalizing field)
if ( ! $?EXPT )          setenv EXPT 2004-03.T63         # Experiment ID

if ( ! $?BIN_DIR )       setenv BIN_DIR ${HOME}/bin
if ( ! $?SRC_DIR )       setenv SRC_DIR ${HOME}/gen_be/scripts
if ( ! $?DAT_DIR )       setenv DAT_DIR /tara/dmbarker/kma_stats/$EXPT
if ( ! $?RUN_DIR )       setenv RUN_DIR /tara2/dmbarker/gen_be

if ( ! -d ${RUN_DIR} ) mkdir ${RUN_DIR}
if ( ! -d ${RUN_DIR}/$EXPT ) mkdir ${RUN_DIR}/$EXPT

cd ${RUN_DIR}/$EXPT

echo "---------------------------------------------------------------"
echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
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
    num_bins_hgt = ${NUM_BINS_HGT},
    binwidth_hgt = ${BINWIDTH_HGT},
    binwidth_lat = ${BINWIDTH_LAT},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}',
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

./gen_be_cov2d.exe
#   rm -rf gen_be_stage1.exe

exit(0)
