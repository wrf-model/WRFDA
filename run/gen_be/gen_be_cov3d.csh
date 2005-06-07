#! /bin/csh -f
#-----------------------------------------------------------------------
set echo

setenv VARIABLE1 t_u
setenv VARIABLE2 t

if ( ! $?START_DATE )    setenv START_DATE    2004030212 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2004033112 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      24         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?VARIABLE1 )     setenv VARIABLE1     chi_u      # Experiment ID
if ( ! $?VARIABLE2 )     setenv VARIABLE2     chi        # Experiment ID (normalizing field)
if ( ! $?EXPT )          setenv EXPT 2004-03.T63         # Experiment ID

if ( ! $?SRC_DIR )       setenv SRC_DIR /tara/dmbarker/wrfvar/gen_be
if ( ! $?DAT_DIR )       setenv DAT_DIR /tara2/dmbarker/kma_stats
if ( ! $?RUN_DIR )       setenv RUN_DIR $DAT_DIR 

if ( ! -d ${RUN_DIR} ) mkdir ${RUN_DIR}
if ( ! -d ${RUN_DIR}/$EXPT ) mkdir ${RUN_DIR}/$EXPT

cd ${RUN_DIR}/$EXPT

echo "---------------------------------------------------------------"
echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
echo "---------------------------------------------------------------"

ln -sf ${SRC_DIR}/gen_be_cov3d.exe .

cat >! gen_be_cov3d_nl.nl << EOF
  &gen_be_cov3d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}',
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

./gen_be_cov3d.exe

exit(0)
