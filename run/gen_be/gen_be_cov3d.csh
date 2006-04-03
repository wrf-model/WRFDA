#! /bin/csh -f
#-----------------------------------------------------------------------

#set echo

setenv VARIABLE1 t_u
setenv VARIABLE2 t
setenv WRFVAR_DIR ${HOME}/code_development/WRF_V2.1.2/tmp/wrfvar.gen_be.test

if ( ! $?START_DATE )    setenv START_DATE    2003010200 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2003012812 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      5          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?VARIABLE1 )     setenv VARIABLE1     chi_u      # Experiment ID
if ( ! $?VARIABLE2 )     setenv VARIABLE2     chi        # Experiment ID (normalizing field)

if ( ! $?ID )            setenv ID ${BE_METHOD}.bin_type${BIN_TYPE}
if ( ! $?SRC_DIR )       setenv SRC_DIR ${HOME}/code_development/WRF_V2.1.2
if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${SRC_DIR}/wrfvar
if ( ! $?DATA_DISK )     setenv DATA_DISK /ocotillo1
if ( ! $?DOMAIN )        setenv DOMAIN con200
if ( ! $?DAT_DIR )       setenv DAT_DIR ${DATA_DISK}/${user}/data/${DOMAIN}/noobs/gen_be
if ( ! $?RUN_DIR )       setenv RUN_DIR ${DAT_DIR}/${ID}

cd ${RUN_DIR}

ln -sf ${WRFVAR_DIR}/gen_be/gen_be_cov3d.exe .

cat >! gen_be_cov3d_nl.nl << EOF
  &gen_be_cov3d_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    variable1 = '${VARIABLE1}',
    variable2 = '${VARIABLE2}',
    dat_dir = '${DAT_DIR}' /
EOF

./gen_be_cov3d.exe

exit(0)

