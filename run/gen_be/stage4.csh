#! /bin/csh -f
#-----------------------------------------------------------------------
# Script run_filter.csh
#
# Purpose: 
#
#          0) Set up environment variables.
#
#-----------------------------------------------------------------------

#set echo

#setenv TESTING_SPECTRAL .TRUE.
#setenv TESTING_EOFS .TRUE.
setenv WRFVAR_DIR /zebra/users/rizvi/wrf3dvar_kma
setenv DAT_DIR /data2/dmbarker/kma_stats/2004-03.T63
setenv RUN_DIR /mmmtmp1/rizvi/kma_stats

#setenv TESTING_EOFS .TRUE.
setenv EXPT 2004-03.T63   

#setenv RUN_GEN_BE_STAGE1 # Set to run stage 1 (Remove mean, split variables).
#setenv RUN_GEN_BE_STAGE2 # Set to run stage 2 (Regression Coefficients).
#setenv RUN_GEN_BE_STAGE3 # Set to run stage 3 (Vertical Covariances).
setenv RUN_GEN_BE_STAGE4 # Set to run stage 4 (Horizontal Covariances).
setenv RUN_GEN_BE_DIAGS  # Set to run gen_be diagnostics.

if ( ! $?START_DATE )    setenv START_DATE    2004030312 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2004033112 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      24         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?GAUSSIAN_LATS ) setenv GAUSSIAN_LATS .false.    # Set if Gaussian latitudes used.
if ( ! $?REMOVE_MEAN )   setenv REMOVE_MEAN   .true.     # Remove time/ensemble/area mean.
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      1          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?NUM_BINS_HGT )  setenv NUM_BINS_HGT  30         # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
if ( ! $?TESTING_EOFS )  setenv TESTING_EOFS .false.     # True if performing EOF tests.
if ( ! $?USE_GLOBAL_EOFS ) setenv USE_GLOBAL_EOFS .true. # True if using global EOFs.
if ( ! $?DATA_ON_LEVELS )  setenv DATA_ON_LEVELS .false. # False if fields projected onto modes.
if ( ! $?NUM_LEVELS )    setenv NUM_LEVELS    30         # Hard-wired for now....
if ( ! $?TESTING_SPECTRAL ) setenv TESTING_SPECTRAL .false.  # True if performing spectral tests.
if ( ! $?EXPT )          setenv EXPT 2004-03.T63         # Experiment ID

if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${HOME}/wrf3dvar
if ( ! $?SRC_DIR )       setenv SRC_DIR ${WRFVAR_DIR}/main/gen_be
if ( ! $?DAT_DIR )       setenv DAT_DIR /taiwania3/dmbarker/gen_be_stats/$EXPT
if ( ! $?RUN_DIR )       setenv RUN_DIR /mmmtmp/${user}/gen_be_stats

if ( ! -d ${RUN_DIR} ) mkdir ${RUN_DIR}
if ( ! -d ${RUN_DIR}/$EXPT ) mkdir ${RUN_DIR}/$EXPT

#List of control variables:

foreach SV ( fullflds psi chi t rh ps )
   if ( ! -d ${RUN_DIR}/$EXPT/$SV ) mkdir ${RUN_DIR}/$EXPT/$SV
end

set CONTROL_VARIABLES = ( psi chi_u t_u rh ps_u )

foreach CV ( $CONTROL_VARIABLES )
   if ( ! -d ${RUN_DIR}/$EXPT/$CV ) mkdir ${RUN_DIR}/$EXPT/$CV
end

cd ${RUN_DIR}/$EXPT

#------------------------------------------------------------------------
#  Run Stage 1: Read "standard fields", and remove tim/ensemble/area mean.
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE1 ) then
   echo "---------------------------------------------------------------"
   echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
   echo "---------------------------------------------------------------"

   cp ${SRC_DIR}/gen_be_stage1.exe .

cat >! gen_be_stage1_nl.nl << EOF
  &gen_be_stage1_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    num_bins_hgt = ${NUM_BINS_HGT},
    binwidth_hgt = ${BINWIDTH_HGT},
    binwidth_lat = ${BINWIDTH_LAT},
    remove_mean = ${REMOVE_MEAN},
    gaussian_lats = ${GAUSSIAN_LATS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage1.exe  > gen_be_stage1.log
#   rm -rf gen_be_stage1.exe
endif

#------------------------------------------------------------------------
#  Run Stage 2: Calculate regression coefficients.
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE2 ) then

   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients."
   echo "---------------------------------------------------------------"

   cp ${SRC_DIR}/gen_be_stage2.exe .

cat >! gen_be_stage2_nl.nl << EOF
  &gen_be_stage2_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    num_bins_hgt = ${NUM_BINS_HGT},
    binwidth_hgt = ${BINWIDTH_HGT},
    binwidth_lat = ${BINWIDTH_LAT},
    testing_eofs = ${TESTING_EOFS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage2.exe  > gen_be_stage2.log
#   rm -rf gen_be_stage2.exe
endif

#------------------------------------------------------------------------
#  Run Stage 3: Read 3D fields for control variable, and calculate vertical
#  covariances,
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE3 ) then

   echo "---------------------------------------------------------------"
   echo "3) Perform EOF decomposition of fields (gen_be_stage3)."
   echo "---------------------------------------------------------------"

   cp ${SRC_DIR}/gen_be_stage3.exe .

   foreach CV ( $CONTROL_VARIABLES )
      setenv VARIABLE $CV

      if ( $CV == "ps" ) then
         echo "Bypassing vertical transform for " $CV
      else if ( $CV == "ps_u" ) then
         echo "Bypassing vertical transform for " $CV
      else

cat >! gen_be_stage3_nl.nl << EOF
  &gen_be_stage3_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    bin_type = ${BIN_TYPE},
    num_bins_hgt = ${NUM_BINS_HGT},
    binwidth_hgt = ${BINWIDTH_HGT},
    binwidth_lat = ${BINWIDTH_LAT},
    testing_eofs = ${TESTING_EOFS},
    use_global_eofs = ${USE_GLOBAL_EOFS},
    data_on_levels = ${DATA_ON_LEVELS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage3.exe  > gen_be_stage3.log
      endif
   end
endif

#------------------------------------------------------------------------
#  Run Stage 4: Read 2D (horizontal) fields for control variable, 
#  and calculate horizontal covariances,
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE4 ) then
   echo "---------------------------------------------------------------"
   echo "3) Perform spectral decomposition of fields."
   echo "---------------------------------------------------------------"

   cp ${SRC_DIR}/gen_be_stage4.exe .

   foreach CV ( $CONTROL_VARIABLES )
      setenv VARIABLE $CV

      if ( $CV == "ps"  ) then
         setenv MAX_VINDEX 1
         setenv DATA_ON_LEVELS .TRUE.
      else if ( $CV == "ps_u" ) then
         setenv MAX_VINDEX 1
         setenv DATA_ON_LEVELS .TRUE.
      else
         setenv MAX_VINDEX $NUM_LEVELS
         setenv DATA_ON_LEVELS .FALSE.
      endif

      set VINDEX = 1
      while ( $VINDEX <= $MAX_VINDEX )

cat >! gen_be_stage4_nl.nl << EOF
  &gen_be_stage4_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${VARIABLE}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    k = ${VINDEX},
    testing_spectral = ${TESTING_SPECTRAL},
    data_on_levels = ${DATA_ON_LEVELS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage4.exe  > gen_be_stage4.log

         set VINDEX = `expr $VINDEX + 1`
      end # Loop over levels
   end    # Loop over control variables

endif

#------------------------------------------------------------------------
#  Finally, gather data together into a single BE file:
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_DIAGS ) then
   cp ${SRC_DIR}/gen_be_diags.exe .

cat >! gen_be_diags_nl.nl << EOF
  &gen_be_diags_nl
    be_method = '${BE_METHOD}' /
EOF

      ./gen_be_diags.exe  > gen_be_diag.log
endif

exit(0)
