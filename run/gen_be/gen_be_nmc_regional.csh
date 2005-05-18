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

   set Beginning_cpu_time = `date`
   echo "Beginning CPU time: ${Beginning_cpu_time}"

setenv WRFVAR_DIR /snowdrift/users/dmbarker/code_development/wrf3dvar 
setenv DAT_DIR /snowdrift/users/dmbarker/200405.amps2
setenv RUN_DIR /var/tmp/dmbarker
#setenv DAT_DIR /data3/dmbarker/data/amps2/noobs/gen_be_input 

setenv TESTING_EOFS .FALSE.
setenv TESTING_SPECTRAL .FALSE.
setenv BIN_TYPE 5
setenv NUM_PASSES 0
setenv EXPT NMC.bin_type${BIN_TYPE}
setenv START_DATE  2004050112
setenv END_DATE    2004053000
#uh_method = 'power' or 'scale'
  set UH_METHOD = 'scale'

#setenv RUN_GEN_BE_STAGE1 # Set to run stage 1 (Remove mean, split variables).
#setenv RUN_GEN_BE_STAGE2 # Set to run stage 2 (Regression Coefficients).
#setenv RUN_GEN_BE_STAGE2A # Set to run stage 2 (Regression Coefficients).
#setenv RUN_GEN_BE_STAGE3 # Set to run stage 3 (Vertical Covariances).
#setenv RUN_GEN_BE_STAGE4 # Set to run stage 4 (Horizontal Covariances).
setenv RUN_GEN_BE_DIAGS  # Set to run gen_be diagnostics.
setenv RUN_GEN_BE_DIAGS_READ  # Set to run gen_be diagnostics_read.

if ( ! $?START_DATE )    setenv START_DATE    2002010512 # Starting time of period.
if ( ! $?END_DATE )      setenv END_DATE      2002010800 # Ending time of period.
if ( ! $?INTERVAL )      setenv INTERVAL      12         # Period between files (hours).
if ( ! $?BE_METHOD )     setenv BE_METHOD     NMC        # NMC (NMC-method), ENS (Ensemble-Method).
if ( ! $?NE )            setenv NE 1                     # Number of ensemble members (for ENS).
if ( ! $?GAUSSIAN_LATS ) setenv GAUSSIAN_LATS .false.    # Set if Gaussian latitudes used.
if ( ! $?REMOVE_MEAN )   setenv REMOVE_MEAN   .true.     # Remove time/ensemble/area mean.
if ( ! $?BIN_TYPE )      setenv BIN_TYPE      1          # 0=None, 1=1:ni, 2=latitude, ....
if ( ! $?LAT_MIN )       setenv LAT_MIN       -90.0      # Used if BIN_TYPE = 2.
if ( ! $?LAT_MAX )       setenv LAT_MAX       90.0       # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_LAT )  setenv BINWIDTH_LAT  10.0       # Used if BIN_TYPE = 2.
if ( ! $?HGT_MIN )       setenv HGT_MIN       0.0        # Used if BIN_TYPE = 2.
if ( ! $?HGT_MAX )       setenv HGT_MAX       20000.0    # Used if BIN_TYPE = 2.
if ( ! $?BINWIDTH_HGT )  setenv BINWIDTH_HGT  1000.0     # Used if BIN_TYPE = 2.
if ( ! $?NUM_PASSES )    setenv NUM_PASSES    2          # Number of passes of recursive filter.
if ( ! $?RF_SCALE )      setenv RF_SCALE      1.0        # Recursive filter scale.
if ( ! $?TESTING_EOFS )  setenv TESTING_EOFS .false.     # True if performing EOF tests.
if ( ! $?USE_GLOBAL_EOFS ) setenv USE_GLOBAL_EOFS .true. # True if using global EOFs.
if ( ! $?DATA_ON_LEVELS )  setenv DATA_ON_LEVELS .false. # False if fields projected onto modes.
if ( ! $?NUM_LEVELS )    setenv NUM_LEVELS    31         # Hard-wired for now....
if ( ! $?RESOLUTION_KM ) setenv RESOLUTION_KM 30         # Hard-wired for now....
if ( ! $?TESTING_SPECTRAL ) setenv TESTING_SPECTRAL .false.  # True if performing spectral tests.
if ( ! $?EXPT )          setenv EXPT 2002-01.NMC         # Experiment ID

if ( ! $?WRFVAR_DIR )    setenv WRFVAR_DIR ${WRFVAR_DIR}/wrf3dvar
if ( ! $?SRC_DIR )       setenv SRC_DIR ${WRFVAR_DIR}/main/gen_be
if ( ! $?DAT_DIR )       setenv DAT_DIR /mmmtmp1/guo/gen_be_run
if ( ! $?RUN_DIR )       setenv RUN_DIR /mmmtmp1/guo/gen_be_run

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

   ln -sf ${SRC_DIR}/gen_be_stage1.exe .

cat >! gen_be_stage1_nl.nl << EOF
  &gen_be_stage1_nl
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
    remove_mean = ${REMOVE_MEAN},
    gaussian_lats = ${GAUSSIAN_LATS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage1.exe >& gen_be_stage1.log
#   rm -rf gen_be_stage1.exe
endif

#------------------------------------------------------------------------
#  Run Stage 2: Calculate regression coefficients.
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE2 ) then

   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients."
   echo "---------------------------------------------------------------"

   ln -sf ${SRC_DIR}/gen_be_stage2.exe .

cat >! gen_be_stage2_nl.nl << EOF
  &gen_be_stage2_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    testing_eofs = ${TESTING_EOFS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage2.exe >& gen_be_stage2.log
#   rm -rf gen_be_stage2.exe
endif
#------------------------------------------------------------------------
#  Run Stage 2a: Calculate control variable fields.
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE2A ) then

   echo "---------------------------------------------------------------"
   echo "Run Stage 2a: Calculate control variable fields."
   echo "---------------------------------------------------------------"

   ln -sf ${SRC_DIR}/gen_be_stage2a.exe .

cat >! gen_be_stage2a_nl.nl << EOF
  &gen_be_stage2a_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    be_method = '${BE_METHOD}',
    ne = ${NE},
    num_passes = ${NUM_PASSES},
    rf_scale = ${RF_SCALE},
    testing_eofs = ${TESTING_EOFS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

   ./gen_be_stage2a.exe >& gen_be_stage2a.log
endif

#------------------------------------------------------------------------
#  Run Stage 3: Read 3D fields for control variable, and calculate vertical
#  covariances,
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE3 ) then

   echo "---------------------------------------------------------------"
   echo "3) Perform EOF decomposition of fields (gen_be_stage3)."
   echo "---------------------------------------------------------------"

   ln -sf ${SRC_DIR}/gen_be_stage3.exe .

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
    lat_min = ${LAT_MIN},
    lat_max = ${LAT_MAX},
    binwidth_lat = ${BINWIDTH_LAT},
    hgt_min = ${HGT_MIN},
    hgt_max = ${HGT_MAX},
    binwidth_hgt = ${BINWIDTH_HGT},
    testing_eofs = ${TESTING_EOFS},
    use_global_eofs = ${USE_GLOBAL_EOFS},
    data_on_levels = ${DATA_ON_LEVELS},
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

         ./gen_be_stage3.exe >& gen_be_stage3.log
      endif
   end
endif

#------------------------------------------------------------------------
#  Run Stage 4: Read 2D (horizontal) fields for control variable, 
#  and calculate horizontal covariances,
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_STAGE4 ) then
   if (${UH_METHOD} == 'power') then 
   
   echo "---------------------------------------------------------------"
   echo "3) Perform spectral decomposition of fields."
   echo "---------------------------------------------------------------"

   ln -sf ${SRC_DIR}/gen_be_stage4.exe .

   foreach CV ( $CONTROL_VARIABLES )
      setenv VARIABLE $CV

      if ( $CV == "ps"  ) then
         setenv MAX_VINDEX 1
      else if ( $CV == "ps_u" ) then
         setenv MAX_VINDEX 1
      else
         setenv MAX_VINDEX $NUM_LEVELS
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
    expt = '${EXPT}',
    dat_dir = '${DAT_DIR}' /
EOF

         ./gen_be_stage4.exe >& gen_be_stage4.log

         set VINDEX = `expr $VINDEX + 1`
      end # Loop over levels
   end    # Loop over control variables

   else 
#----- Scale length
#   set CV = $CONTROL_VARIABLES
   foreach CV ( $CONTROL_VARIABLES )

   set Starting_cpu_time = `date`
   echo "============== ${CV}: $Starting_cpu_time ==============="

   if ($CV == "ps_u" || $CV == "ps" || $CV == "ps_b") then
      set ks = 1
      set ke = 1
      setenv VERTICAL_LEV 1

   else
      set ks = 1
      set ke = ${NUM_LEVELS}
      setenv VERTICAL_LEV $ke
   endif

   while (${ks} <= ${ke})
cat >! scale_length_nl.nl << EOF
  &scale_length_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}',
    interval = ${INTERVAL},
    variable = '${CV}',
    be_method = '${BE_METHOD}',
    ne = ${NE},
    nk = ${ks},
    vertical_level = ${VERTICAL_LEV},
    cut_dist_km = 6000.,
    resolution_km = ${RESOLUTION_KM}/
EOF
   ln -sf ${SRC_DIR}/gen_be_stage4_scale.exe .

   ./gen_be_stage4_scale.exe >& gen_be_stage4_scale.log

   @ ks = ${ks} + 1
   end 

end

endif

endif

#------------------------------------------------------------------------
#  Finally, gather data together into a single BE file:
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_DIAGS ) then
   ln -sf ${SRC_DIR}/gen_be_diags.exe .

#uh_method = 'power' or 'scale'
#  set UH_METHOD = 'scale'

cat >! gen_be_diags_nl.nl << EOF
  &gen_be_diags_nl
    be_method = '${BE_METHOD}',
    uh_method = '${UH_METHOD}' /
EOF

      ./gen_be_diags.exe >& gen_be_diags.log

#mv gen_be.${BE_METHOD}.dat gen_be.${BE_METHOD}.dat.${UH_METHOD}

endif

#------------------------------------------------------------------------
#  Read BE file to check data packed correctly, and write plot diagnostics.
#------------------------------------------------------------------------

if ( $?RUN_GEN_BE_DIAGS_READ ) then

cat >! gen_be_diags_nl.nl << EOF
  &gen_be_diags_nl
    be_method = '${BE_METHOD}',
    uh_method = '${UH_METHOD}' /
EOF

   ln -sf ${SRC_DIR}/gen_be_diags_read.exe .
   ./gen_be_diags_read.exe >& gen_be_diags_read.log

endif

   set Ending_cpu_time = `date`
   echo "Ending CPU time: ${Ending_cpu_time}"

exit(0)
