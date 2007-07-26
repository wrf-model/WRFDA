#!/bin/ksh
#-----------------------------------------------------------------------
# Purpose : Create BE statistics from input perturbation files.
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
# Run Stage 2: Calculate regression coefficients.
# Run Stage 2a: Calculate control variable fields.
# Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
# Run Stage 4: Calculate horizontal covariances.
# Finally, gather data together into a single BE file.
#-----------------------------------------------------------------------

#set echo

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/gen_be/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir $STAGE0_DIR; fi

#List of control variables:
for SV in fullflds psi chi t rh ps; do
   if [[ ! -d ${RUN_DIR}/$SV ]]; then mkdir ${RUN_DIR}/$SV; fi
done

for CV in $CONTROL_VARIABLES; do
   if [[ ! -d ${RUN_DIR}/$CV ]]; then mkdir ${RUN_DIR}/$CV; fi
done

cd $RUN_DIR

#------------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo "WRFVAR_DIR is" $WRFVAR_DIR $(svnversion $WRFVAR_DIR)

if $RUN_GEN_BE_STAGE0; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 0: Calculate ensemble perturbations from model forecasts."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   $WRFVAR_DIR/scripts/gen_be/gen_be_stage0_wrf.ksh
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 0 for WRF failed with error" $RC
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 1: Read "standard fields", and remove time/ensemble/area mean.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE1; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 1: Read "standard fields", and remove time/ensemble/area mean."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage1.exe .

   cat > gen_be_stage1_nl.nl << EOF
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
    dat_dir = '${STAGE0_DIR}' /
EOF

   ./gen_be_stage1.exe > gen_be_stage1.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 1 failed with error" $RC
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 2: Calculate regression coefficients.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE2; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 2: Calculate regression coefficients."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2.exe .

   cat > gen_be_stage2_nl.nl << EOF
&gen_be_stage2_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    ne = ${NE},
    testing_eofs = ${TESTING_EOFS} /
EOF

   ./gen_be_stage2.exe > gen_be_stage2.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 failed with error" $RC
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 2a: Calculate control variable fields.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE2A; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 2a: Calculate control variable fields."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage2a.exe .

   cat > gen_be_stage2a_nl.nl << EOF
&gen_be_stage2a_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    ne = ${NE},
    num_passes = ${NUM_PASSES},
    rf_scale = ${RF_SCALE} /
EOF

   ./gen_be_stage2a.exe > gen_be_stage2a.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2a failed with error" $RC
      exit 1
   fi

   rm -rf ${DELETE_DIRS} 2> /dev/null
   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Run Stage 3: Read 3D control variable fields, and calculate vertical covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE3; then

   echo "---------------------------------------------------------------"
   echo "Run Stage 3: Read 3D control variable fields, and calculate vertical covariances."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_stage3.exe .

   for CV in $CONTROL_VARIABLES; do

      cat > gen_be_stage3_nl.nl << EOF
&gen_be_stage3_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${CV}',
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
    data_on_levels = ${DATA_ON_LEVELS} /
EOF

      ./gen_be_stage3.exe > gen_be_stage3.${CV}.log 2>&1

      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 3 for $CV failed with error" $RC
         exit 1
      fi
   done

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi

#------------------------------------------------------------------------
#  Run Stage 4: Calculate horizontal covariances.
#------------------------------------------------------------------------

if $RUN_GEN_BE_STAGE4; then

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   if $GLOBAL; then    

      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (global power spectra)."
      echo "---------------------------------------------------------------"

      ${WRFVAR_DIR}/scripts/gen_be/gen_be_stage4_global.ksh > gen_be_stage4_global.log 2>&1

   else

      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)."
      echo "---------------------------------------------------------------"

      ${WRFVAR_DIR}/scripts/gen_be/gen_be_stage4_regional.ksh > gen_be_stage4_regional.log 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 4 failed with error" $RC
         exit 1
      fi

   fi 

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"

fi

#------------------------------------------------------------------------
#  Finally, gather data together into a single BE file:
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS; then
   ln -sf ${BUILD_DIR}/gen_be_diags.exe .

   cat > gen_be_diags_nl.nl << EOF
&gen_be_diags_nl
   uh_method = '${UH_METHOD}',
   n_smth_sl = ${N_SMTH_SL}, /
EOF

   ./gen_be_diags.exe > gen_be_diags.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags failed with error" $RC
      exit 1
   fi

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi

#------------------------------------------------------------------------
#  Read BE file to check data packed correctly, and write plot diagnostics.
#------------------------------------------------------------------------

if $RUN_GEN_BE_DIAGS_READ; then

   cat > gen_be_diags_nl.nl << EOF
&gen_be_diags_nl
   uh_method = '${UH_METHOD}' /
EOF

   ln -sf ${BUILD_DIR}/gen_be_diags_read.exe .
   ./gen_be_diags_read.exe > gen_be_diags_read.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage gen_be_diags_read failed with error" $RC
      exit 1
   fi
fi

#------------------------------------------------------------------------
#  Calculate multivariate regression diagnostics:
#------------------------------------------------------------------------

if $RUN_GEN_BE_MULTICOV; then

   # Calculate chi diagnostics:
   export VARIABLE1=chi_u
   export VARIABLE2=chi

   $WRFVAR_DIR/scripts/gen_be/gen_be_cov3d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov3d (chi) failed with error" $RC
      exit 1
   fi

   # Calculate T diagnostics:
   export VARIABLE1=t_u
   export VARIABLE2=t

   $WRFVAR_DIR/scripts/gen_be/gen_be_cov3d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov3d (T) failed with error" $RC
      exit 1
   fi

   # Calculate ps diagnostics:
   export VARIABLE1=ps_u
   export VARIABLE2=ps

   $WRFVAR_DIR/scripts/gen_be/gen_be_cov2d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov2d failed with error" $RC
      exit 1
   fi

fi

export END_CPU=$(date)
echo "Ending CPU time: ${END_CPU}"

exit 0