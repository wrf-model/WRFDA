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

#-----------------------------------------------------------------------------------
# Don't change anything below this line.
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export GEN_BE_DIR=${GEN_BE_DIR:-$REL_DIR/gen_be}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$GEN_BE_DIR/scripts}

. ${SCRIPTS_DIR}/gen_be_set_defaults.ksh

if [[ ! -d $RUN_DIR ]]; then mkdir -p $RUN_DIR; fi
if [[ ! -d $STAGE0_DIR ]]; then mkdir -p $STAGE0_DIR; fi

mkdir -p $WORK_DIR
cd $WORK_DIR

# List of standard variables:
if [[ $MASSCV == "temp" ]]; then
    STANDARD_VARIABLES="fullflds psi chi t rh ps"
else
    if [[ $BALPRES == "purestats" ]]; then
        STANDARD_VARIABLES="fullflds psi chi p rh"
    else
        STANDARD_VARIABLES="fullflds psi chi p lbp rh"
    fi
fi
for SV in $STANDARD_VARIABLES; do mkdir -p $SV; done

# List of control variables:

for CV in $CONTROL_VARIABLES; do mkdir -p $CV; done

# quick add !--ym--!
for CV in raincl qcloud qrain qsnow qice rh_psu rhm qcondm qcond vor div; do mkdir -p $CV; done

#------------------------------------------------------------------------
# Run Stage 0: Calculate ensemble perturbations from model forecasts.
#------------------------------------------------------------------------

echo 
echo $(date) "Start"
echo "GEN_BE_DIR is" $GEN_BE_DIR $(svnversion $GEN_BE_DIR)

if $RUN_GEN_BE_STAGE0; then
   echo "---------------------------------------------------------------"
   echo "Run Stage 0: Calculate ensemble perturbations from model forecasts."
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   if [ $MODEL = "WRF" ]; then
       $SCRIPTS_DIR/gen_be_stage0_wrf.ksh
   elif [ $MODEL = "UM" ]; then
       $SCRIPTS_DIR/gen_be_stage0_um.ksh
   else
       echo "Error in the model name" > $RUN_DIR/FAIL
       exit 1
   fi

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 0 for $MODEL failed with error" $RC
      echo "stage 0" > $RUN_DIR/FAIL
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
    dat_dir = '${STAGE0_DIR}',
    masscv = '${MASSCV}',
    balpres = '${BALPRES}',
    vertical_ip =  ${VERTICAL_IP},
    model = '${MODEL}',
    N_holm_bins = ${N_HOLM_BINS}/
EOF

   ./gen_be_stage1.exe > gen_be_stage1.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 1 failed with error" $RC
      echo "stage 1" > $RUN_DIR/FAIL
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
    masscv = '${MASSCV}',
    balpres = '${BALPRES}',
    nobaldiv = ${NOBALDIV},
    testing_eofs = ${TESTING_EOFS} /
EOF

   ./gen_be_stage2.exe > gen_be_stage2.log 2>&1
   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2 failed with error" $RC
      echo "stage 2" > $RUN_DIR/FAIL
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
    bin_type = ${BIN_TYPE},
    cv_options = ${CV_OPTIONS},
    num_passes = ${NUM_PASSES},
    masscv = '${MASSCV}',
    balpres = '${BALPRES}',
    rf_scale = ${RF_SCALE} /
EOF

   ./gen_be_stage2a.exe > gen_be_stage2a.log 2>&1

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "Stage 2a failed with error" $RC
      echo "stage 2a" > $RUN_DIR/FAIL
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
         echo "stage 3" > $RUN_DIR/FAIL
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

      ${SCRIPTS_DIR}/gen_be_stage4_global.ksh > gen_be_stage4_global.log 2>&1

   else
      echo "---------------------------------------------------------------"
      echo "Run Stage 4: Calculate horizontal covariances (regional lengthscales)."
      echo "---------------------------------------------------------------"

      ${SCRIPTS_DIR}/gen_be_stage4_regional.ksh > gen_be_stage4_regional.log 2>&1
      RC=$?
      if [[ $RC != 0 ]]; then
         echo "Stage 4 failed with error" $RC
         echo "stage 4" > $RUN_DIR/FAIL
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
      echo "gen_be_diags" > $RUN_DIR/FAIL
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
      echo "gen_be_diags_read" > $RUN_DIR/FAIL
      exit 1
   fi
fi

#------------------------------------------------------------------------
#  Calculate multivariate regression diagnostics:
#------------------------------------------------------------------------

if $RUN_GEN_BE_MULTICOV; then

   export VARIABLE1=t
   export VARIABLE2=rh
   export HREF=2
   
   $SCRIPTS_DIR/gen_be_cov3d.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_cov3d failed with error" $RC
      echo "gen_be_cov3d " > $RUN_DIR/FAIL
      exit 1
   fi

fi
#------------------------------------------------------------------------
#  Calculate histogram diagnostics:
#------------------------------------------------------------------------
if $RUN_GEN_BE_HISTOG; then
   echo "---------------------------------------------------------------"
   echo "Run Hist: Diagnose pdf for perturbations"
   echo "---------------------------------------------------------------"

   export BEGIN_CPU=$(date)
   echo "Beginning CPU time: ${BEGIN_CPU}"

   ln -sf ${BUILD_DIR}/gen_be_hist.exe .

   for CV in t rh rhm qcond qcondm; do
   export HREF=2
#      for HREF in 1 2 3 4 5 6; do
      	cat > gen_be_hist_nl.nl << EOF
&gen_be_hist_nl
    start_date = '${START_DATE}',
    end_date = '${END_DATE}', 
    interval = ${INTERVAL},
    variable = '${CV}',
    ne = ${NE},
    Nstdev = 5,
    N_dim_hist = 20,
    holm_reference = $HREF,
    N_holm_bins = $N_HOLM_BINS/
EOF

      	./gen_be_hist.exe > gen_be_hist.${CV}.log 2>&1

      	RC=$?
      	if [[ $RC != 0 ]]; then
         	echo "Hist for $CV failed with error" $RC
         	echo "Hist" > $RUN_DIR/FAIL
         	exit 1
      	fi
      done
#   done

   export END_CPU=$(date)
   echo "Ending CPU time: ${END_CPU}"
fi
#------------------------------------------------------------------------
if $RUN_GEN_BE_GRAPHICS; then

   $SCRIPTS_DIR/gen_be_graphics.ksh

   RC=$?
   if [[ $RC != 0 ]]; then
      echo "gen_be_graphics failed with error" $RC
      echo "gen_be_graphics" > $RUN_DIR/FAIL
      exit 1
   fi
fi

# Preserve the interesting log files
mv $WORK_DIR/*log $RUN_DIR
mv $STAGE0_DIR/*log $RUN_DIR
mv $WORK_DIR/be.dat $RUN_DIR
mv $WORK_DIR/*.pdf  $RUN_DIR
mv $WORK_DIR/*.dat  $RUN_DIR
mv $WORK_DIR/fort.* $RUN_DIR

if $CLEAN; then rm -rf $WORK_DIR; fi

echo
echo $(date) "Finished"

touch $RUN_DIR/SUCCESS

exit 0
