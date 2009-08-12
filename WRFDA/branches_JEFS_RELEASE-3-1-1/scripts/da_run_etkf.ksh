#!/bin/ksh
#########################################################################
# Script: da_run_etkf.ksh
#
# Purpose: To perform an Ensemble Transform Kalman Filter (ETKF)
# rescaling of ensemble forecast perturbations.
# The ensemble mean is the WRF-Var analysis.
#
# Note: DATE is defined as the time of the perturbation. We derive
# PREV_DATE (initial time of forecast) using FCST_RANGE.
#
# Owner: Dale Barker
#
#########################################################################

#########################################################################
# Ideally, you should not need to change the code below, but if you 
# think it necessary then please email wrfhelp@ucar.edu with details.
#########################################################################
#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

# name of this script
self=$(/usr/bin/basename $0)

. ${scriptdir}/JME_SharedFunctions.ksh    # To use the JME shared functions

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export NUM_PROCS=1 # will not run parallel
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
. ${SCRIPTS_DIR}/da_set_defaults.ksh
export RUN_DIR=${RUN_DIR:-$EXP_DIR/etkf}
export WORK_DIR=$RUN_DIR/working

export NL_CHECK_MAX_IV=.false.
export NL_ANALYSIS_TYPE="VERIFY"
export DA_ANALYSIS=analysis_not_used
export ETKF_INPUT_DIR=${ETKF_INPUT_DIR:-$FC_DIR}
export ETKF_OUTPUT_DIR=${ETKF_OUTPUT_DIR:-$FC_DIR}
DIRECTORY=${ETKF_INPUT_DIR}/${PREV_DATE}

echo "<HTML><HEAD><TITLE>$EXPT etkf</TITLE></HEAD><BODY><H1>$EXPT etkf</H1><PRE>"

date

#-----------------------------------------------------------------------
# [2] Set up configuration:
#-----------------------------------------------------------------------

mkdir -p $WORK_DIR
cd $WORK_DIR

export PREV_DATE=$($BUILD_DIR/da_advance_time.exe $DATE -$FCST_RANGE)
export YYYY=$(echo $DATE | cut -c1-4)
export MM=$(echo $DATE | cut -c5-6)
export DD=$(echo $DATE | cut -c7-8)
export HH=$(echo $DATE | cut -c9-10)
export FILE_DATE=${YYYY}-${MM}-${DD}_${HH}:00:00
export FILENAME=${FILE_TYPE}_d01_${FILE_DATE}
export DA_FILE=$ETKF_INPUT_DIR/$PREV_DATE/${FILENAME} #JEFS test uses wrfout

#-----------------------------------------------------------------------
# [3] Create observation files for ETKF:
#-----------------------------------------------------------------------
export RUN_WPB=true    # true if DA_FIRST_GUESS is set outside da_run_wrfvar.ksh

echo " \n      Running 0-iteration var for ${NUM_MEMBERS} members" $(date)

export RUN_DIR_SAVE=$RUN_DIR

let MEM=1
let JOB=1
while [[ $MEM -le $NUM_MEMBERS ]]; do

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi
   export DA_FIRST_GUESS=${DIRECTORY}.${CMEM}/${FILENAME}

   export RUN_DIR=$WORK_DIR/wrfvar.${CMEM}
   mkdir -p $RUN_DIR
   cd $RUN_DIR
   echo '   <A HREF="working/wrfvar.'$CMEM'">wrfvar run '$CMEM'</a>'
   $SCRIPTS_DIR/da_run_wrfvar.ksh > index.html 2>&1
   cd $WORK_DIR
   let MEM=$MEM+1
   let JOB=$JOB+1

   if [[ $JOB -gt $NUM_JOBS || $MEM -gt $NUM_MEMBERS ]]; then
      export JOB=1
      wait # Wait for current jobs to finish
   fi
done
echo " Done running 0-iteration var for ${NUM_MEMBERS} members" $(date)

#-----------------------------------------------------------------------
# [3a] Run fortran code that reads in gts_omb_oma files from 0-iteration
#      Var and creates new gts_omb_oma and ob.etkf files that contain a 
#      maximum subset of the observations that pass WRF-Var's quality 
#      control.
#-----------------------------------------------------------------------
echo " \n      Running kaiser for ${NUM_MEMBERS} members" $(date)

${ETKFaid_dir}/kaiser -m ${NUM_MEMBERS} -d ${WORK_DIR} \
                      -j > ${WORK_DIR}/kaiser.out 2>&1
echo " Done running kaiser for ${NUM_MEMBERS} members" $(date)

#----------------------------------------------------------------------
# Look for "SUCCESSFUL COMPLETION" message in kaiser.out file
#----------------------------------------------------------------------
stdOutFile=$WORK_DIR/kaiser.out
kaiser_exec=${ETKFaid_dir}/kaiser

CheckRSLOutput ${stdOutFile} ${self} ${kaiser_exec} ${thisensemble} \
               ${scriptdir} ${logfile} ${email_target} $JME_ValidDate

# ${WORK_DIR}/ob.etkf*.new and ${WORK_DIR}/gts*.new files have been created
#
#--------- Done finding subset of obs ----------------------------------

#-----------------------------------------------------------------------
# [3b] Append the line count from the new ob.etkf*.new file.
#      Keep ob.etkf.000 files created by Var down in the wrfvar.${CMEM}
#      diretories; mv ob.etkf.new files to ob.etkf files
#                  mv gts_omb_oma_01.e???.new files to gts_omb_oma_01.e???
#-----------------------------------------------------------------------
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do

   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
#  export RUN_DIR=$WORK_DIR/wrfvar.${CMEM}/working

#JLS what to do if these files don't exist, kaiser will exit above if it fails....
   wc -l ${WORK_DIR}/ob.etkf.${CMEM}.new > $WORK_DIR/ob.etkf.${CMEM}
   cat ${WORK_DIR}/ob.etkf.${CMEM}.new  >> $WORK_DIR/ob.etkf.${CMEM}
   mv  ${WORK_DIR}/gts_omb_oma_01.${CMEM}.new ${WORK_DIR}/gts_omb_oma_01.${CMEM}

   let MEM=$MEM+1
done

cd $WORK_DIR

#-----------------------------------------------------------------------
# [3c] Clean up *.new files from Kaiser
#-----------------------------------------------------------------------
  rm -f ob.etkf.*.new

#-----------------------------------------------------------------------
# [4] Calculate ensemble mean:
#-----------------------------------------------------------------------

#ln -s "$DIRECTORY/\*.e\*" .    This creates a link called *.e*

#Copy first member as template for mean:

# Initialize ensemble mean and variance with first member
cp ${DIRECTORY}.e001/${FILENAME} ${DIRECTORY}/${FILENAME}
cp ${DIRECTORY}.e001/${FILENAME} ${DIRECTORY}/${FILENAME}.vari

cat > gen_be_ensmean_nl.nl << EOF
  &gen_be_ensmean_nl
    directory = '${DIRECTORY}'
    filename = '${FILENAME}'
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV} /
EOF

echo " \n      Running gen_be_ensmean for ${NUM_MEMBERS} members" $(date)

ln -fs $BUILD_DIR/gen_be_ensmean.exe .
./gen_be_ensmean.exe > gen_be_ensmean.out 2>&1

cp gen_be_ensmean.out $RUN_DIR_SAVE
echo
echo '   <A HREF="gen_be_etkf.out">gen_be_ensmean.out</a>'

echo " Done running gen_be_ensmean for ${NUM_MEMBERS} members" $(date)

#-----------------------------------------------------------------------
# [5] Run Ensemble Transform Kalman Filter:
#-----------------------------------------------------------------------

#Prepare ETKF input/output files:

echo " \n      Running ETKF for ${NUM_MEMBERS} members" $(date)

ln -sf ${DA_FILE} etkf_input                     # ETKF input mean (unchanged)
let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10  ]]; then export CMEM=e00$MEM; fi

   ln -sf ${DIRECTORY}.${CMEM}/${FILENAME} etkf_input.${CMEM}  # ETKF input (unchanged).
   cp ${DIRECTORY}.${CMEM}/${FILENAME} etkf_output.${CMEM}     # ETKF output (overwritten).
   let MEM=$MEM+1
done

#Create namelist and run ETKF:

cat > gen_be_etkf_nl.nl << EOF
  &gen_be_etkf_nl
    num_members = ${NUM_MEMBERS},
    nv = ${NV},
    cv = ${CV},
    naccumt1 = ${NACCUMT1},
    naccumt2 = ${NACCUMT2},
    nstartaccum1 = ${NSTARTACCUM1},
    nstartaccum2 = ${NSTARTACCUM2},
    nout = ${CYCLE_NUMBER},
    tainflatinput = ${TAINFLATINPUT},
    rhoinput = ${RHOINPUT} /
EOF

ln -fs $BUILD_DIR/gen_be_etkf.exe .
./gen_be_etkf.exe > gen_be_etkf.out 2>&1

echo " Done running ETKF for ${NUM_MEMBERS} members" $(date)

cp gen_be_etkf.out $RUN_DIR_SAVE
echo
echo '   <A HREF="gen_be_etkf.out">gen_be_etkf.out</a>'

mkdir -p $ETKF_OUTPUT_DIR/$PREV_DATE
mkdir -p $ETKF_OUTPUT_DIR/$DATE

# Move ensemble mean and variance

# JLS Don't start moving files, it's hard enough to figure out where they are
# mv ${FILE_TYPE}_d01_${FILE_DATE}      $ETKF_OUTPUT_DIR/$PREV_DATE
# mv ${FILE_TYPE}_d01_${FILE_DATE}.vari $ETKF_OUTPUT_DIR/$PREV_DATE

# Move ensemble of analyses:

let MEM=1
while [[ $MEM -le $NUM_MEMBERS ]]; do
   export CMEM=e$MEM
   if [[ $MEM -lt 100 ]]; then export CMEM=e0$MEM; fi
   if [[ $MEM -lt 10 ]]; then export CMEM=e00$MEM; fi
# JLS retain the name of the etkf output files for clarity
#   mv $WORK_DIR/etkf_output.${CMEM} $ETKF_OUTPUT_DIR/$DATE/${FILE_TYPE}_d01_${FILE_DATE}.${CMEM}
#   mv $WORK_DIR/etkf_output.${CMEM} $FC_DIR/$DATE/${FILE_TYPE}_d01_${FILE_DATE}.${CMEM}
   let MEM=$MEM+1
done

if $CLEAN; then
   rm -rf $WORK_DIR
fi

date
echo '</PRE></BODY></HTML>'

exit 0

