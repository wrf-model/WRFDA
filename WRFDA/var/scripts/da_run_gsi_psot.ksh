#!/bin/ksh
#########################################################################
# Script: da_run_gsi_psot.ksh
#
# Purpose: Main script for running GSI single obs tests
# Author : Syed RH Rizvi, MMM/ESSL/NCAR,  Date:04/15/2009
#########################################################################

# Description:
#          PSEUDO_VAR_SIZE: number of tests.
#          PSEUDO_VAR_LIST: list of variables 
#          PSEUDO_VAL_LIST: list of innovations'
#          PSEUDO_ERR_LIST: list of observation errors       
#          PSEUDO_LAT_LIST: list of latitudes   
#          PSEUDO_LON_LIST: list of longitudes  
#          PSEUDO_P_LIST  : list of vertical locations in pressure (hPa)
#          
#########################################################################
#------------------------------------------------------------------------
#Set defaults for required environment variables
#------------------------------------------------------------------------
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

ivar=0
for var in $PSEUDO_VAR_LIST; do
   (( ivar=ivar+1 ))
   export PSEUDO_VAR[$ivar]=$var
done
if [[ $ivar != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_VAR_LIST($ivar) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)! "
   exit 1
fi

ival=0
for var in $PSEUDO_VAL_LIST; do
   (( ival=ival+1 ))
   export PSEUDO_VAL[$ival]=$var
done
if [[ $ival != $PSEUDO_VAR_SIZE ]] ; then
   echo "Error: Size of PSEUDO_VAL_LIST($ival) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

ierr=0
for var in $PSEUDO_ERR_LIST; do
   (( ierr=ierr+1 ))
export PSEUDO_ERR[$ierr]=$var
done
if [[ $ierr != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_ERR_LIST($ierr) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

ix=0
for var in $PSEUDO_LAT_LIST; do
   (( ix=ix+1 ))
   export PSEUDO_LAT[$ix]=$var
done
if [[ $ix != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_LAT_LIST($ix) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

iy=0
for var in $PSEUDO_LON_LIST; do
   (( iy=iy+1 ))
   export PSEUDO_LON[$iy]=$var
done
if [[ $iy != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_LON_LIST($iy) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

iz=0
for var in $PSEUDO_P_LIST; do
   (( iz=iz+1 ))
   export PSEUDO_P[$iz]=$var
done
if [[ $iz != $PSEUDO_VAR_SIZE ]]; then
   echo "Error: Size of PSEUDO_P_LIST($iz) does not match with PSEUDO_VAR_SIZE($PSEUDO_VAR_SIZE)!"
   exit 1
fi

#------------------------------------------------------------------------
#PSOT: loop for different variables
 cd  ${EXP_DIR}
#------------------------------------------------------------------------
export FC_DIR_SAVE=$FC_DIR
iv=1
while [[ $iv -le $PSEUDO_VAR_SIZE ]]; do
   export PSEUDO_VAR=${PSEUDO_VAR[$iv]}
   export PSEUDO_VAL=${PSEUDO_VAL[$iv]}
   export PSEUDO_ERR=${PSEUDO_ERR[$iv]}
   export PSEUDO_LAT=${PSEUDO_LAT[$iv]}
   export PSEUDO_LON=${PSEUDO_LON[$iv]}
   export PSEUDO_P=${PSEUDO_P[$iv]}

   echo "Single obs test for $PSEUDO_VAR($PSEUDO_LAT,$PSEUDO_LON,$PSEUDO_P) with val=$PSEUDO_VAL and err=$PSEUDO_ERR"
 
   export FC_DIR=${FC_DIR_SAVE}/psot$iv
 
   if test ! -d $DAT_DIR; then mkdir $DAT_DIR; fi
   if test ! -d $REG_DIR; then mkdir $REG_DIR; fi
   if test ! -d $EXP_DIR; then mkdir $EXP_DIR; fi
   if test ! -d $EXP_DIR/run; then mkdir $EXP_DIR/run; fi
   if test ! -d $FC_DIR_SAVE; then mkdir $FC_DIR_SAVE; fi
   if test ! -d $FC_DIR; then mkdir $FC_DIR; fi

   export DATE=$INITIAL_DATE

      if test ! -d $FC_DIR/$DATE; then mkdir -p $FC_DIR/$DATE; fi
 
      export RUN_DIR=$EXP_DIR/run/$DATE/psot$iv
      mkdir -p $RUN_DIR
 
      export DA_FIRST_GUESS=${RC_DIR}/$DATE/wrfinput_d${DOMAINS}
      export DA_ANALYSIS=$FC_DIR/$DATE/wrfinput_d${DOMAINS}

      #-----------------------------------------------------------------------
      #PSOT: submit job
      #-----------------------------------------------------------------------
      export WRFVAR_VN=$(svnversion -n \$WRFVAR_DIR 2>/dev/null)

      if [[ $SUBMIT != LSF ]]; then
          echo "Only works with LSF queueing system"
          exit 1
      fi


      cat > job.ksh <<EOF
#!/bin/ksh
#
# LSF batch script
#
############BSUB -a mpich_gm
#BSUB -a poe
#BSUB -n $NUM_PROCS
#BSUB -J $EXPT
#BSUB -o $EXPT.out
#BSUB -e $EXPT.err
#BSUB -q $QUEUE
#BSUB -P $PROJECT
#BSUB -W $WALLCLOCK
#BSUB -R "span[ptile=$LSF_PTILE]"
############BSUB -w \"done(${PREV_JOBID})\"

. ${SCRIPTS_DIR}/da_run_gsi.ksh > $RUN_DIR/index.html 2>&1

EOF

      chmod +x job.ksh
      bsub -q $QUEUE -n $NUM_PROCS < job.ksh  

      RC=$?
      if test $RC != 0; then
         echo $(date) "${ERR}Failed with error $RC$END"
         exit 1
      fi

   echo

   ((iv=iv+1 ))
done #end of loop for variables

exit 0
