#!/bin/ksh  -aeux
#########################################################################
# Script: da_plot_psot.ksh
#
# Author : Syed RH Rizvi, NCAR/NESL/MMM/DAS,  Date:04/15/2009
# Purpose: Ploting script for single obs tests (works both for GSI & WRFDA)
#
# Update : Syed RH Rizvi, NCAR/NESL/MMM/DAS,  Date:07/22/2010
#          plots directly WRFDA analysis_incremets produced with
#          WRITE_INCREMENTS=true for PSOT test
#
#########################################################################
#------------------------------------------------------------------------
#Set defaults for required environment variables
#------------------------------------------------------------------------
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/WRFDA}
export SCRIPTS_DIR=${SCRIPTS_DIR:-$WRFVAR_DIR/var/scripts}
export GRAPHICS_DIR=${GRAPHICS_DIR:-$WRFVAR_DIR/var/graphics/ncl}

. ${SCRIPTS_DIR}/da_set_defaults.ksh

echo "expt_dir= $EXP_DIR"
export PLOT_DIR=${PLOT_DIR:-$EXP_DIR/plotpsot} #where will be the plots

if test ! -d $PLOT_DIR; then mkdir $PLOT_DIR; fi
cd $PLOT_DIR

export PLOT_WKS=${PLOT_WKS:-pdf}

#-------------------------------------------------------------------------
# convert pseudo list into array
#-------------------------------------------------------------------------
ivar=0
for var in $PSEUDO_VAR_LIST; do
   (( ivar=ivar+1 ))
   export PSEUDO_VAR[$ivar]=$var
done

ival=0
for var in $PSEUDO_VAL_LIST; do
   (( ival=ival+1 ))
   export PSEUDO_VAL[$ival]=$var
done

ierr=0
for var in $PSEUDO_ERR_LIST; do
   (( ierr=ierr+1 ))
   export PSEUDO_ERR[$ierr]=$var
done

ix=0
for var in $PSEUDO_X_LIST; do
   (( ix=ix+1 ))
   export PSEUDO_X[$ix]=$var
done

iy=0
for var in $PSEUDO_Y_LIST; do
   (( iy=iy+1 ))
   export PSEUDO_Y[$iy]=$var
done

iz=0
for var in $PSEUDO_Z_LIST; do
   (( iz=iz+1 ))
   export PSEUDO_Z[$iz]=$var
done

#-------------------------------------------------------------------------
# plot
#-------------------------------------------------------------------------
iv=1
for var in ${PSEUDO_VAR[*]}; do

   expt=psot$iv
   xlon=${PSEUDO_X[$iv]} 
   xlat=${PSEUDO_Y[$iv]} 
   kl=${PSEUDO_Z[$iv]}


   export ANALYSIS=${EXP_DIR}/fc/psot$iv/$INITIAL_DATE/wrfinput_d${DOMAINS}
   export DA_FIRST_GUESS=${DA_FIRST_GUESS:-${RC_DIR}/$INITIAL_DATE/wrfinput_d01}
   export ANAL_INCREMENTS=${EXP_DIR}/run/$INITIAL_DATE/psot$iv/analysis_increments


   if $RUN_PSOT_GSI ; then 
     if $ONEOB_ONGRID ; then 
       NCL_COMMAND_LINE="'works=\"${PLOT_WKS}\"' 'expt=\"$expt\"'  \
                'kl=$kl' 'xlon=$xlon' 'xlat=$xlat' \
                'bakfile=\"$DA_FIRST_GUESS\"' 'analfile=\"$ANALYSIS\"'"
      echo "ncl ${NCL_COMMAND_LINE} $GRAPHICS_DIR/psot.ncl" > run_psot_ncl
     else 
      iz=0
      for var in $PSEUDO_LAT_LIST; do
      (( iz=iz+1 ))
       export PSEUDO_LAT[$iz]=$var
      done
      iz=0
      for var in $PSEUDO_LON_LIST; do
      (( iz=iz+1 ))
       export PSEUDO_LON[$iz]=$var
      done

       xlat=${PSEUDO_LAT[$iv]} 
       xlon=${PSEUDO_LON[$iv]} 
       NCL_COMMAND_LINE="'works=\"${PLOT_WKS}\"' 'expt=\"$expt\"'  \
                'kl=$kl' 'xlon=$xlon' 'xlat=$xlat' \
                'bakfile=\"$DA_FIRST_GUESS\"' 'analfile=\"$ANALYSIS\"'"
      echo "ncl ${NCL_COMMAND_LINE} $GRAPHICS_DIR/psot_gsi_on_latlon.ncl" > run_psot_ncl
     fi
   fi
   if $RUN_PSOT_WRFVAR ; then 
     if $PLOT_ANAL_INC ; then 
      NCL_COMMAND_LINE="'works=\"${PLOT_WKS}\"' 'expt=\"$expt\"'  \
            'kl=$kl' 'xlon=$xlon' 'xlat=$xlat' \
            'bakfile=\"$DA_FIRST_GUESS\"' 'anal_inc_file=\"$ANAL_INCREMENTS\"'"
      echo "ncl ${NCL_COMMAND_LINE} $GRAPHICS_DIR/plot_anal_inc.ncl" > run_psot_ncl
     else
      NCL_COMMAND_LINE="'works=\"${PLOT_WKS}\"' 'expt=\"$expt\"'  \
            'kl=$kl' 'xlon=$xlon' 'xlat=$xlat' \
            'bakfile=\"$DA_FIRST_GUESS\"' 'analfile=\"$ANALYSIS\"'"
      echo "ncl ${NCL_COMMAND_LINE} $GRAPHICS_DIR/psot.ncl" > run_psot_ncl
     fi
   fi
       chmod +x run_psot_ncl
       ./run_psot_ncl
       rm -f run_psot_ncl
   (( iv=iv+1 ))
done

exit 0
