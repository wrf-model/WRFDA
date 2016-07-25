#!/bin/ksh -aeux
#########################################################################
# Script: wrapper_run_gsi_psot.ksh
#
# Purpose: Wrapper for GSI  Single Obs tests 
#
# Author:  Syed RH Rizvi, MMM/ESSL/NCAR,  Date:04/15/2009
#########################################################################
#export PROJECT=48500053    #Monthly
export PROJECT=48503002   #DATC


export WALLCLOCK=5
export QUEUE=premium
export NUM_PROCS=8 
set echo
#------------------------------------------------------------------------
#  Set variable to run psot either for GSI or WRF-Var
export RUN_PSOT_WRFVAR=true     
export RUN_PSOT_GSI=false         

# Having run psot set follwing variable true to draw plots, 
# otherwise set it to false                                 
#
export RUN_PLOT_PSOT=true  
#------------------------------------------------------------------------

export REGION=katrina
#Directories:
export        DAT_DIR=/ptmp/rizvi/data              
export        REG_DIR=$DAT_DIR/$REGION              
export        RC_DIR=$REG_DIR/rc
export        OB_DIR=$REG_DIR/ob

export        WRFVAR_DIR=/mmm/users/rizvi/code/WRFDA_TRUNK_2009April12_3818
#export        WRFVAR_DIR=/ptmp/rizvi/trunk_v_3929

export        GRAPHICS_DIR=$WRFVAR_DIR/var/graphics/ncl
#export        GSI_DIR=/mmm/users/rizvi/code/BOULDER_GSI_Q1FY09
export        GSI_DIR=/ptmp/rizvi/GSI_Q1FY09


#Experiment details:
export GSI_QOPTION=2
export INTERPOLATE_REG_STATS=true    # default = true 
export ONEOB_ONGRID=true           

export CLEAN=false
#-------------------------------------------------------------------------
#Time info
export INITIAL_DATE=2005082400
export   FINAL_DATE=$INITIAL_DATE

#export DA_FIRST_GUESS=$REG_DIR/rc/$INITIAL_DATE/wrfinput_d01              
export DA_FIRST_GUESS=$REG_DIR/exp-1/run_06Hr_cycling/fc/2005082400/wrfinput_d01
#-------------------------------------------------------------------------
if $RUN_PSOT_GSI ;then
export ONEOBTEST=true

export BE_FILE=nam_nmmstat_na_glberror
export DA_BACK_ERRORS=/ptmp/rizvi/ncep_be/${BE_FILE}
export EXPT=run_gsi_psot-glberror-qopt_${GSI_QOPTION}
##
#
#export BE_FILE=nmmstat
#export BEDIR=flg_2-no_f-pscb-lat_4.0
#export DA_BACK_ERRORS=$REG_DIR/${BEDIR}/${BE_FILE}
#export EXPT=run_gsi_psot_${BEDIR}

#
#==================================================================
#           For tuning GSI parameters 
#==================================================================
### Default values GSI_AS=0.6,0.6,0.75,0.75,0.75},0.75,1.0,1.0
#
##
#export GSI_AS1=1.0   
#export GSI_AS2=1.0 
#export GSI_AS4=1.0
#export GSI_AS5=1.0 
#export GSI_AS3=1.0 
###
### Default value  GSI_VS=0.7
#export GSI_VS=1.0
##
### Default values for Horizontal scalelengths GSI_HZSCL1 = 1.7,0.8,0.5
#export GSI_HZSCL1=1.0  
#export GSI_HZSCL2=1.0 
#export GSI_HZSCL3=1.0 
##
### Default values are GSI_HSWGT1=0.45,0.30,0.25
#export GSI_HSWGT1=1.0
#export GSI_HSWGT2=1.0
#export GSI_HSWGT3=1.0
#
###==================================================================
#

export        RUN_DIR=$REG_DIR/$EXPT          

export        EXP_DIR=$RUN_DIR
export        FC_DIR=$EXP_DIR/fc


export PSEUDO_VAR_SIZE=3                   #Single obs tests number                      
export PSEUDO_VAR_LIST="u t q"             #List of variables
export PSEUDO_VAL_LIST="1. 1. 1."          #Innov size                           
export PSEUDO_ERR_LIST="1. 1. 1."          #Obs errors                           
export PSEUDO_LAT_LIST="30.0 30.0 30.0"    #Lat locations       
export PSEUDO_LON_LIST="-85. -85. -00."    #Lon locations       
export PSEUDO_P_LIST="500 500 500"            #Pr  locations

export PSEUDO_X_LIST="230 230 230"            #Obs X-loc consistent with PSEUDO_LON_LIST
export PSEUDO_Y_LIST="175 175 175"         #Obs Y-loc consistent with PSEUDO_LAT_LIST
export PSEUDO_Z_LIST="25 25 25"            #Obs Z-loc consistent with PSEUDO_P_LIST
#
if $RUN_PLOT_PSOT ;then
$WRFVAR_DIR/var/scripts/da_plot_psot_gsi.ksh
#$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
else
rm -rf $RUN_DIR
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_gsi_psot.ksh 
$WRFVAR_DIR/var/scripts/da_run_job.ksh
fi
fi

#-------------------------------------------------------------------------
if $RUN_PSOT_WRFVAR ;then
#export DA_BACK_ERRORS=$REG_DIR/exp-1/run_gen_be/be.dat 
export DA_BACK_ERRORS=$REG_DIR/exp-5/run_gen_be/be.dat 
export EXPT=run_wrfvar_psot

export        RUN_DIR=$REG_DIR/$EXPT          
export        EXP_DIR=$RUN_DIR
export        FC_DIR=$EXP_DIR/fc
export        BE_DIR=$REG_DIR/be

export NL_WRITE_INCREMENTS=true
#export MAP_PROJ=mercator
export NL_E_WE=460
export NL_E_SN=351
export NL_E_VERT=51
export REF_LAT=30.0
export REF_LON=-85.0
export TRUELAT1=30.0
export TRUELAT2=30.0
export STAND_LON=-85.0
export NL_DX=12000
export NL_DY=12000

export NL_USE_BASEPARAM_FR_NML=true

export PSEUDO_VAR_SIZE=3
export PSEUDO_VAR_LIST="u t q"         #  Can be    "u   u    t    t      q"
export PSEUDO_VAL_LIST="1. 1. 0.001"       #  Should be "1.0 1.0 1.0  1.0  0.001"
export PSEUDO_ERR_LIST="1. 1. 0.001"       #  Should be "1.0 1.0 1.0  1.0  0.001"
export PSEUDO_X_LIST="230 230 230"            #Obs X-loc consistent with PSEUDO_LON_LIST
export PSEUDO_Y_LIST="175 175 175"         #Obs Y-loc consistent with PSEUDO_LAT_LIST
export PSEUDO_Z_LIST="25 25 25"            #Obs Z-loc consistent with PSEUDO_P_LIST

if $RUN_PLOT_PSOT ;then
#$WRFVAR_DIR/var/scripts/da_plot_psot_wrfvar.ksh
$WRFVAR_DIR/var/scripts/da_plot_psot.ksh
else
rm -rf $RUN_DIR
export SCRIPT=$WRFVAR_DIR/var/scripts/da_run_wrfvar_psot.ksh
$WRFVAR_DIR/var/scripts/da_run_job.ksh
fi
fi
#-------------------------------------------------------------------------
