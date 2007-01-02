#! /bin/csh -f
#########################################################################
# Script: run_wrf_wrapper.csh
#
# Purpose: Provide user-modifiable interface to run_wrf.csh script.
#
# Author: Dale Barker, MMM Division, NCAR.
#
# History:
# 06/06/2003:  First Version.                                      Dale Barker
# 10/02/2004:  Given to Alain Caya for use in EnKF/WRF-VAr study.  Dale Barker
# 22/08/2005:  Given to Kwan-Young Chung for use in AMPS studies.  Dale Barker
# 10/25/2005:  Tidied up for use in WRF V2.1.1 regression tests.   Dale Barker
#
# Request: Please acknowledge author in work that uses this script.
#
# Description:
#
# Here are a few examples of environment variables you may want to 
# change in run_wrf_wrapper.csh:
#
# 1) "setenv RUN_SI .TRUE." runs the SI).
# 2) "setenv WRF_DIR /data1/dmbarker/WRF_V2.1" points to directory
# containing WRF code (I always include all components as subdirectories
# e.g. $WRF_DIR/wrfvar contains the WRF-Var code.
# 3) "setenv START_DATE 2003010100" begins the experiment at 00 UTC
# 1 January 2003.

# You will see the full list of environment variables, and their default
# values in run_wrf.csh. If one is hard-wired (i.e. not an environment
# variable then email wrfhelp@ucar.edu and we will add it for the next
# release.
#########################################################################

#set echo

#########################################################################
#[1] Decide what stages to run:
#########################################################################

#setenv RUN_RESTORE_DATA .TRUE.
#setenv RUN_SI .TRUE.
#setenv RUN_REAL .TRUE.
#setenv RUN_OBSPROC .TRUE.
setenv RUN_VAR .TRUE.
#setenv RUN_WRF_BC .TRUE.
#setenv RUN_WRF .TRUE.
#setenv CYCLING .TRUE.

#########################################################################
# [2] Override default environment variables here:
#########################################################################

#source ~/.cshrc
#setenv NETCDF /usr/local/netcdf-3.5.0-pgi
#setenv MPICH /snowdrift/users/bray/mpich/mpich-1.2.7p1_pgi
#setenv PATH $MPICH/bin:$PATH

#Disks, directories:
setenv DATA_DISK /ocotillo1
setenv SRC_DIR ${DATA_DISK}/dmbarker/code/WRF_V2.1
setenv WRF_DIR ${SRC_DIR}/WRFV2.1.2
setenv WRFVAR_DIR ${DATA_DISK}/dmbarker/code/branches/wrfvar.rb_v21

#setenv WRF_BC_DIR /ocotillo1/caya/WRF_BC/src
#setenv OBS_DIR /ocotillo1/caya/data/conus200 # Alain's "unclean" obs!
#setenv OBS_DIR /ocotillo1/dmbarker/data/con200/get_obs # Cleaned obs.

#Job details:
setenv EXPT cs1.v21
setenv NUM_PROCS 8 
setenv START_DATE 2003010112
setenv FINAL_DATE 2003010112
setenv FCST_RANGE_OUT 24

#WRF SI:
#smoke: setenv GEOG_DATAROOT /data2/powers/wrfsi_211/extdata/GEOG
#smoke: setenv DATAROOT /data1/dmbarker/data/wrfsi/domains
setenv GEOG_DATAROOT /usr/local/wrfsi/SI_GEOG
setenv DATAROOT /ocotillo5/dmbarker/data/wrfsi/domains
setenv RUN_GRID_GEN .FALSE.

#WRF:
#setenv WRF_DT 600.0
#setenv DEBUG_LEVEL 500

#WRF-Var:
setenv DA_Check_Max_IV .FALSE.
#setenv DA_CV_OPTIONS 5
#setenv DA_BACK_ERRORS /ocotillo1/dmbarker/data/con200/noobs/gen_be/NMC.bin_type1/gen_be.NMC.dat

#########################################################################
# [3] Call run_wrf.csh script:
#########################################################################

./run_wrf.csh

exit(0)

