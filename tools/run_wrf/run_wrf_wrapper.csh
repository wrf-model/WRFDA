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

#########################################################################
#[1] Decide what stages to run:
#########################################################################

setenv RUN_RESTORE_DATA .TRUE.
setenv RUN_SI .TRUE.
setenv RUN_REAL .TRUE.
#setenv RUN_OBSPROC .TRUE.
#setenv RUN_VAR .TRUE.
#setenv RUN_WRF_BC .TRUE.
setenv RUN_WRF .TRUE.
#setenv CYCLING .TRUE.

#########################################################################
# [2] Override default environment variables here:
#########################################################################

#Disks, directories:
setenv SRC_DIR /ocotillo/users/dmbarker/code_development/WRF_V2.1.2 
#setenv WRF_BC_DIR /ocotillo1/caya/WRF_BC/src
#setenv OBS_DIR /ocotillo1/caya/data/conus200 # Alain's "unclean" obs!
#setenv OBS_DIR /ocotillo1/dmbarker/data/con200/get_obs # Cleaned obs.

#Job details:
setenv REGION con200
setenv EXPT noobs
setenv NUM_PROCS 8 
setenv FINAL_DATE 2003010100
setenv FCST_RANGE_OUT 24

#WRF SI:
setenv XDIM 45
setenv YDIM 45
setenv DELTA_X 200000
setenv DELTA_Y 200000
#setenv RUN_GRID_GEN .FALSE. # Uncomment after you've run SI once for this domain!
#set VERTICAL_LEVELS - do this directly in run_wrfsi.csh.

#WRF:
setenv DA_TIME_STEP_SOUND 6

#WRF-Var:
#setenv DA_CV_OPTIONS 5
#setenv DA_BACK_ERRORS /ocotillo1/dmbarker/data/con200/noobs/gen_be/NMC.bin_type1/gen_be.NMC.dat

#########################################################################
# [3] Call run_wrf.csh script:
#########################################################################

./run_wrf.csh

exit(0)

