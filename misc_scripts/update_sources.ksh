#!/bin/ksh
#
# Update both serial and parallel source. 
#
# Usage:  update_sources.ksh

set -x

cd /ptmp/hender/wrfplus_WORK
rsync -av loquat.mmm.ucar.edu:/loquat2/hender/Tasks/4DVAR_Optimization/wrfplus_WORK/ .

cd /ptmp/hender/wrfplus_WORK_parallel
rsync -av loquat.mmm.ucar.edu:/loquat2/hender/Tasks/4DVAR_Optimization/wrfplus_WORK/ .

