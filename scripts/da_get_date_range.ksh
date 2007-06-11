#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_get_date_range.ksh
#
# Purpose: Set environment variables associated with date range.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

#-----------------------------------------------------------------------
# [2] Calculate output environment variables:
#-----------------------------------------------------------------------

export NL_START_YEAR=`echo $DATE | cut -c1-4`
export NL_START_MONTH=`echo $DATE | cut -c5-6`
export NL_START_DAY=`echo $DATE | cut -c7-8`
export NL_START_HOUR=`echo $DATE | cut -c9-10`
export NL_START_MINUTE=00
export NL_START_SECOND=00

export END_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $FCST_RANGE 2>/dev/null`

export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
export NL_END_DAY=`echo $END_DATE | cut -c7-8`
export NL_END_HOUR=`echo $END_DATE | cut -c9-10`
export NL_END_MINUTE=00
export NL_END_SECOND=00

echo 0

