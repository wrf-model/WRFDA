#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_get_date_range.ksh
#
# Purpose: Set environment variables associated with date range.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set arguments:
#-----------------------------------------------------------------------

if test $# -lt 2; then
   echo "Usage: da_get_date_range.ksh <YYYYMMDDHH> <TIME_HOURS>"
   exit 1
fi

export START_DATE=$1
export TIME_HOURS=$2

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

#-----------------------------------------------------------------------
# [2] Set environment variables:
#-----------------------------------------------------------------------

export END_DATE=`$WRFVAR_DIR/build/advance_cymdh.exe $START_DATE $TIME_HOURS 2>/dev/null`

export START_YEAR=`echo $START_DATE | cut -c1-4`
export START_MONTH=`echo $START_DATE | cut -c5-6`
export START_DAY=`echo $START_DATE | cut -c7-8`
export START_HOUR=`echo $START_DATE | cut -c9-10`
export END_YEAR=`echo $END_DATE | cut -c1-4`
export END_MONTH=`echo $END_DATE | cut -c5-6`
export END_DAY=`echo $END_DATE | cut -c7-8`
export END_HOUR=`echo $END_DATE | cut -c9-10`

export NL_START_YEAR=`echo $START_DATE | cut -c1-4`
export NL_START_MONTH=`echo $START_DATE | cut -c5-6`
export NL_START_DAY=`echo $START_DATE | cut -c7-8`
export NL_START_HOUR=`echo $START_DATE | cut -c9-10`
export NL_START_MINUTE=00
export NL_START_SECOND=00

export NL_END_YEAR=`echo $END_DATE | cut -c1-4`
export NL_END_MONTH=`echo $END_DATE | cut -c5-6`
export NL_END_DAY=`echo $END_DATE | cut -c7-8`
export NL_END_HOUR=`echo $END_DATE | cut -c9-10`
export NL_END_MINUTE=00
export NL_END_SECOND=00
