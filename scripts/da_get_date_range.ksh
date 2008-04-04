#!/bin/ksh
#-----------------------------------------------------------------------
# Script da_get_date_range.ksh
#
# Purpose: Set environment variables associated with date range.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# [1] Set environment variables:
#-----------------------------------------------------------------------

export DATE=${DATE:-2003010100}
export FCST_RANGE=${FCST_RANGE:-6}
export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

#-----------------------------------------------------------------------
# [2] Calculate environment variables:
#-----------------------------------------------------------------------

export START_YEAR=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f ccyy 2>/dev/null)
export START_MONTH=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f mm 2>/dev/null)
export START_DAY=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f dd 2>/dev/null)
export START_HOUR=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f hh 2>/dev/null)
export START_MINUTE=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f nn 2>/dev/null)
export START_SECOND=$($BUILD_DIR/da_advance_time.exe $DATE 00 -f ss 2>/dev/null)

export NL_START_YEAR="11*${START_YEAR}"
export NL_START_MONTH="11*${START_MONTH}"
export NL_START_DAY="11*${START_DAY}"
export NL_START_HOUR="11*${START_HOUR}"
export NL_START_MINUTE="11*${START_MINUTE}"
export NL_START_SECOND="11*${START_SECOND}"

export END_YEAR=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f ccyy 2>/dev/null)
export END_MONTH=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f mm 2>/dev/null)
export END_DAY=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f dd 2>/dev/null)
export END_HOUR=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f hh 2>/dev/null)
export END_MINUTE=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f nn 2>/dev/null)
export END_SECOND=$($BUILD_DIR/da_advance_time.exe $DATE ${FCST_RANGE}s -f ss 2>/dev/null)

export NL_END_YEAR="11*${END_YEAR}"
export NL_END_MONTH="11*${END_MONTH}"
export NL_END_DAY="11*${END_DAY}"
export NL_END_HOUR="11*${END_HOUR}"
export NL_END_MINUTE="11*${END_MINUTE}"
export NL_END_SECOND="11*${END_SECOND}"
