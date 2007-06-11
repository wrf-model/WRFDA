#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_ungrib_afwa.ksh
#
# Purpose: Run WPS's ungrib in AFWA mode.

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

if test ! -d $REG_DIR; then mkdir $REG_DIR; fi 
if test ! -d $RUN_DIR; then mkdir -p $RUN_DIR; fi 

export DATE_SAVE=$DATE
export FCST_RANGE_SAVE=$FCST_RANGE
export WORK_DIR=$RUN_DIR/working.run_ungrib_afwa
mkdir -p $WORK_DIR; cd $WORK_DIR
ln -fs $WPS_DIR/ungrib.exe .

#-----------------------------------------------------------------------
# [1] Run ungrib for AGRMET:
#-----------------------------------------------------------------------

export FG_TYPE=agrmet
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/*

export FCST_RANGE=0
${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE
${RUN_CMD} ./ungrib.exe

RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi
mv FILE:* AGRMET
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [2] Run ungrib for NAVYSST:
#-----------------------------------------------------------------------

export FG_TYPE=navysst
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
$WPS_DIR/link_grib.csh $DAT_DIR/$FG_TYPE/$DATE/navyssts

# Temporarily change date to that in SST file:
export SST_DATE=`$WPS_DIR/util/g1print.exe ${DAT_DIR}/$FG_TYPE/${DATE}/navyssts | grep -ni WTMP | cut -c 34-46`
export YEAR=`echo $SST_DATE | cut -c1-4`
export MONTH=`echo $SST_DATE | cut -c6-7`
export DAY=`echo $SST_DATE | cut -c9-10`
export HOUR=`echo $SST_DATE | cut -c12-13`
export DATE=${YEAR}${MONTH}${DAY}${HOUR}
export FCST_RANGE=0
${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

${RUN_CMD} ./ungrib.exe

RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi
mv FILE:* NAVYSST
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [3] Run ungrib for GFS:
#-----------------------------------------------------------------------

export DATE=$DATE_SAVE # Restore true date.
export FCST_RANGE=$FCST_RANGE_SAVE
export FG_TYPE=gfs
ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable

#Link relevant GFS grib files:
mkdir tmp
export FCST_HOUR=0
export HOUR=`echo $DATE | cut -c 9-10`
while test $FCST_HOUR -le $FCST_RANGE; do
   if test $FCST_HOUR -lt 10; then export FCST_HOUR=0$FCST_HOUR; fi
   ln -sf $DAT_DIR/$FG_TYPE/$DATE/MT.avn_CY.${HOUR}_fh.00${FCST_HOUR}_tl.press_gr.0p5deg tmp/.
   export FCST_HOUR=`expr $FCST_HOUR + $LBC_FREQ`
done

$WPS_DIR/link_grib.csh tmp/*

#Create namelist:
export CONSTANTS1=./AGRMET
export CONSTANTS2=./NAVYSST

${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh
cp namelist.wps namelist.wps.$FG_TYPE

${RUN_CMD} ./ungrib.exe

RC=$?
if test $RC != 0; then
   echo ungrib failed with error $RC
   exit $RC
fi
rm Vtable GRIBFILE*

#-----------------------------------------------------------------------
# [4] Run metgrid:
#-----------------------------------------------------------------------

ln -fs $WPS_DIR/metgrid/METGRID.TBL.AFWA METGRID.TBL
ln -fs $WPS_DIR/metgrid.exe .
${RUN_CMD} ./metgrid.exe

RC=$? 
mv metgrid.log* $RUN_DIR
if test -f $RUN_DIR/metgrid.log.0000; then
   echo '<A HREF="metgrid.log.0000">metgrid.log.0000</a>'
else
   echo '<A HREF="metgrid.log">metgrid.log</a>'
fi

if test $RC != 0; then
   echo metgrid failed with error $RC
   exit $RC
fi

mv met_em.d${DOMAIN}* $RC_DIR/$DATE

if $CLEAN; then rm -rf $WORK_DIR; fi

exit 0

