#!/bin/ksh
#-----------------------------------------------------------------------
# Script: da_run_wps.ksh
#
# Purpose: Run WPS

#-----------------------------------------------------------------------
# [1] Set defaults for required environment variables:
#-----------------------------------------------------------------------

export REL_DIR=${REL_DIR:-$HOME/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

. ${WRFVAR_DIR}/scripts/da_set_defaults.ksh

. ${WRFVAR_DIR}/scripts/da_get_date_range.ksh
export END_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe $DATE $FCST_RANGE 2>/dev/null`

#-----------------------------------------------------------------------
# [2] Setup run:
#-----------------------------------------------------------------------

if test ! -d $REG_DIR; then mkdir $REG_DIR; fi 
if test ! -d $RUN_DIR; then mkdir -p $RUN_DIR; fi 
if test ! -d $RC_DIR/$DATE; then mkdir -p $RC_DIR/$DATE; fi 

echo "<HTML><HEAD><TITLE>$EXPT wps</TITLE></HEAD><BODY><H1>$EXPT wps</H1><PRE>"

date

echo 'WPS_DIR       <A HREF="'$WPS_DIR'"</a>'$WPS_DIR'</a>'
echo "DATE          $DATE"
echo "END_DATE      $END_DATE"
echo 'WPS_INPUT_DIR <A HREF="file:'$WPS_INPUT_DIR'"</a>'$WPS_INPUT_DIR'</a>'
echo 'RUN_DIR       <A HREF="file:'$RUN_DIR'"</a>'$RUN_DIR'</a>'
echo 'RC_DIR        <A HREF="file:'$RC_DIR'"</a>'$RC_DIR'</a>'

#-----------------------------------------------------------------------
# [3.0] Run WPS:
#-----------------------------------------------------------------------

   if $DUMMY; then
      echo "Dummy wps"
      LOCAL_DATE=$DATE
      while test $LOCAL_DATE -le $END_DATE; do
         export L_YEAR=`echo $LOCAL_DATE | cut -c1-4`
         export L_MONTH=`echo $LOCAL_DATE | cut -c5-6`
         export L_DAY=`echo $LOCAL_DATE | cut -c7-8`
         export L_HOUR=`echo $LOCAL_DATE | cut -c9-10`
         echo Dummy wps > met_em.d${DOMAIN}.${L_YEAR}-${L_MONTH}-${L_DAY}_${L_HOUR}:00:00.nc
         LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe ${LOCAL_DATE} 1 2>/dev/null`
      done
   else
      if $RUN_GEOGRID; then # Run geogrid:
         export WORK_DIR=$RUN_DIR/working.geogrid
         rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR
         ${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh

	 ln -fs $WPS_DIR/geogrid.exe .
	 ${RUN_CMD} ./geogrid.exe

	 RC=$?
	 mv geogrid.log* $RUN_DIR
	 if test -f $RUN_DIR/geogrid.log.0000; then
            echo '<A HREF="geogrid.log.0000">geogrid.log.0000</a>'
	 else
            echo '<A HREF="geogrid.log">geogrid.log</a>'
	 fi

	 if test $RC != 0; then
            echo geogrid failed with error $RC
            exit $RC
	 fi
      fi

      # Run ungrib:
      export WORK_DIR=$RUN_DIR/working
      rm -rf $WORK_DIR; mkdir -p $WORK_DIR

      if $RUN_UNGRIB_AFWA; then # Uses AGRMET, NAVYSST, and 1/2 degree GFS GRIB data.
         export WORK_DIR=$RUN_DIR/working.run_ungrib_afwa
         rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

         ${WRFVAR_DIR}/scripts/da_run_ungrib_afwa.ksh > run_ungrib_afwa.log 2>&1
         RC=$?
         if test $RC != 0; then
            echo `date` "${ERR}Failed with error $RC$END"
            exit 1
         fi
      else
         cd $WORK_DIR
         ${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh
         cp namelist.wps ${RUN_DIR}/namelist.wps.ungrib

         ln -fs $WPS_DIR/ungrib/Variable_Tables/Vtable.$FG_TYPE Vtable
         LOCAL_DATE=$DATE
         FILES=''
         while test $LOCAL_DATE -le $END_DATE; do
            FILES="$FILES $WPS_INPUT_DIR/$LOCAL_DATE/*"
            LOCAL_DATE=`$WRFVAR_DIR/build/da_advance_cymdh.exe ${LOCAL_DATE} ${LBC_FREQ} 3>/dev/null`
         done
         $WPS_DIR/link_grib.csh $FILES

         ln -fs $WPS_DIR/ungrib.exe .

         ./ungrib.exe > ungrib.log 2>&1

         RC=$?
         mv ungrib.log $RUN_DIR
         echo '<A HREF="ungrib.log">ungrib.log</a>'

         if test $RC != 0; then
            echo ungrib failed with error $RC
            exit $RC
         fi
      fi

      # Run metgrid:
      cd $WORK_DIR
      ${WRFVAR_DIR}/scripts/da_create_wps_namelist.ksh
      cp namelist.wps ${RUN_DIR}/namelist.wps.metgrid

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
      
   fi

cd $OLDPWD

if $CLEAN; then rm -rf $RUN_DIR/working.*; fi

date

echo "</BODY></HTML>"

exit 0

