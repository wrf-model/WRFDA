#!/bin/ksh
#-------------------------------------------------------------------
#  Script for running observation error tuning (Hollingsworh Method)
#        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
#-------------------------------------------------------------------

echo ""
echo "Running da_ominusb.ksh"
echo ""

export REL_DIR=${REL_DIR:-$HOME/code/trunk}
export WRFVAR_DIR=${WRFVAR_DIR:-$REL_DIR/wrfvar}

export DAT_DIR=${DAT_DIR:-$HOME/data}
export REGION=${REGION:-con200}
export REG_DIR=${REG_DIR:-$DAT_DIR/$REGION} 
export EXPT=${EXPT:-test}
export INPUT_DIR=${INPUT_DIR:-$REG_DIR/diagnostics}

export START_DATE=${START_DATE:-2003010100}
export END_DATE=${END_DATE:-2003010200}
export CYCLE_PERIOD=${CYCLE_PERIOD:-6}

export WORK_DIR=${WORK_DIR:-$PWD/ominusb}

echo "WRFVAR_DIR    = $WRFVAR_DIR"
echo "INPUT_DIR     = $INPUT_DIR"
echo "WORK_DIR      = $WORK_DIR"
echo "START_DATE    = $START_DATE"
echo "END_DATE      = $END_DATE"

rm -rf $WORK_DIR; mkdir -p $WORK_DIR; cd $WORK_DIR

ln -fs ${WRFVAR_DIR}/build/da_ominusb.exe .

for FILE1 in ${INPUT_DIR}/*.dat; do
   FILE2=`basename $FILE1`
   FILE2=${FILE2%%.dat}
   ln -fs $FILE1 fort.35

   ./da_ominusb.exe > ominusb_$FILE2.log 2>&1
   if test -f fort.30; then 
      mv fort.30     $FILE2.sigma_o_b
   fi
   mv ominusb.out $FILE2.out
done

rm fort.35

exit 0




for TYPE in synop buoy ships metar sondsfc; do
   for FILE in ${TYPE}_*_omb.dat; do
   for VAR in u v t q p; do
      export FILENAME=${TYPE}_${VAR}_omb.dat

      
      ln -fs ${INPUT_DIR}/${FILENAME}.dat fort.35

     ./da_ominusb.exe > ominusb_${FILENAME}.log 2>&1

     rm fort.35
     mv fort.30     $FILENAME.sigma_o_b
     mv ominusb.out $FILENAME.out
   done
done

for TYPE in sound bogus; do
   for VAR in u v t q; do
      export FILENAME=${TYPE}_${VAR}_omb.dat
      ln -fs ${INPUT_DIR}/${FILENAME}.dat fort.35

      ./da_ominusb.exe ominusb_${FILENAME}.log 2>&1

      rm fort.35
      mv fort.30     $FILENAME.sigma_o_b
     mv ominusb.out $FILENAME.out
   done
done

for TYPE in airst; do
   for VAR in t q; do
      export FILENAME=${TYPE}_${VAR}_omb.dat
      ln -fs ${INPUT_DIR}/${FILENAME}.dat fort.35

      ./da_ominusb.exe ominusb_${FILENAME}.log 2>&1

      rm fort.35
      mv fort.30     $FILENAME.sigma_o_b
      mv ominusb.out $FILENAME.out
   done
done

for TYPE in airep; do
   for VAR in u v t; do
      export FILENAME=${TYPE}_${VAR}_omb.dat
      ln -fs ${INPUT_DIR}/${FILENAME}.dat fort.35

      ./da_ominusb.exe > ominusb_${FILENAME}.log 2>&1

      rm fort.35
      mv fort.30     $FILENAME.sigma_o_b
      mv ominusb.out $FILENAME.out
   done
done

for TYPE in geoamv polaramv pilot profiler; do
   for VAR in u v; do
      export FILENAME=${TYPE}_${VAR}_omb.dat
      ln -fs ${INPUT_DIR}/${FILENAME}.dat fort.35
      ./da_ominusb.exe > ominusb_${FILENAME}.log 2>&1
      rm fort.35
      mv fort.30     $FILENAME.sigma_o_b
      mv ominusb.out $FILENAME.out
   done
done

for TYPE in gpsref; do
  for VAR in ref; do
      export FILENAME=${TYPE}_${VAR}_omb.dat
      ln -fs ${RUN_DIR}/${FILENAME}.dat fort.35

      ./da_ominusb.exe > ominusb_${FILENAME}.log 2>&1
      rm fort.35
      mv fort.30     $FILENAME.sigma_o_b
      mv ominusb.out $FILENAME.out
   done
done

echo ""
echo " da_ominusb.ksh completed"
echo ""
