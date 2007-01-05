#!/bin/ksh

export FULL=${FULL:-false}
export COMPILE=${COMPILE:-true}
export RUN=${RUN:-true}

# Need a cleaner mapping between compiler and configure options.
# Assuming option 2 is pgi mpi is a hack

export TYPE=${TYPE:-trunk}
export REGIONS=${REGIONS:-con200}
export PROCS=${PROCS:-1}
export COMPILERS=${COMPILERS:-g95}

echo "TYPE      $TYPE"
echo "COMPILE   $COMPILE"
echo "FULL      $FULL"
echo "RUN       $RUN"
echo "CLEAN     $CLEAN"
echo "COMPILERS $COMPILERS"
echo "REGIONS   $REGIONS"
echo "PROCS     $PROCS"

###########
# Compiling
###########

let COUNT=1

export TARGET=wrfvar

for COMPILER in $COMPILERS; do
   export ID=${TYPE}_${COMPILER}_${MACHINE}
   if $COMPILE; then
      OPTION=${OPTIONS[$COUNT]}

      echo "Compiling ${ID}/$TARGET"
      cd $HOME/${ID}/$TARGET
      . ./setup.ksh $COMPILER >/dev/null
      svn update #>/dev/null 2>&1
      svn status
      if $FULL; then ./clean_new -a >/dev/null 2>&1; fi
      echo $OPTION | ./configure_new $TARGET >/dev/null 2>&1
      rm -f build/links
      ./compile_new $TARGET > compile.out 2>&1
      ls -l build/$TARGET.exe
      let COUNT=$COUNT+1
   fi
   if $RUN; then
      for REGION in $REGIONS; do
         for NUM_PROCS in $PROCS; do
            export NUM_PROCS
            cd $DAT_DIR/$REGION
            . $HOME/$ID/wrfvar/setup.ksh $COMPILER >/dev/null
            echo "Testing $ID $TARGET on $REGION"
            ./test.ksh
         done
      done
   fi
done
