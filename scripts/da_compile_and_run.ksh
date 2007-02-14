#!/bin/ksh

export DAT_DIR=${DAT_DIR:-~/data}

export FULL=${FULL:-false}
export COMPILE=${COMPILE:-true}
export RUN=${RUN:-true}
export CLEAN=${CLEAN:-true}

# Need a cleaner mapping between compiler and configure options.
# Assuming option 2 is pgi mpi is a hack

export TYPE=${TYPE:-simple}
export REGIONS=${REGIONS:-con200}
export PROCS=${PROCS:-1}
export COMPILERS=${COMPILERS:-g95}
export TARGET=${TARGET:-wrfvar}

echo "TYPE      $TYPE"
echo "COMPILE   $COMPILE"
echo "FULL      $FULL"
echo "RUN       $RUN"
echo "CLEAN     $CLEAN"
echo "COMPILERS $COMPILERS"
echo "REGIONS   $REGIONS"
echo "PROCS     $PROCS"
echo "TARGET    $TARGET"

###########
# Compiling
###########

let COUNT=1

for COMPILER in $COMPILERS; do
   export ID=${COMPILER}_${MACHINE}_${TYPE}
   export WRFVAR_DIR=$HOME/$ID/wrfvar
   if $COMPILE; then
      OPTION=${OPTIONS[$COUNT]}
      echo "Compiling $WRFVAR_DIR $TARGET with option $OPTION"
      cd $WRFVAR_DIR
      . ./setup.ksh $COMPILER >/dev/null
      svn update #>/dev/null 2>&1
      svn status
      if $FULL; then ./clean -a >/dev/null 2>&1; fi
      echo $OPTION | ./configure $TARGET >configure.out 2>&1
      rm -f build/links
      ./compile $TARGET > compile.out 2>&1
      if $CLEAN; then ./clean > /dev/null 2>&1; fi
      ls -l build/wrfvar.exe
      let COUNT=$COUNT+1
   fi
   if $RUN; then
      for REGION in $REGIONS; do
         for NUM_PROCS in $PROCS; do
            export NUM_PROCS
            cd $DAT_DIR/$REGION
            . $WRFVAR_DIR/setup.ksh $COMPILER >/dev/null
            echo "Testing $WRFVAR_DIR on $REGION"
            ./test.ksh
         done
      done
   fi
done
