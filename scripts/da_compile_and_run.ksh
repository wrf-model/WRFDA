#!/bin/ksh

export FULL=${FULL:-false}
export COMPILE=${COMPILE:-true}
export RUN=${RUN:-true}

# Need a cleaner mapping between compiler and configure options.
# Assuming option 2 is pgi mpi is a hack

export ID=${ID:-trunk}
export REGIONS=${REGIONS:-con200}
export TARGETS=${TARGETS:-wrfvar}
export CONFIGS=${CONFIGS:-rsl}
export PROCS=${PROCS:-1}
export COMPILERS=${COMPILERS:-g95}

echo "ID        $ID"
echo "COMPILE   $COMPILE"
echo "FULL      $FULL"
echo "TARGETS   $TARGETS"
echo "COMPILERS $COMPILERS"
echo "RUN       $RUN"
echo "CLEAN     $CLEAN"
echo "REGIONS   $REGIONS"
echo "PROCS     $PROCS"

###########
# Compiling
###########

let COUNT=1

for CONFIG in $CONFIGS; do

  for COMPILER in $COMPILERS; do
    if $COMPILE; then
      OPTION=${OPTIONS[$COUNT]}

      . ~/setup_$COMPILER

      export BUILD=${ID}_${CONFIG}_${COMPILER}_${MACHINE}

      for TARGET in $TARGETS; do
        echo "Compiling ${BUILD}/$TARGET"
        cd ~bray/${BUILD}/$TARGET
        . ./setup.ksh $COMPILER >/dev/null
        svn update #>/dev/null 2>&1
        svn status
	if $FULL; then ./clean_new -a >/dev/null 2>&1; fi
#        echo $OPTION | ./configure_new $TARGET >/dev/null 2>&1
        echo $OPTION | ./configure_new $TARGET
        rm -f build/links
        ./compile_new $TARGET > compile.out 2>&1
        ls -l build/wrfvar.exe
      done
      let COUNT=$COUNT+1
    fi
    if $RUN; then
      for REGION in $REGIONS; do
        for NUM_PROCS in $PROCS; do
          export NUM_PROCS
          if test $CONFIG = ser && test $NUM_PROCS != 1; then
            echo "Skipping parallel runs of serial code"
          else
            cd ~bray/data/$REGION
            . $HOME/$BUILD/wrfvar/setup.ksh $COMPILER >/dev/null
            echo "Testing $BUILD $TARGET on $REGION"
            if test $TARGET = be; then
              ./gen_be.ksh
            else
              ./test.ksh
            fi
          fi
        done
      done
    fi
  done
done

#ls -lrt ~bray/*/*/build/*.exe
#ls -lrt ~bray/data/*/*/cost_fn




