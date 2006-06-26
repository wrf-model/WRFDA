#! /bin/sh
# Tests for the hdiff tool

HDIFF=hdiff               # The tool name
HDIFF_BIN=`pwd`/$HDIFF    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d testfiles || mkdir testfiles

# Print message with formats according to message level ($1)
MESG() {
  level=$1
  shift
  case $level in
    0)
      echo '============================='
      echo $*
      echo '============================='
      ;;
    3)
      echo '-----------------------------'
      echo $*
      echo '-----------------------------'
      ;;
    6)
      echo "*** $* ***"
      ;;
    *)
      echo "MESG(): Unknown level ($level)"
      exit 1
      ;;
  esac
}


# Report the result and exit
FINISH()
{
    if [ $nerrors -eq 0 ]
    then
	MESG 0 "All hdiff tests passed"
    else
	MESG 0 "hdiff tests failed: $nerrors"
    fi
    exit $nerrors
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
TOOLTEST() {
   expect="$srcdir/testfiles/$1"
   actual="testfiles/`basename $1 .txt`.out"
   actual_err="testfiles/`basename $1 .txt`.err"
   shift

   # Run test.
   # Tflops interprets "$@" as "" when no parameter is given (e.g., the
   # case of missing file name).  Changed it to use $@ till Tflops fixes it.
   TESTING $HDIFF $@
   (
      echo "#############################"
      echo "Expected output for '$HDIFF $@'" 
      echo "#############################"
      cd $srcdir/testfiles
      if [ "`uname -s`" = "TFLOPS O/S" ]; then
        $RUNSERIAL $HDIFF_BIN $@
      else
        $RUNSERIAL $HDIFF_BIN "$@"
      fi
   ) >$actual 2>$actual_err
   cat $actual_err >> $actual

 
   # Used only to create the output file; uncomment to create
   #   if [ ! -f $expect ]; then
   # Create the expected file if it doesn't yet exist.
   #   echo " CREATED"
   #   cp $actual $expect
   #   elif
   if $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.txt) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
     if test -z "$HDF4_NOCLEANUP"; then
     rm -f $actual $actual_err
     fi
}




RUN() {

# help message
TOOLTEST hdiff_01.txt 

# Compare global attributes only
TOOLTEST hdiff_02.txt -g hdifftst1.hdf hdifftst2.hdf

# Compare SD local attributes only
TOOLTEST hdiff_03.txt -s hdifftst1.hdf hdifftst2.hdf

# Compare SD data only
TOOLTEST hdiff_04.txt -d hdifftst1.hdf hdifftst2.hdf

# Compare Vdata data only
TOOLTEST hdiff_05.txt -D hdifftst1.hdf hdifftst2.hdf

# Print statistics
TOOLTEST hdiff_06.txt -d -S hdifftst1.hdf hdifftst2.hdf

# Compare SD data on variable(s)
TOOLTEST hdiff_07.txt -d -v dset1 hdifftst1.hdf hdifftst2.hdf

# Compare vdata on variable(s) 
TOOLTEST hdiff_08.txt -D -u vdata1 hdifftst1.hdf hdifftst2.hdf

# Print difference up to count number
TOOLTEST hdiff_09.txt -d -e 2 hdifftst1.hdf hdifftst2.hdf

# Print difference when it is greater than limit
TOOLTEST hdiff_10.txt -d -t 2 hdifftst1.hdf hdifftst2.hdf

# no options
TOOLTEST hdiff_11.txt hdifftst1.hdf hdifftst2.hdf

}


##############################################################################
###			  T H E   T E S T S                                            ###
##############################################################################

# Print a beginning banner
MESG 0 "Running hdiff tests"

# compare output
RUN

# End of test, return exit code
FINISH
