#! /bin/sh
# Tests for the hrepack  tool

HZIP=hrepack              # The tool name
HZIP_BIN=`pwd`/$HZIP      # The path of the tool binary

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
	MESG 0 "All hrepack tests passed"
    else
	MESG 0 "hrepack tests failed: $nerrors"
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
# `.out'.  The actual output is not removed if $HDF_NOCLEANUP has a
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

   if [ ! -f $expect ]; then
   # Create the expect file if it doesn't yet exist.
      echo " CREATED"
      cp $actual $expect
   elif $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.txt) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
     if test -z "$HDF_NOCLEANUP"; then
     rm -f $actual $actual_err
     fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                            ###
##############################################################################
##############################################################################


# Print a beginning banner
MESG 0 "Running hrepack tests"

# just run the program test_hrepack; this has several runs with 
# different compression and chunking options
./test_hrepack

# save the exit code from the run
exit_code=$?
nerrors=$exit_code

# End of test, return exit code
FINISH

