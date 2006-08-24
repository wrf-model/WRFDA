#!/bin/ksh

#  Purpose: Trace script usage

typeset NAME=$1
typeset DIR=$2

if test ! -z $DIR; then
   shift 2
   typeset REST="$@"
   if test -d $DIR; then
      echo "<A HREF=\"$DIR/index.html\">$NAME</a> $(date) $REST"
   else
      echo "$NAME $(date)"
   fi
else
   shift 1
   typeset REST="$@"
   echo "$INIT $NAME $(date)" $REST
fi
