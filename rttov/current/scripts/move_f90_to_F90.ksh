#!/bin/sh
# 
# RTTOV delivered code file names are *.F90 and
# HP compiler does not allow this kind of extension.
# This script will move all F90 files to f90 and
# change the dependencies inside the makefiles
#
# P. Brunel March 11th, 2004
#
cd ../src
\ls -1 *f90 |tr "." " "|
while read nom ext
do 
   ancien=${nom}.f90
   nouveau=${nom}.F90
   echo "mv $ancien $nouveau"
   mv $ancien $nouveau
done

for nomfic in Makefile_lib Makefile_main
do
  echo ${nomfic}
  ed ${nomfic} << EOF > ed.log
g/f90/s//F90/g
w
EOF
done

exit

