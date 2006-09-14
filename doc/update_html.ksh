#!/bin/ksh
# Assuming starting from wrfvar/doc directory
cd ..
svn update
./clean_new -a
echo 1 | ./configure_new wrfvar
cd build
make -r setup
# This file causes generate_html.pl to stall
rm -rf RELHUM.inc
cd ..
cd doc
mkdir -p html
./generate_html.pl ../build html
