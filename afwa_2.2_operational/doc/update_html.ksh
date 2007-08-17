#!/bin/ksh
# Assuming starting from wrfvar/doc directory
cd ..
svn update
./clean -a
. ./setup.ksh g95
echo 4 | ./configure
cd build
make -r setup
ln -fs inc/* .
# This file causes generate_html.pl to stall
rm -rf RELHUM.inc
cd ../doc
mkdir -p html
./generate_html.pl ../build html
./list_refs.ksh html
