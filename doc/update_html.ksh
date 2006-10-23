#!/bin/ksh
# Assuming starting from wrfvar/doc directory
cd ..
svn update
./clean_new -a
echo 1 | ./configure_new wrfvar
cd build
make -r setup
ln -fs inc/* .
# This file causes generate_html.pl to stall
rm -rf RELHUM.inc
cd ../doc
mkdir -p html
./generate_html.pl ../build html
./list_refs.ksh html
