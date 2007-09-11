#!/bin/ksh
# da_update_html.ksh

# Assuming starting from wrfvar/doc directory
cd ..
svn update
./clean -a
. ./setup.ksh gnu
echo 3 | ./configure # optimised code
cd build
make -r setup
ln -fs inc/* .
# This file causes da_generate_html.pl to stall
rm -rf RELHUM.inc
cd ../doc
mkdir -p html
./da_generate_html.pl ../build html
./da_list_refs.ksh html
