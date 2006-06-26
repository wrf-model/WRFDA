#!/bin/csh
 
################################################################################
#
#  CLS applications
#
################################################################################
 
# create the directory structure
 
echo "creating $SUBSYSTOP directory structure ..."
 
mkdir -p $SUBSYSTOP/install/data
 
# copy the data directories into the install data directory
 
echo "copying data into $SUBSYSTOP/install/data directory ..."
 
cp -r $SUBSYSTOP/DESKT/data/* $SUBSYSTOP/install/data
 

