#!/bin/sh

for i in ./*.hdf
do
	echo "--------------------------------------------------"
	echo $i
	echo "--------------------------------------------------"
	../hrepack -v -i $i -o ${i}.output.hdf -t "*:GZIP 1"
done
ls -l
