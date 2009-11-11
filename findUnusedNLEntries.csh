#!/bin/csh
#
# Search all .F source files for each namelist value in the run/namelist.input file 
# to determine if the given value is used.
# 
# The contents of nlValues.txt must be updated if values are added to namelist.input
#
# Outputs:
# - There are N namelistSummaries/*.txt files where the name of the file is the namelist
#	entry value.  These files can be searched to determine where namelist values are  
#	referenced.
# - There are N namelistSummaries/*.grep files where the name of the file is the namelist
#	entry value.  These files contain a count of the number of times (potentially 0)
#	that the namelist value is used in the entire code base
# - The file namelistSummaries/unusedEntries.txt enumerates the values not used
#
# Last Updated: 11/11/09 bp

set nlDir = namelistSummaries 
mkdir $nlDir

foreach nl ( `cat nlValues.txt` )
	set oFile = $nlDir/$nl.txt
	echo $oFile 
	if( -e $oFile ) rm $oFile
	find . -name "*.F" -print -exec grep $nl {} \; > $oFile

	set grepFile = $nlDir/$nl.grep
	if( -e $grepFile ) rm $grepFile
	grep -i $nl $oFile | sed -e "/^\!/d" | grep -i nl_get > $grepFile 
end

# Count uses and determine unused namelist values
set unusedFile = $nlDir/unusedEntries.txt
wc -l $nlDir/*.grep | grep " 0" > $unusedFile
echo "Unused namelist entries enumerated in : " $unusedFile
set nUnused = `wc -l $unusedFile`
echo "The number of unused namelist.input entries = " $nUnused



