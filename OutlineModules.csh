#!/bin/csh
# Recursively search the *.F files of the current working directory
#   and create a textual outline of module and subroutine signatures.
#   8/6/09 bp
# todo: for more structured code, would be useful to include type definitions
#

set fName = ModulesOutline.txt   # output text file
#rm $fName 
echo "Create Module Outline: " $fName " for  .F files in: " `pwd`
echo "Create Module Outline: " $fName " for  .F files in: " `pwd` >> $fName

set wrfpDirs = "dyn_em dyn_exp external frame inc main phys Registry run share"

echo "go"
foreach d ( `echo $wrfpDirs` )
   foreach f ( `find $d -name "*.F" -print` )
      echo $f
      echo "Filename:::: " $f >> $fName
      echo "==================================================" >> $fName
      # Extract module signatures; exclude module procedures and use references, align in 1st column
      grep -i module $f | sed -e "/^ *\!/d" | sed -e "/[eE][nN][dD]/d" | \
         sed -e "/[uU][sS][eE]/d" | sed -e "/[pP][rR][oO][cC][eE][dD][uU][rR][eE]/d" | \
         sed -e "/[cC][aA][lL]/d" | sed -e "/[wW][rR][iI][tT][eE]/d" | \
         sed -e "/\'/d" | sed -e "/:/d" | sed -e "s/^ *//g" >> $fName
      # Extract global subroutine signatures; align on 3rd column
      grep -i subroutine $f | sed -e "/^ *\!/d" | sed -e "/[eE][nN][dD]/d"  | sed -e "/[cC][aA][lL]/d" | \
         sed -e "s/^ */   /" >> $fName
      echo " " >> $fName
   end
end
