#! /bin/csh

 echo ""
 echo "Running script convert_to_mm5v3.csh.csh"
 echo ""

 rm -f fort.*

#### Note: Any MMINPUT file, not need to be that for your case.
#ln -sf "MMINPUT template file"                        MMINPUT_template
 ln -sf /mmmtmp/guo/wrf_plot/MMINPUT_template          MMINPUT_template

#ln -sf "binary analysis increment file"               ANALYSIS_INCREMENT
 ln -sf /mmmtmp/guo/ref_3_18ZZ/ANALYSIS_INCREMENT      ANALYSIS_INCREMENT

 ./convert_to_mm5v3.exe ANALYSIS_INCREMENT >&! convert_to_mm5v3.print.out

 echo "convert_to_mm5v3.csh completed."
 echo "Using the file: MMINPUT_ANALYSIS_INCREMENT for plotting with MM5/GRAPH."

 exit (0)

