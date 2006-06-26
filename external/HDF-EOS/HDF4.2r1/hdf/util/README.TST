All the input data files needed to test the utilities
are in the subdirectory testfiles/. 


Instructions on testing the HDF utility programs:


hdf24to8 -- converts 24-bit raster images to hdf 8-bit images

   Copy head.r24.Z from the testfiles/ directory to the util/ directory.
   
   Type: uncompress head.r24.Z

   Execute the following in the util/ directory: 

        cp testfiles/head.r24.Z .
        uncompress head.r24.Z
        ./hdf24to8 head.r24 head8.hdf

   View head8.hdf using any visualization tool available (mosaic, collage, etc).

   Delete head.r24 and head8.hdf when you are done.

        rm head.r24 head8.hdf


hdfed -- hdf file editor

   Copy the file storm110.hdf from the testfiles/ directory to the 
   util/ directory.

   Execute the following in the util/ directory:
       
        cp testfiles/storm110.hdf .
        ./hdfed storm110.hdf

        Running interactively, type the following commands:

                info -all
                prev tag = 300
                info -long
                dump -short

        The latter two commands should result in the following responses:

         (6)    Image Dimensions              : (Tag 300)
                Ref: 110, Offset: 3459, Length: 20 (bytes)
       0:          0         57          0         57        106        110
      12:          1          0          0          0


        Type help and experiment.  Most of the information can be verified
        with hdfls.  Be sure to type 'close' then 'quit' when you are finished
        and delete storm110.hdf when you are done.

            rm storm110.hdf

        Copy ntcheck.hdf from the testfiles/ directory to the util/ directory.
  
        Execute the following in the util/ directory:

            cp testfiles/ntcheck.hdf .
            ./hdfed ntcheck.hdf

        ntcheck.hdf will be used as an input file to test the dump function. 

        The command:

          dump -help

        displays the list of formats supported by hdfed. 

        Commands:

          prev tag=<tag>  ref=<ref>

        and

          next tag=<rag> ref=<ref> 

        move you back and forth among the objects.
        
          info -all 
                
        gives the tag and reference numbers for each object.

	  
        Look at the data in various objects and verify the data type.
        For example:

          next tag = 702  ref = 2      
           
          You will be past this data object and will receive the message
          "Reached end of file. Not moved."  Repeat the command using prev 
          instead of next.
                         
          dump -float

          The first 5 lines of output should read as follows:

             0:    0.000000e+00   1.000000e+00   2.000000e+00   3.000000e+00
            16:    4.000000e+00   5.000000e+00   6.000000e+00   7.000000e+00
            32:    8.000000e+00   9.000000e+00   4.000000e+01   4.100000e+01
            48:    4.200000e+01   4.300000e+01   4.400000e+01   4.500000e+01
            64:    4.600000e+01   4.700000e+01   4.800000e+01   4.900000e+01
         	

	      next tag = 702  ref = 6     

          dump -short

          The first 5 lines of output should read as follows:
  
           0:          0          1          2          3          4          5
          12:          6          7          8          9       6000       6001
          24:       6002       6003       6004       6005       6006       6007
          36:       6008       6009      12000      12001      12002      12003
          48:      12004      12005      12006      12007      12008      12009

 
        The following is a cross reference of ref number and data type if you
        want to experiment: 

           
         ref		number type


	  2		DFNT_FLOAT32

  	  3		DFNT_INT8

	  4		DFNT_UINT8

	  5		DFNT_INT16

	  6		DFNT_UINT16

	  7		DFNT_INT32

	  8		DFNT_UINT32

        Type 'close' and 'quit' when you are finished.
        Delete ntcheck.hdf.

            rm ntcheck.hdf

ristosds -- converts a series raster image hdf files into a
                  single 3D sds hdf file.

   Copy the three HDF files storm110.hdf, storm120.hdf, and
   storm130.hdf from the testfiles/ directory.

   Execute the following in the util/ directory:
 
    cp testfiles/storm110.hdf testfiles/storm120.hdf testfiles/storm130.hdf .
    ./ristosds storm*.hdf -o storm.hdf
 
   Compare storm110.hdf with storm.hdf using the following commands:
   
        hdfed storm.hdf
        prev tag = 702
	info -long
    (*)	dump -length 20 -byte
	close
	open storm110.hdf
	prev tag = 302
        info -long
   (**)	dump -length 20 -byte
   	close
        quit

   * In storm.hdf tag 702's element should be 9747 bytes.

  ** In storm110.hdf tag 302's element should be 1/3 of
     9747, which is 3249.  (It is a 57x57 image.)

     Compare the first few numbers in storm110's image
     with the first few numbers in storm.hdf's SDS.  
     They should be the same. 

     Remove storm*.hdf from the util/ directory before continuing.

         rm storm*.hdf

hdfpack --  compacts an hdf file

   Copy the file test.hdf from the testfiles/ directory.

   Execute the following in the util/ directory:

        cp testfiles/test.hdf .
        ./hdfpack test.hdf test.pck
        ./hdfpack -b test.hdf test.blk

   Use hdfls to get a listing of test.hdf and test.pck.  The only
   difference between the 2 listings should be that test.pck
   shouldn't have any special elements and it also shouldn't 
   have any "Linked Block Indicators."

          ./hdfls test.hdf
          ./hdfls test.pck

   The file sizes should be as follows:

       test.hdf - 11795 
       test.pck - 6747 
       test.blk - 7599
  
   Depending on the platform, the file sizes may be one byte off 
   for test.pck and test.blk.
  
   Remove test.cdf, test.blk, and test.pck.

       rm test.hdf test.blk test.pck

hdftopal/paltohdf  -- converts between a raw palette and an hdf

   Copy the file palette.raw from the testfiles/ directory.

   Execute the following in the util/ directory:
      
        cp testfiles/palette.raw .
        ./paltohdf palette.raw palette.hdf
        ./hdftopal palette.hdf palette.raw.new

   Use hdfls with the '-l' option to examine palette.hdf.
   It should have an 'Image Palette-8' and an 'Image Palette,'
   both with length 768 bytes.  They should also have the same
   reference number.

   Use the Unix utility 'cmp' or something similar to do a byte-for-byte
   comparison of palette.raw and palette.raw.new.  They should be
   identical.

      cmp palette.raw palette.raw.new

   Remove palette.*.

      rm palette.*
         
	
r8tohdf/hdftor8 -- converts between 8-bit raster images and hdf files

   Copy the files storm*.raw and palette.raw from the testfiles/ directory.

   Execute the following in the util/ directory:

        cp testfiles/storm*.raw .
        ./r8tohdf 57 57 storm.hdf storm*.raw
        ./r8tohdf 57 57 storm.hdf -p palette.raw -i storm110.raw
        ./hdftor8 storm.hdf

   Use hdfls with the '-l' option to examine storm.hdf.  It should
   contain five raster image sets, one of which will be compressed
   under IMCOMP compression.  (If you do not put the '-p' in the
   second r8tohdf command above, you should get an error message.)
   The non-compressed raster images should be the same length as
   the raw raster files.  The compressed will be about 25% of that
   size.

   Use the Unix utility 'cmp' or something similar to do byte-for-byte
   comparisons on the raw raster files produced by hdftor8:
             
         cmp img001-057.057  storm110.raw   
         cmp img002-057.057  storm120.raw
         cmp img003-057.057  storm130.raw
         cmp img004-057.057  storm140.raw

   There should be one more img* file than you had at the start.  One of the
   img files may not compare exactly with any one of the raw rasters, and the 
   rest will compare with one of the other raw rasters.  There is no guarantee
   about the order of the produced raw rasters, but it is likely they will be 
   produced in the order in which they went into the file, which would be
   increasing numerical order, with the compressed image last.

   Remove storm* and img* when you are done.

         rm storm* img*

hdfcomp -- re-compress 8-bit raster hdf file

   Copy the files storm*.hdf from the testfiles/ directory.

   Execute:

        cp testfiles/storm*.hdf .
        ./hdfcomp allstorms.hdf storm*.hdf
        ./hdfcomp allcomp.hdf -c storm*.hdf

   Use hdfls with the '-l' option to examine the two HDF files.  The first,
   allstorms.hdf, should simply hold the raster together in one file,
   with no compression.  You can use hdfls to check the original files.
   The second file, allcomp.hdf, should hold all the rasters in a
   compress format.  Run-Length Encoding (RLE) compression will result
   in modest savings - about 10% to 15% for these files.

   Remove allstorms.hdf and allcomp.hdf.

        rm storm*.hdf all*.hdf

jpeg2hdf/hdf2jpeg

   Copy the file jpeg_img.jpg from the testfiles/ directory.

   Execute:

        cp testfiles/jpeg_img.jpg .
        ./jpeg2hdf jpeg_img.jpg jpeg.hdf
        ./hdf2jpeg jpeg.hdf jpeg2.jpg

   Use hdfls with the '-l' option to examine the HDF file.  It should
   contain one raster image set, which will be compressed with JPEG
   compression.  The JPEG compressed image will be 2922 bytes in size.

   Use the Unix utility 'cmp' or something similar to do byte-for-byte
   comparisons on the produced raw raster files by hdf2jpeg.  The
   initial jpeg_img.jpg file should be an exact match for the new
   jpeg2.jpg file.

        cmp jpeg_img.jpg jpeg2.jpg

   Remove jpeg.hdf, jpeg_img.jpg, and jpeg2.jpg.

        rm jpeg.hdf jpeg_img.jpg jpeg2.jpg

fp2hdf -- converts floating point 2D/3D datasets into hdf SDS or RIS.

   To test this utility you must first create the ASCII and binary test 
   files with the fptest program.  Then run fp2hdf on the test files that 
   get created, and analyze the output.  Following are the steps to 
   do this:

   1. Run fptest to create 2D/3D ASCII/binary test files:  

        ./fptest

                   FILE    TYPE    DIMENSIONS 
                   ----    ----    ----------
                   ctxtr2  TEXT    3x4
                   ctxtr3  TEXT    5x3x4
                   cb32r2  FP32    3x4
                   cb32r3  FP32    5x3x4
                   cb64r2  FP64    3x4
                   cb64r3  FP64    5x3x4

     Following are the values of the dimension scales and arrays that 
     get created:

     row scale values start at 11 and increment by 1 => 11, 12, 13
     column scale values start at 21 and increment by 2 => 21, 23, 25, 27
     plane scale values start at 51 and increment by 5 => 51, 56, 61, 66, 71

     data element value = row scale value + column scale value [+ plane 
                          scale value, if rank=3]

     For an array of [3][4], data values are:

      3.200000E+01  3.400000E+01  3.600000E+01  3.800000E+01
      3.300000E+01  3.500000E+01  3.700000E+01  3.900000E+01
      3.400000E+01  3.600000E+01  3.800000E+01  4.000000E+01
 
     For array of [5][3][4], data values are:
 
      8.300000E+01  8.500000E+01  8.700000E+01  8.900000E+01
      8.400000E+01  8.600000E+01  8.800000E+01  9.000000E+01
      8.500000E+01  8.700000E+01  8.900000E+01  9.100000E+01
 
      8.800000E+01  9.000000E+01  9.200000E+01  9.400000E+01
      8.900000E+01  9.100000E+01  9.300000E+01  9.500000E+01
      9.000000E+01  9.200000E+01  9.400000E+01  9.600000E+01

      9.300000E+01  9.500000E+01  9.700000E+01  9.900000E+01
      9.400000E+01  9.600000E+01  9.800000E+01  1.000000E+02
      9.500000E+01  9.700000E+01  9.900000E+01  1.010000E+02

      9.800000E+01  1.000000E+02  1.020000E+02  1.040000E+02
      9.900000E+01  1.010000E+02  1.030000E+02  1.050000E+02
      1.000000E+02  1.020000E+02  1.040000E+02  1.060000E+02

      1.030000E+02  1.050000E+02  1.070000E+02  1.090000E+02
      1.040000E+02  1.060000E+02  1.080000E+02  1.100000E+02
      1.050000E+02  1.070000E+02  1.090000E+02  1.110000E+02

   2. Run fp2hdf on the test files that were created:
    
      ./fp2hdf ctxtr2 -o ctxtr2.hdf
      ./fp2hdf ctxtr3 -o ctxtr3.hdf
      ./fp2hdf cb32r2 -o cb32r2.hdf
      ./fp2hdf cb32r3 -o cb32r3.hdf
      ./fp2hdf cb64r2 -o cb64r2.hdf
      ./fp2hdf cb64r3 -o cb64r3.hdf
      ./fp2hdf ctxtr2 -o ctxtr2_ris.hdf -raster -e 50 50
      ./fp2hdf cb64r2 -o cb64r2_ris.hdf -raster -i 50 50 -f

   3. Use hdfls and hdfed to verify the correctness of the output 
      *.hdf files.  The rank, dimension size, number type, 
      dimension scale and data values should agree with those
      listed in item 1 above. 

      You can also use the hdp command.  To use hdp from the
      util/ directory, type:
   
       For an SDS:

        ../../mfhdf/dumper/hdp dumpsds <hdf filename>

       For a raster image:
  
        ../../mfhdf/dumper/hdp dumprig <hdf filename>

      Collage can also be used to display the *.hdf files.
      Display the spreadsheet and compare the values to those
      in Item 1.  For the Raster Images, display the image; 
      the values are interpolated, and will not match the
      values as show in Item 1.  

        ctxtr2.hdf -- 2D SDS, display spreadsheet
        ctxtr3.hdf -- 3D SDS, display spreadsheet along z axis.
        cb32r2.hdf -- 2D SDS, display spreadsheet
        cb32r3.hdf -- 3D SDS, display spreadsheet along z axis
        cb64r2.hdf -- 2D SDS, display spreadsheet
        cb64r3.hdf -- 3D SDS, display spreadsheet along z axis
        ctxtr2_ris.hdf -- RIS, display image
        cb64r2_ris.hdf -- 2D SDS, display spreadsheet
                          RIS, display image 
   
   4. Remove ctxtr* and cb*.  

       rm ctxtr* cb* *.hdf
                   






