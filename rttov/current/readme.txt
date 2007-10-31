RTTOV-8_7 Release 17 November 2005
***************************************************************************
This readme file refers to the version rttov_8_7 of RTTOV-8.
This is an updated version of the rttov_8_5 code which includes
all bug fixes up to the above date posted on the RTTOV-8 site
bug page.It also includes a new code for the RTTOV_SCATT 
routines provided by ECMWF and modified by the Met Office. 

The rttov-8 code is contained in a single file rttov87.tar.gz.

New users are advised to read the Users guide in the
docs directory. Updated versions of this may also be
obtained at: 
http://www.metoffice.gov.uk/research/interproj/nwpsaf/rtm/
in pdf format which gives all the details necessary to 
compile and run the code. 

RTTOV-8 is designed for UNIX/Linux systems. The software is 
now successfully tested on SUN, HP, SGI, Linux PC, NEC SX6, Cray T3E and 
Fujitsu VPP systems and for a range of compilers listed in the 
Makefile supplied.

The following system components are needed before running RTTOV-8:
    * UNIX or Linux operating system
    * Fortran 90 compiler
    * make utilities
    * gzip and gunzip
    * about 100 MByte of free disk space (160 MByte if you require
      AIRS coefficient files) 

Some basic information on installing the RTTOV-8 Fortran 90 code in 
a UNIX or LINUX environment follows. This assumes the code is obtained 
as a compressed unix tar file via ftp or on CD-ROM from ECMWF Data
Services. The file name should be rttov87.tar.gz and be copied to your 
top RTTOV directory (e.g. ~user/rttov87) from which subdirectories 
will be created.

First uncompress the tar file:  
gunzip rttov87.tar.gz 

and expand it: 
tar -xvf rttov87.tar 
 
The following subdirectories are created and contain:

- src 	Fortran source code + makefile for a variety of platforms	
- scripts  Unix test scripts for running test programs	
- data	Associated input data files required for testing
- rtcoef_rttov7	 RTTOV-7 coefficient files for most sensors supported AIRS must be downloaded
- rtcoef_rttov8	 RTTOV-8 coefficient files for a few sensors supported
- rtcoef_scatt   RTTOV_SCATT coefficient files THIS DIRECTORY IS
EMPTY. COEFFS FOR SSMI AND AMSU-A MUST BE DOWNLOADED FROM THE WEB
SITE IF REQUIRED
- test	Output of test programs run on user's machine
- reftest Output of test programs on NWP SAF test platform 
- docs	  Documentation 

The fortran code consists of subroutines and modules and 3 top 
level programs for testing the RTTOV-8 code:
- tstrad.F90 Test program for classical RTTOV
- example_fwd.F90 Example source code provided to aid user
- rttovcld_test.F90 Test program for rttov_cld
- tstrad_rttov7.F90 Test program calling rttov-8 from an rttov-7 like interface
- rttovscatt_test.F90 Test program for scattering code
- rttov_ascii2bin_coef.F90 Program to convert ascii coeff files to
  binary on users platform.

The first step is to compile the code and make an executable using 
the makefiles supplied. Edit the file called Makefile in src so 
that the f90 compiler options match those available on your machine. 
A selection of compiler flags for different platforms are listed 
so if you are running using one of these compilers you should be 
able to just uncomment the relevant section. 
You can run make like "make basic" to just compile
the part of the code for clear air RTTOV (most users) or  
"make all" to compile all the code. The full set of options are:

	make basic (default): compile classical RTTOV code
	make cld:  compile  cloud code with input cloud profile
	make scat:  compile scattering code
	make all:  compile all the code

With luck the code will compile and produce  
executables for the RTTOV tests 

tstrad.out   (basic RTTOV-8 classical test program)
example_fwd.out (compilation of example source code provided)
rttovcld_test.out (test program for rttov_cld)
tstrad_rttov7.out (test program calling rttov-8 from an rttov-7 like interface)
rttovscatt_test.out (test program for scattering code)
rttov_ascii2bin_coef.out (program to convert ascii coeff files to
binary on users platform)

Then in the scripts directory either run the test script for the 
instrument you are interested in (tstrad_fwd.ksh) after
commenting out those sensors you do not want to simulate)
or run all the test scripts e.g. sh tstrad_all.scr as a full test.
Then look at the size of the *.diff files in the test subdir.
They should be zero for all forward rttov runs but for the
tstrad_full.ksh output they will have some differences as
documented in the users guide.
If the differences are as expected the code is ready to be 
incorporated in your code.
Report all bugs by email to: nwpsaf@metoffice.gov.uk

Note that to reduce the size of the tar file the AIRS coefficient file 
and all but one of the scattering coefficient files are not included 
in this file but can be downloaded from the RTTOV-7/8 
web pages if required.  Also new coefficient files with RTTOV-8
predictors and more FASTEM-3 coefficients will be made available
on the RTTOV-8 web page as they become available. 
Bug fixes announced will also be on the web site along with 
corrected code to replace the module provided in the tar file.

The contents of the tar file for rttov_8_7 are:
 
rttov87/
rttov87/src/
rttov87/src/example_fwd.F90
rttov87/src/mod_cparam.F90
rttov87/src/mod_rttov_scatt_test.F90
rttov87/src/mod_tstrad.F90
rttov87/src/parkind1.F90
rttov87/src/rttov.F90
rttov87/src/rttov_ad.F90
rttov87/src/rttov_aitosu.F90
rttov87/src/rttov_aitosu_ad.F90
rttov87/src/rttov_aitosu_tl.F90
rttov87/src/rttov_ascii2bin_coef.F90
rttov87/src/rttov_boundaryconditions.F90
rttov87/src/rttov_boundaryconditions_ad.F90
rttov87/src/rttov_boundaryconditions_k.F90
rttov87/src/rttov_boundaryconditions_tl.F90
rttov87/src/rttov_calcbt.F90
rttov87/src/rttov_calcbt_ad.F90
rttov87/src/rttov_calcbt_tl.F90
rttov87/src/rttov_calcemis_ir.F90
rttov87/src/rttov_calcemis_mw.F90
rttov87/src/rttov_calcemis_mw_ad.F90
rttov87/src/rttov_calcemis_mw_k.F90
rttov87/src/rttov_calcemis_mw_tl.F90
rttov87/src/rttov_calcpolarisation.F90
rttov87/src/rttov_calcpolarisation_ad.F90
rttov87/src/rttov_calcpolarisation_tl.F90
rttov87/src/rttov_calcrad.F90
rttov87/src/rttov_calcrad_ad.F90
rttov87/src/rttov_calcrad_k.F90
rttov87/src/rttov_calcrad_tl.F90
rttov87/src/rttov_checkinput.F90
rttov87/src/rttov_cld.F90
rttov87/src/rttov_cld_ad.F90
rttov87/src/rttov_cld_k.F90
rttov87/src/rttov_cld_profout_k.F90
rttov87/src/rttov_cld_tl.F90
rttov87/src/rttov_cmpuc.F90
rttov87/src/rttov_coeffname.F90
rttov87/src/rttov_const.F90
rttov87/src/rttov_dealloc_coef.F90
rttov87/src/rttov_deletecomment.F90
rttov87/src/rttov_direct.F90
rttov87/src/rttov_distribcoeffs.F90
rttov87/src/rttov_eddington.F90
rttov87/src/rttov_eddington_ad.F90
rttov87/src/rttov_eddington_k.F90
rttov87/src/rttov_eddington_tl.F90
rttov87/src/rttov_emiscld.F90
rttov87/src/rttov_emiscld_ad.F90
rttov87/src/rttov_emiscld_tl.F90
rttov87/src/rttov_errorhandling.F90
rttov87/src/rttov_errorreport.F90
rttov87/src/rttov_findnextsection.F90
rttov87/src/rttov_global.F90
rttov87/src/rttov_iniedd.F90
rttov87/src/rttov_iniedd_ad.F90
rttov87/src/rttov_iniedd_k.F90
rttov87/src/rttov_iniedd_tl.F90
rttov87/src/rttov_iniscatt.F90
rttov87/src/rttov_iniscatt_ad.F90
rttov87/src/rttov_iniscatt_k.F90
rttov87/src/rttov_iniscatt_tl.F90
rttov87/src/rttov_initcoeffs.F90
rttov87/src/rttov_integrate.F90
rttov87/src/rttov_integrate_ad.F90
rttov87/src/rttov_integrate_k.F90
rttov87/src/rttov_integrate_tl.F90
rttov87/src/rttov_integratesource.F90
rttov87/src/rttov_integratesource_ad.F90
rttov87/src/rttov_integratesource_k.F90
rttov87/src/rttov_integratesource_tl.F90
rttov87/src/rttov_interpcubic.F90
rttov87/src/rttov_interpcubic_ad.F90
rttov87/src/rttov_interpcubic_tl.F90
rttov87/src/rttov_intex.F90
rttov87/src/rttov_intex_ad.F90
rttov87/src/rttov_intex_tl.F90
rttov87/src/rttov_intext_prof.F90
rttov87/src/rttov_k.F90
rttov87/src/rttov_mieproc.F90
rttov87/src/rttov_mieproc_ad.F90
rttov87/src/rttov_mieproc_k.F90
rttov87/src/rttov_mieproc_tl.F90
rttov87/src/rttov_opencoeff.F90
rttov87/src/rttov_polcoe.F90
rttov87/src/rttov_profaux.F90
rttov87/src/rttov_profaux_ad.F90
rttov87/src/rttov_profaux_k.F90
rttov87/src/rttov_profaux_tl.F90
rttov87/src/rttov_profout_k.F90
rttov87/src/rttov_q2v.F90
rttov87/src/rttov_readcoeffs.F90
rttov87/src/rttov_readcoeffs_ascii.F90
rttov87/src/rttov_readcoeffs_binary.F90
rttov87/src/rttov_readscattcoeffs.F90
rttov87/src/rttov_scatt.F90
rttov87/src/rttov_scatt_ad.F90
rttov87/src/rttov_scatt_k.F90
rttov87/src/rttov_scatt_setupindex.F90
rttov87/src/rttov_scatt_test.F90
rttov87/src/rttov_scatt_tl.F90
rttov87/src/rttov_setgeometry.F90
rttov87/src/rttov_setpredictors.F90
rttov87/src/rttov_setpredictors_8.F90
rttov87/src/rttov_setpredictors_8_ad.F90
rttov87/src/rttov_setpredictors_8_k.F90
rttov87/src/rttov_setpredictors_8_tl.F90
rttov87/src/rttov_setpredictors_ad.F90
rttov87/src/rttov_setpredictors_k.F90
rttov87/src/rttov_setpredictors_tl.F90
rttov87/src/rttov_setpressure.F90
rttov87/src/rttov_setup.F90
rttov87/src/rttov_setupchan.F90
rttov87/src/rttov_setupindex.F90
rttov87/src/rttov_skipcommentline.F90
rttov87/src/rttov_tl.F90
rttov87/src/rttov_transmit.F90
rttov87/src/rttov_transmit_ad.F90
rttov87/src/rttov_transmit_k.F90
rttov87/src/rttov_transmit_tl.F90
rttov87/src/rttov_types.F90
rttov87/src/rttov_v2q.F90
rttov87/src/rttov_writecoef.F90
rttov87/src/rttovcld.F90
rttov87/src/rttovcld_test.F90
rttov87/src/rttovcld_testad.F90
rttov87/src/rttovscatt_test.F90
rttov87/src/rttvi.F90
rttov87/src/test_2_coef.F90
rttov87/src/test_coef.F90
rttov87/src/test_errorhandling.F90
rttov87/src/test_q2v.F90
rttov87/src/tstrad.F90
rttov87/src/tstrad_ad.F90
rttov87/src/tstrad_chansubset.F90
rttov87/src/tstrad_k.F90
rttov87/src/tstrad_rttov7.F90
rttov87/src/tstrad_sx6.F90
rttov87/src/tstrad_tl.F90
rttov87/src/rttov.interface
rttov87/src/rttov_ad.interface
rttov87/src/rttov_aitosu.interface
rttov87/src/rttov_aitosu_ad.interface
rttov87/src/rttov_aitosu_tl.interface
rttov87/src/rttov_boundaryconditions.interface
rttov87/src/rttov_boundaryconditions_ad.interface
rttov87/src/rttov_boundaryconditions_k.interface
rttov87/src/rttov_boundaryconditions_tl.interface
rttov87/src/rttov_calcbt.interface
rttov87/src/rttov_calcbt_ad.interface
rttov87/src/rttov_calcbt_tl.interface
rttov87/src/rttov_calcemis_ir.interface
rttov87/src/rttov_calcemis_mw.interface
rttov87/src/rttov_calcemis_mw_ad.interface
rttov87/src/rttov_calcemis_mw_k.interface
rttov87/src/rttov_calcemis_mw_tl.interface
rttov87/src/rttov_calcpolarisation.interface
rttov87/src/rttov_calcpolarisation_ad.interface
rttov87/src/rttov_calcpolarisation_tl.interface
rttov87/src/rttov_calcrad.interface
rttov87/src/rttov_calcrad_ad.interface
rttov87/src/rttov_calcrad_k.interface
rttov87/src/rttov_calcrad_tl.interface
rttov87/src/rttov_checkinput.interface
rttov87/src/rttov_cld.interface
rttov87/src/rttov_cld_ad.interface
rttov87/src/rttov_cld_k.interface
rttov87/src/rttov_cld_profout_k.interface
rttov87/src/rttov_cld_tl.interface
rttov87/src/rttov_cmpuc.interface
rttov87/src/rttov_coeffname.interface
rttov87/src/rttov_dealloc_coef.interface
rttov87/src/rttov_deletecomment.interface
rttov87/src/rttov_direct.interface
rttov87/src/rttov_distribcoeffs.interface
rttov87/src/rttov_eddington.interface
rttov87/src/rttov_eddington_ad.interface
rttov87/src/rttov_eddington_k.interface
rttov87/src/rttov_eddington_tl.interface
rttov87/src/rttov_emiscld.interface
rttov87/src/rttov_emiscld_ad.interface
rttov87/src/rttov_emiscld_tl.interface
rttov87/src/rttov_errorhandling.interface
rttov87/src/rttov_errorreport.interface
rttov87/src/rttov_findnextsection.interface
rttov87/src/rttov_iniedd.interface
rttov87/src/rttov_iniedd_ad.interface
rttov87/src/rttov_iniedd_k.interface
rttov87/src/rttov_iniedd_tl.interface
rttov87/src/rttov_iniscatt.interface
rttov87/src/rttov_iniscatt_ad.interface
rttov87/src/rttov_iniscatt_k.interface
rttov87/src/rttov_iniscatt_tl.interface
rttov87/src/rttov_initcoeffs.interface
rttov87/src/rttov_integrate.interface
rttov87/src/rttov_integrate_ad.interface
rttov87/src/rttov_integrate_k.interface
rttov87/src/rttov_integrate_tl.interface
rttov87/src/rttov_integratesource.interface
rttov87/src/rttov_integratesource_ad.interface
rttov87/src/rttov_integratesource_k.interface
rttov87/src/rttov_integratesource_tl.interface
rttov87/src/rttov_interpcubic.interface
rttov87/src/rttov_interpcubic_ad.interface
rttov87/src/rttov_interpcubic_tl.interface
rttov87/src/rttov_intex.interface
rttov87/src/rttov_intex_ad.interface
rttov87/src/rttov_intex_tl.interface
rttov87/src/rttov_intext_prof.interface
rttov87/src/rttov_k.interface
rttov87/src/rttov_mieproc.interface
rttov87/src/rttov_mieproc_ad.interface
rttov87/src/rttov_mieproc_k.interface
rttov87/src/rttov_mieproc_tl.interface
rttov87/src/rttov_opencoeff.interface
rttov87/src/rttov_polcoe.interface
rttov87/src/rttov_profaux.interface
rttov87/src/rttov_profaux_ad.interface
rttov87/src/rttov_profaux_k.interface
rttov87/src/rttov_profaux_tl.interface
rttov87/src/rttov_profout_k.interface
rttov87/src/rttov_q2v.interface
rttov87/src/rttov_readcoeffs.interface
rttov87/src/rttov_readcoeffs_ascii.interface
rttov87/src/rttov_readcoeffs_binary.interface
rttov87/src/rttov_readscattcoeffs.interface
rttov87/src/rttov_scatt.interface
rttov87/src/rttov_scatt_ad.interface
rttov87/src/rttov_scatt_k.interface
rttov87/src/rttov_scatt_tl.interface
rttov87/src/rttov_setgeometry.interface
rttov87/src/rttov_setpredictors.interface
rttov87/src/rttov_setpredictors_8.interface
rttov87/src/rttov_setpredictors_8_ad.interface
rttov87/src/rttov_setpredictors_8_k.interface
rttov87/src/rttov_setpredictors_8_tl.interface
rttov87/src/rttov_setpredictors_ad.interface
rttov87/src/rttov_setpredictors_k.interface
rttov87/src/rttov_setpredictors_tl.interface
rttov87/src/rttov_setup.interface
rttov87/src/rttov_setupchan.interface
rttov87/src/rttov_setupindex.interface
rttov87/src/rttov_skipcommentline.interface
rttov87/src/rttov_tl.interface
rttov87/src/rttov_transmit.interface
rttov87/src/rttov_transmit_ad.interface
rttov87/src/rttov_transmit_k.interface
rttov87/src/rttov_transmit_tl.interface
rttov87/src/rttov_v2q.interface
rttov87/src/rttov_writecoef.interface
rttov87/src/rttovcld.interface
rttov87/src/rttvi.interface
rttov87/src/tstrad_ad.interface
rttov87/src/tstrad_k.interface
rttov87/src/tstrad_tl.interface
rttov87/src/Makefile
rttov87/src/Makefile_lib
rttov87/src/Makefile_main
rttov87/docs/
rttov87/docs/RTTOV8_5_Release_Note.pdf
rttov87/docs/RTTOV8_TLD.pdf
rttov87/docs/RTTOV_Release_Note.pdf
rttov87/docs/SPEC_RTTOV8.pdf
rttov87/docs/coef_file_format.pdf
rttov87/docs/compilers.pdf
rttov87/docs/licence.pdf
rttov87/docs/release_authorization_checklist.pdf
rttov87/docs/rttov8_5_release_note.pdf
rttov87/docs/rttov8_beta_test.pdf
rttov87/docs/rttov8_code_review.pdf
rttov87/docs/rttov8_release_list.pdf
 rttov87/docs/rttov8_svr.pdf
 rttov87/docs/rttov8_test_plan_and_resources.pdf
 rttov87/docs/users_guide.pdf
 rttov87/docs/compilers.pdf
 rttov87/docs/coef_file_format.pdf
 rttov87/reftest/
 rttov87/reftest/outputcld_m2_s15_i0_r0_k0.lst
 rttov87/reftest/outputcld_m2_s15_i0_r0_k1.lst
 rttov87/reftest/outputcld_m2_s15_i0_r1_k0.lst
 rttov87/reftest/outputcld_m2_s15_i0_r1_k1.lst
 rttov87/reftest/outputcld_m2_s15_i0_r2_k0.lst
 rttov87/reftest/outputcld_m2_s15_i0_r2_k1.lst
 rttov87/reftest/outputcld_m2_s15_i0_r3_k0.lst
 rttov87/reftest/outputcld_m2_s15_i0_r3_k1.lst
 rttov87/reftest/outputcld_m2_s15_i3.lst
 rttov87/reftest/outputscatt_amsu7.lst
 rttov87/reftest/outputscatt_ssmi7.lst
 rttov87/reftest/tstrad_full_01.lst
 rttov87/reftest/tstrad_full_02.lst
 rttov87/reftest/tstrad_full_03.lst
 rttov87/reftest/tstrad_full_04.lst
 rttov87/reftest/tstrad_full_05.lst
 rttov87/reftest/tstrad_full_06.lst
 rttov87/reftest/tstrad_full_07.lst
 rttov87/reftest/tstrad_full_08.lst
 rttov87/reftest/tstrad_full_09.lst
 rttov87/reftest/tstrad_full_10.lst
 rttov87/reftest/tstrad_full_11.lst
 rttov87/reftest/tstrad_full_12.lst
 rttov87/reftest/tstrad_full_13.lst
 rttov87/reftest/tstrad_full_14.lst
 rttov87/reftest/tstrad_full_15.lst
 rttov87/reftest/tstrad_full_16.lst
 rttov87/reftest/tstrad_full_17.lst
 rttov87/reftest/tstrad_full_18.lst
 rttov87/reftest/tstrad_full_19.lst
 rttov87/reftest/tstrad_full_20.lst
 rttov87/reftest/tstrad_full_21.lst
 rttov87/reftest/tstrad_full_22.lst
 rttov87/reftest/tstrad_full_23.lst
 rttov87/reftest/tstrad_full_24.lst
 rttov87/reftest/tstrad_full_25.lst
 rttov87/reftest/tstrad_full_26.lst
 rttov87/reftest/tstrad_full_27.lst
 rttov87/reftest/tstrad_full_28.lst
 rttov87/reftest/tstrad_full_29.lst
 rttov87/reftest/tstrad_full_30.lst
 rttov87/reftest/tstrad_full_31.lst
 rttov87/reftest/tstrad_full_32.lst
 rttov87/reftest/tstrad_fwd_airs_01.lst
 rttov87/reftest/tstrad_fwd_amsr_01.lst
 rttov87/reftest/tstrad_fwd_amsr_02.lst
 rttov87/reftest/tstrad_fwd_amsua_01.lst
 rttov87/reftest/tstrad_fwd_amsua_02.lst
 rttov87/reftest/tstrad_fwd_amsua_03.lst
 rttov87/reftest/tstrad_fwd_amsua_04.lst
 rttov87/reftest/tstrad_fwd_amsub_01.lst
 rttov87/reftest/tstrad_fwd_amsub_02.lst
 rttov87/reftest/tstrad_fwd_amsub_03.lst
 rttov87/reftest/tstrad_fwd_atsr_01.lst
 rttov87/reftest/tstrad_fwd_atsr_02.lst
 rttov87/reftest/tstrad_fwd_atsr_03.lst
 rttov87/reftest/tstrad_fwd_avhrr_01.lst
 rttov87/reftest/tstrad_fwd_avhrr_02.lst
 rttov87/reftest/tstrad_fwd_avhrr_03.lst
 rttov87/reftest/tstrad_fwd_avhrr_04.lst
 rttov87/reftest/tstrad_fwd_avhrr_05.lst
 rttov87/reftest/tstrad_fwd_avhrr_06.lst
 rttov87/reftest/tstrad_fwd_avhrr_07.lst
 rttov87/reftest/tstrad_fwd_avhrr_08.lst
 rttov87/reftest/tstrad_fwd_avhrr_09.lst
 rttov87/reftest/tstrad_fwd_avhrr_10.lst
 rttov87/reftest/tstrad_fwd_avhrr_11.lst
 rttov87/reftest/tstrad_fwd_avhrr_12.lst
 rttov87/reftest/tstrad_fwd_avhrr_13.lst
 rttov87/reftest/tstrad_fwd_hirs_01.lst
 rttov87/reftest/tstrad_fwd_hirs_02.lst
 rttov87/reftest/tstrad_fwd_hirs_03.lst
 rttov87/reftest/tstrad_fwd_hirs_04.lst
 rttov87/reftest/tstrad_fwd_hirs_05.lst
 rttov87/reftest/tstrad_fwd_hirs_06.lst
 rttov87/reftest/tstrad_fwd_hirs_07.lst
 rttov87/reftest/tstrad_fwd_hirs_08.lst
 rttov87/reftest/tstrad_fwd_hirs_09.lst
 rttov87/reftest/tstrad_fwd_hirs_10.lst
 rttov87/reftest/tstrad_fwd_hirs_11.lst
 rttov87/reftest/tstrad_fwd_hirs_12.lst
 rttov87/reftest/tstrad_fwd_hirs_13.lst
 rttov87/reftest/tstrad_fwd_imager_01.lst
 rttov87/reftest/tstrad_fwd_imager_02.lst
 rttov87/reftest/tstrad_fwd_imager_03.lst
 rttov87/reftest/tstrad_fwd_imager_04.lst
 rttov87/reftest/tstrad_fwd_imager_05.lst
 rttov87/reftest/tstrad_fwd_imager_11.lst
 rttov87/reftest/tstrad_fwd_imager_12.lst
 rttov87/reftest/tstrad_fwd_mhs_01.lst
 rttov87/reftest/tstrad_fwd_modis_01.lst
 rttov87/reftest/tstrad_fwd_modis_02.lst
 rttov87/reftest/tstrad_fwd_msu_01.lst
 rttov87/reftest/tstrad_fwd_msu_02.lst
 rttov87/reftest/tstrad_fwd_msu_03.lst
 rttov87/reftest/tstrad_fwd_msu_04.lst
 rttov87/reftest/tstrad_fwd_msu_05.lst
 rttov87/reftest/tstrad_fwd_msu_06.lst
 rttov87/reftest/tstrad_fwd_msu_07.lst
 rttov87/reftest/tstrad_fwd_msu_08.lst
 rttov87/reftest/tstrad_fwd_msu_09.lst
 rttov87/reftest/tstrad_fwd_mviri_01.lst
 rttov87/reftest/tstrad_fwd_mviri_02.lst
 rttov87/reftest/tstrad_fwd_mviri_03.lst
 rttov87/reftest/tstrad_fwd_seviri_01.lst
 rttov87/reftest/tstrad_fwd_seviri_02.lst
 rttov87/reftest/tstrad_fwd_sounder_01.lst
 rttov87/reftest/tstrad_fwd_sounder_02.lst
 rttov87/reftest/tstrad_fwd_sounder_03.lst
 rttov87/reftest/tstrad_fwd_sounder_04.lst
 rttov87/reftest/tstrad_fwd_ssmi_01.lst
 rttov87/reftest/tstrad_fwd_ssmi_02.lst
 rttov87/reftest/tstrad_fwd_ssmi_03.lst
 rttov87/reftest/tstrad_fwd_ssmi_04.lst
 rttov87/reftest/tstrad_fwd_ssmi_05.lst
 rttov87/reftest/tstrad_fwd_ssmi_06.lst
 rttov87/reftest/tstrad_fwd_ssmi_07.lst
 rttov87/reftest/tstrad_fwd_ssmis_01.lst
 rttov87/reftest/tstrad_fwd_ssu_01.lst
 rttov87/reftest/tstrad_fwd_ssu_02.lst
 rttov87/reftest/tstrad_fwd_ssu_03.lst
 rttov87/reftest/tstrad_fwd_ssu_04.lst
 rttov87/reftest/tstrad_fwd_ssu_05.lst
 rttov87/reftest/tstrad_fwd_ssu_06.lst
 rttov87/reftest/tstrad_fwd_ssu_07.lst
 rttov87/reftest/tstrad_fwd_ssu_08.lst
 rttov87/reftest/tstrad_fwd_vissr_13.lst
 rttov87/reftest/tstrad_fwd_windsat_01.lst
 rttov87/reftest/rtcoef_1_ascii.out
 rttov87/reftest/rtcoef_1_ascii_2.out
 rttov87/reftest/rtcoef_1_binary.out
 rttov87/reftest/rtcoef_2_ascii.out
 rttov87/reftest_rttov8/
 rttov87/reftest_rttov8/outputscatt_amsu8.lst
 rttov87/reftest_rttov8/outputscatt_ssmi8.lst
 rttov87/reftest_rttov8/tstrad_full_01.lst
 rttov87/reftest_rttov8/tstrad_full_02.lst
 rttov87/reftest_rttov8/tstrad_full_03.lst
 rttov87/reftest_rttov8/tstrad_full_04.lst
 rttov87/reftest_rttov8/tstrad_full_05.lst
 rttov87/reftest_rttov8/tstrad_full_06.lst
 rttov87/reftest_rttov8/tstrad_full_07.lst
 rttov87/reftest_rttov8/tstrad_full_08.lst
 rttov87/reftest_rttov8/tstrad_full_09.lst
 rttov87/reftest_rttov8/tstrad_full_10.lst
 rttov87/reftest_rttov8/tstrad_full_11.lst
 rttov87/reftest_rttov8/tstrad_full_12.lst
 rttov87/reftest_rttov8/tstrad_full_13.lst
 rttov87/reftest_rttov8/tstrad_full_14.lst
 rttov87/reftest_rttov8/tstrad_full_15.lst
 rttov87/reftest_rttov8/tstrad_full_16.lst
 rttov87/reftest_rttov8/tstrad_full_17.lst
 rttov87/reftest_rttov8/tstrad_full_18.lst
 rttov87/reftest_rttov8/tstrad_full_19.lst
 rttov87/reftest_rttov8/tstrad_full_20.lst
 rttov87/reftest_rttov8/tstrad_full_21.lst
 rttov87/reftest_rttov8/tstrad_full_22.lst
 rttov87/reftest_rttov8/tstrad_full_23.lst
 rttov87/reftest_rttov8/tstrad_full_24.lst
 rttov87/reftest_rttov8/tstrad_full_25.lst
 rttov87/reftest_rttov8/tstrad_full_26.lst
 rttov87/reftest_rttov8/tstrad_full_27.lst
 rttov87/reftest_rttov8/tstrad_full_28.lst
 rttov87/reftest_rttov8/tstrad_full_29.lst
 rttov87/reftest_rttov8/tstrad_full_30.lst
 rttov87/reftest_rttov8/tstrad_full_31.lst
 rttov87/reftest_rttov8/tstrad_full_32.lst
 rttov87/reftest_rttov8/tstrad_full_33.lst
 rttov87/reftest_rttov8/tstrad_full_34.lst
 rttov87/reftest_rttov8/tstrad_full_35.lst
 rttov87/reftest_rttov8/tstrad_full_36.lst
 rttov87/scripts/
 rttov87/scripts/example_fwd.ksh
 rttov87/scripts/move_F90_to_f90.ksh
 rttov87/scripts/move_f90_to_F90.ksh
 rttov87/scripts/rttov_ascii2bin_coef.ksh
 rttov87/scripts/rttov_id.ksh
 rttov87/scripts/single_test_full.ksh
 rttov87/scripts/single_test_fwd.ksh
 rttov87/scripts/single_test_fwd_rttov7.ksh
 rttov87/scripts/single_test_sx6.ksh
 rttov87/scripts/test_all.ksh
 rttov87/scripts/test_coef.ksh
 rttov87/scripts/test_errorhandling.ksh
 rttov87/scripts/test_full.ksh
 rttov87/scripts/test_fwd.ksh
 rttov87/scripts/test_fwd_rttov7.ksh
 rttov87/scripts/test_rttov8.ksh
 rttov87/scripts/test_rttovcld.ksh
 rttov87/scripts/test_rttovscatt.ksh
 rttov87/scripts/test_sx6.ksh
 rttov87/data/
 rttov87/data/Mid-Latitude_Summer.dat
 rttov87/data/Mid-Latitude_Winter.dat
 rttov87/data/Sub-Arctic_Summer.dat
 rttov87/data/Sub-Arctic_Winter.dat
 rttov87/data/Tropical.dat
 rttov87/data/ec_prof_1.dat
 rttov87/data/input_airs.dat
 rttov87/data/input_airs_1.dat
 rttov87/data/input_airs_118.dat
 rttov87/data/input_airs_120.dat
 rttov87/data/input_airs_20.dat
 rttov87/data/input_airs_281.dat
 rttov87/data/input_airs_em8.dat
 rttov87/data/input_airs_sx6.dat
 rttov87/data/input_all_amsua.dat
 rttov87/data/input_all_amsub.dat
 rttov87/data/input_allsats_ssmi.dat
 rttov87/data/input_allsats_ssmi_f2.dat
 rttov87/data/input_amsr.dat
 rttov87/data/input_amsre.dat
 rttov87/data/input_amsua.dat
 rttov87/data/input_amsua_em05.dat
 rttov87/data/input_amsua_f1.dat
 rttov87/data/input_amsua_f2.dat
 rttov87/data/input_amsub_f2.dat
 rttov87/data/input_atsr.dat
 rttov87/data/input_avhrr.dat
 rttov87/data/input_avhrr1.dat
 rttov87/data/input_gms.dat
 rttov87/data/input_goesim.dat
 rttov87/data/input_goessnd.dat
 rttov87/data/input_hirs.dat
 rttov87/data/input_hsb_f2.dat
 rttov87/data/input_ircloud.dat
 rttov87/data/input_ircloud1.dat
 rttov87/data/input_irem.dat
 rttov87/data/input_lwp.dat
 rttov87/data/input_lwp_f2.dat
 rttov87/data/input_met.dat
 rttov87/data/input_modis.dat
 rttov87/data/input_msg.dat
 rttov87/data/input_msu_f2.dat
 rttov87/data/input_mwcloud.dat
 rttov87/data/input_ssmi.dat
 rttov87/data/input_ssmi_f2.dat
 rttov87/data/input_ssmi_f3.dat
 rttov87/data/input_ssmis.dat
 rttov87/data/input_ssmis_f3.dat
 rttov87/data/input_ssmis_test.dat
 rttov87/data/input_ssu.dat
 rttov87/data/input_tmi.dat
 rttov87/data/input_tmi_f2.dat
 rttov87/data/input_universal.dat
 rttov87/data/input_vtpr.dat
 rttov87/data/input_windsat.dat
 rttov87/data/prof_arc.dat
 rttov87/data/prof_arc_f2.dat
 rttov87/data/prof_lwp.dat
 rttov87/data/prof_lwp_f2.dat
 rttov87/data/prof_trop.dat
 rttov87/data/prof_trop_cld.dat
 rttov87/data/prof_trop_f2.dat
 rttov87/data/prof_trop_fracld.dat
 rttov87/data/prof_us_mls_mviri.dat
 rttov87/data/prof_us_mlw_mviri.dat
 rttov87/data/prof_us_stand_hirs.dat
 rttov87/data/prof_us_stand_mviri.dat
 rttov87/data/refprof.dat
 rttov87/data/refprof_43.dat
 rttov87/data/profiles2_fmt
 rttov87/data/profiles_fmt
 rttov87/rtcoef_scatt/
 rttov87/test/
 rttov87/test/rttov7/
 rttov87/readme.txt
 rttov87/rtcoef_rttov7/
 rttov87/rtcoef_rttov7/rtcoef_adeos_2_amsr.dat
 rttov87/rtcoef_rttov7/rtcoef_coriolis_1_windsat.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_10_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_11_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_12_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_13_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_14_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_15_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_16_ssmis.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_1_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_8_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_dmsp_9_ssmi.dat
 rttov87/rtcoef_rttov7/rtcoef_envisat_1_atsr.dat
 rttov87/rtcoef_rttov7/rtcoef_eos_1_modis.dat
 rttov87/rtcoef_rttov7/rtcoef_eos_2_amsr.dat
 rttov87/rtcoef_rttov7/rtcoef_eos_2_amsua.dat
 rttov87/rtcoef_rttov7/rtcoef_eos_2_hsb.dat
 rttov87/rtcoef_rttov7/rtcoef_eos_2_modis.dat
 rttov87/rtcoef_rttov7/rtcoef_ers_1_atsr.dat
 rttov87/rtcoef_rttov7/rtcoef_ers_2_atsr.dat
 rttov87/rtcoef_rttov7/rtcoef_fy1_3_mvisr.dat
 rttov87/rtcoef_rttov7/rtcoef_fy2_2_vissr.dat
 rttov87/rtcoef_rttov7/rtcoef_fy2_3_vissr.dat
 rttov87/rtcoef_rttov7/rtcoef_gms_5_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_10_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_10_sounder.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_11_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_11_sounder.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_12_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_8_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_8_sounder.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_9_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_goes_9_sounder.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_1_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_2_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_3_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_4_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_5_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_6_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_meteosat_7_mviri.dat
 rttov87/rtcoef_rttov7/rtcoef_msg_1_seviri.dat
 rttov87/rtcoef_rttov7/rtcoef_msg_2_seviri.dat
 rttov87/rtcoef_rttov7/rtcoef_mtsat_1_imager.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_10_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_10_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_10_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_10_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_11_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_11_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_11_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_11_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_12_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_12_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_12_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_13_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_14_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_14_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_14_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_14_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_15_amsua.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_15_amsub.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_15_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_15_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_16_amsua.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_16_amsub.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_16_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_16_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_17_amsua.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_17_amsub.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_17_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_17_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_18_amsua.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_18_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_18_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_18_mhs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_1_vtpr1.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_1_vtpr2.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_2_vtpr1.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_2_vtpr2.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_3_vtpr1.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_3_vtpr2.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_4_vtpr1.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_4_vtpr2.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_5_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_5_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_5_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_5_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_6_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_6_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_6_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_6_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_7_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_7_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_7_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_7_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_8_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_8_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_8_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_8_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_9_avhrr.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_9_hirs.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_9_msu.dat
 rttov87/rtcoef_rttov7/rtcoef_noaa_9_ssu.dat
 rttov87/rtcoef_rttov7/rtcoef_trmm_1_tmi.dat
 rttov87/rtcoef_rttov8/
 rttov87/rtcoef_rttov8/fastem3/
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_10_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_11_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_12_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_13_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_14_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_15_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_17_amsub.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_8_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_9_ssmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_eos_2_amsr.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_14_msu.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_15_amsua.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_15_amsub.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_16_amsua.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_16_amsub.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_17_amsua.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_trmm_1_tmi.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_coriolis_1_windsat.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_16_ssmis.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_dmsp_16_ssmis.tmp
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_18_amsua.dat
 rttov87/rtcoef_rttov8/fastem3/rtcoef_noaa_18_mhs.dat
 rttov87/rtcoef_rttov8/rttov7pred/
 rttov87/rtcoef_rttov8/rttov7pred/rtcoef_noaa_15_hirs.dat
 rttov87/rtcoef_rttov8/rttov7pred/rtcoef_noaa_16_avhrr.dat
 rttov87/rtcoef_rttov8/rttov8pred/
 rttov87/rtcoef_rttov8/rttov8pred/rtcoef_noaa_17_hirs.dat
 rttov87/rtcoef_rttov8/rttov8pred/rtcoef_noaa_16_hirs_100L.dat
 rttov87/rtcoef_rttov8/rttov8pred/rtcoef_noaa_16_hirs_43L.dat
 rttov87/rtcoef_rttov8/rttov8pred/rtcoef_noaa_15_hirs.dat
 rttov87/rtcoef_rttov8/rttov8pred/co2var/
 rttov87/rtcoef_rttov8/rttov8pred/co2var/rtcoef_noaa_17_hirs.dat.rttov8_cv
 rttov87/rtcoef_rttov8/rttov8pred/rtcoef_noaa_15_amsua.dat

Roger Saunders, NWP SAF
17 Nov 2005
