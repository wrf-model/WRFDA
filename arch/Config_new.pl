#!/usr/bin/perl
#
# Configuration script for WRF code

$sw_devtop = `pwd` ;
$sw_registry = "" ;
$sw_em_core = "" ;
$sw_da_core = "" ;
$sw_nmm_core = "" ;
$sw_coamps_core = "" ;
$sw_exp_core = "" ;
$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_phdf5_path=""; 
$sw_hdf_path=""; 
$sw_hdfeos_path=""; 
$sw_jasperlib_path=""; 
$sw_jasperinc_path=""; 
$sw_esmflib_path="";
$sw_esmfinc_path="";
$sw_rttov_path=""; 
$sw_ldflags=""; 
$sw_compileflags=""; 
$WRFCHEM = 0 ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any

# Transfer arguments to local variables

while ( substr( $ARGV[0], 0, 1 ) eq "-" ) {
  if ( substr( $ARGV[0], 1, 9 ) eq "registry=" ) {
    $sw_registry = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "em_core=" ) {
    $sw_em_core = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "da_core=" ) {
    $sw_da_core = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "nmm_core=" ) {
    $sw_nmm_core = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "coamps_core=" ) {
    $sw_coamps_core = substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "exp_core=" ) {
    $sw_exp_core = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "perl=" ) {
    $sw_perl_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" ) {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 6 ) eq "phdf5=" ) {
    $sw_phdf5_path = substr( $ARGV[0], 7 ) ;
  }
  if ( substr( $ARGV[0], 1, 4 ) eq "hdf=" ) {
    $sw_hdf_path = substr( $ARGV[0], 5 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "hdfeos=" ) {
    $sw_hdfeos_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 6 ) eq "rttov=" ) {
    $sw_rttov_path = substr( $ARGV[0], 7 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" ) {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" ) {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "ldflags=" ) {
    $sw_ldflags = substr( $ARGV[0], 9 ) ;
    # multiple options separated by spaces are passed in from sh script
    # separated by ! instead. Replace with spaces here.
    $sw_ldflags =~ s/!/ /g ;
  }
  if ( substr( $ARGV[0], 1, 13 ) eq "compileflags=" ) {
    $sw_compileflags = substr( $ARGV[0], 14 ) ;
    $sw_compileflags =~ s/!/ /g ;
    # look for each known option
    $where_index = index ( $sw_compileflags , "-DWRF_CHEM" ) ;
    if ( $where_index eq -1 ) {
      $WRFCHEM = 0 ;
    } else {
      $WRFCHEM = 1 ;
    } 
  }
  shift @ARGV ;
}

# The jasper library is required to build Grib2 I/O.  User must set 
# environment variables JASPER_LIB and JASPER_INC to paths to library and 
# include files to enable this feature prior to running configure.  
 if ( $ENV{JASPER_LIB} && $ENV{JASPER_INC} )
   {
   printf "Configuring to use jasper library to build Grib2 I/O...\n" ;
   printf("  \$JASPER_LIB = %s\n",$ENV{JASPER_LIB});
   printf("  \$JASPER_INC = %s\n",$ENV{JASPER_INC});
   $sw_jasperlib_path = $ENV{JASPER_LIB}; 
   $sw_jasperinc_path = $ENV{JASPER_INC}; 
   }
 else
   {
   printf "\$JASPER_LIB or \$JASPER_INC not found in environment, configuring to build without grib2 I/O...\n" ;
   }

# A separately-installed ESMF library is required to build the ESMF 
# implementation of WRF IOAPI in external/io_esmf.  This is needed 
# to couple WRF with other ESMF components.  User must set environment 
# variables ESMF_LIB and ESMF_INC to paths ESMF to library and include 
# files to enable this feature prior to running configure.
 if ( $ENV{ESMF_LIB} && $ENV{ESMF_INC} )
   {
   printf "Configuring to use ESMF library to build WRF...\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf "WARNING:  THIS IS AN EXPERIMENTAL CONFIGURATION\n" ;
   printf "WARNING:  IT DOES NOT WORK WITH NESTING\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf("  \$ESMF_LIB = %s\n",$ENV{ESMF_LIB});
   printf("  \$ESMF_INC = %s\n",$ENV{ESMF_INC});
   $sw_esmflib_path = $ENV{ESMF_LIB};
   $sw_esmfinc_path = $ENV{ESMF_INC};
   }

# parse the configure.defaults_new file

$validresponse = 0 ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;
  printf "Choose single-threaded options for be.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults_new" 
      or die "Cannot open ./arch/configure.defaults_new for reading" ;
  while ( <CONFIGURE_DEFAULTS> ) {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && 
       ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 ) ) {
      $optstr[$opt] = substr($_,6) ;
      $optstr[$opt] =~ s/^[ 	]*// ;
      if ( substr( $optstr[$opt], 0,4 ) ne "NULL" ) {
        printf "  %2d.  %s",$opt,$optstr[$opt] ;
        $opt++ ;
      }
    }
  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) { 
    $validresponse = 1 ;
  } else { 
    printf("\nInvalid response (%d)\n",$response);
  }
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;


open CONFIGURE_PREAMBLE, "< ./arch/preamble_new" 
  or die "Cannot open ./arch/preamble_new for reading" ;
$latchon = 0 ;
while ( <CONFIGURE_PREAMBLE> ) {

  $_ =~ s/CONFIGURE_DEVTOP/$sw_devtop/g ;
  $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g ;
  $_ =~ s/CONFIGURE_LDFLAGS/$sw_ldflags/g ;
  $_ =~ s/CONFIGURE_COMPILEFLAGS/$sw_compileflags/g ;
  $_ =~ s/CONFIGURE_REGISTRY/$sw_registry/g ;
  $_ =~ s/CONFIGURE_DA_CORE/$sw_da_core/g ;
  $_ =~ s/CONFIGURE_EM_CORE/$sw_em_core/g ;
  $_ =~ s/CONFIGURE_NMM_CORE/$sw_nmm_core/g ;
  $_ =~ s/CONFIGURE_COAMPS_CORE/$sw_coamps_core/g ;
  $_ =~ s/CONFIGURE_EXP_CORE/$sw_exp_core/g ;
  
  if ( $sw_netcdf_path ) { 
    $_ =~ s:CONFIGURE_NETCDF_PATH:$sw_netcdf_path:g ;
    $_ =~ s:CONFIGURE_WRFIO_NF:wrfio_nf:g ;
    $_ =~ s:CONFIGURE_NETCDF_FLAG:-DNETCDF: ;
    $_ =~ s:CONFIGURE_NETCDF_LIB:-lnetcdf: ;
  } else { 
    $_ =~ s:CONFIGURE_NETCDF_PATH::g ;
    $_ =~ s:CONFIGURE_WRFIO_NF::g ;
    $_ =~ s:CONFIGURE_NETCDF_FLAG::g ;
    $_ =~ s:CONFIGURE_NETCDF_LIB::g ;
  }

  if ( $sw_phdf5_path ) { 
    $_ =~ s:CONFIGURE_PHDF5_PATH:$sw_phdf5_path: ;
    $_ =~ s:CONFIGURE_WRFIO_PHDF5:wrfio_phdf5:g ;
    $_ =~ s:CONFIGURE_PHDF5_FLAG:-DPHDF5: ;
  } else { 
    $_ =~ s:CONFIGURE_PHDF5_PATH::g ;
    $_ =~ s:CONFIGURE_WRFIO_PHDF5::g ;
    $_ =~ s:CONFIGURE_PHDF5_FLAG::g ;
  }

  if ( $sw_hdf_path ) { 
    $_ =~ s:CONFIGURE_HDF_LIB:$sw_hdf_path/NewHDF/lib: ;
    $_ =~ s:CONFIGURE_HDF_INC:$sw_hdf_path/NewHDF/include: ;
  } else { 
    $_ =~ s:CONFIGURE_HDF_LIB::g ;
    $_ =~ s:CONFIGURE_HDF_INC::g ;
  }

  if ( $sw_hdfeos_path ) { 
    $_ =~ s:CONFIGURE_HDFEOS_LIB:$sw_hdfeos_path/lib: ;
    $_ =~ s:CONFIGURE_HDFEOS_INC:$sw_hdfeos_path/include: ;
  } else { 
    $_ =~ s:CONFIGURE_HDFEOS_LIB::g ;
    $_ =~ s:CONFIGURE_HDFEOS_INC::g ;
  }

  if ( $sw_jasperlib_path && $sw_jasperinc_path ) 
    { $_ =~ s:CONFIGURE_WRFIO_GRIB2:wrfio_grib2:g ;
      $_ =~ s:CONFIGURE_GRIB2_FLAG:-DGRIB2:g ;
      $_ =~ s:CONFIGURE_GRIB2_INC:-I$sw_jasperinc_path:g ;
      $_ =~ s:CONFIGURE_GRIB2_LIB:-L../external/io_grib2 -lio_grib2 -L$sw_jasperlib_path -ljasper:g ;
    } else { $_ =~ s:CONFIGURE_WRFIO_GRIB2::g ;
      $_ =~ s:CONFIGURE_GRIB2_FLAG::g ;
      $_ =~ s:CONFIGURE_GRIB2_INC::g ;
      $_ =~ s:CONFIGURE_GRIB2_LIB::g ;
    }

  # ESMF substitutions in configure.defaults_new
  if ( $sw_esmflib_path && $sw_esmfinc_path ) {
    $_ =~ s:CONFIGURE_ESMF_IO_LIB:-L$sw_esmflib_path -lesmf -L../external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
    $_ =~ s:CONFIGURE_ESMF_IO_EXT_LIB:-L$sw_esmflib_path -lesmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
    $_ =~ s:CONFIGURE_ESMF_COUPLING:1:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_DEPENDENCE:module_utility.o:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_INC:-I$sw_esmfinc_path -I../main:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_INC:-I../external/io_esmf:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_DEFS:-DESMFIO:g ;
    $_ =~ s:CONFIGURE_ESMF_TARGET:wrfio_esmf:g ;
  } else {
    $_ =~ s:CONFIGURE_ESMF_IO_LIB: -lesmf_time:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_EXT_LIB: -lesmf_time:g ;
    $_ =~ s:CONFIGURE_ESMF_COUPLING:0:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_DEPENDENCE:module_utility.o:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_INC::g ;
    $_ =~ s:CONFIGURE_ESMF_IO_INC:-I../external/esmf_time_f90:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_DEFS::g ;
    $_ =~ s:CONFIGURE_ESMF_TARGET:esmf_time:g ;
  }

  if ( $sw_rttov_path ) {
    $_ =~ s:CONFIGURE_RTTOV_PATH:$sw_rttov_path:g ;
    $_ =~ s:CONFIGURE_RTTOV_FLAG:-DRTTOV: ;
    $_ =~ s:CONFIGURE_RTTOV_LIB:-L$sw_rttov_path/lib -lrttov: ;
    $_ =~ s:CONFIGURE_RTTOV_INC:-I$sw_rttov_path/src: ;
  } else {
    $_ =~ s:CONFIGURE_RTTOV_PATH::g ;
    $_ =~ s:CONFIGURE_RTTOV_FLAG::g ;
    $_ =~ s:CONFIGURE_RTTOV_LIB::g ;
    $_ =~ s:CONFIGURE_RTTOV_INC::g ;
  }

  @machopts1 = ( @machopts1, $_ ) ;
  if ( substr( $_, 0, 10 ) eq "ENVCOMPDEF" )
  {
    @machopts1 = ( @machopts1, "WRF_CHEM\t=\t$WRFCHEM \n" ) ;
  }
}
close CONFIGURE_PREAMBLE ;

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults_new" 
  or die "Cannot open ./arch/configure.defaults_new for reading" ;
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> ) {
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 ) {
    $latchon = 0 ;
  }
  if ( $latchon == 1 ) {

    @machopts2 = ( @machopts2, $_ ) ;

  }
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0 ) {
    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    if ( $x eq $optstr[$optchoice] ) {
      $latchon = 1 ;
    }
  }
}
close CONFIGURE_DEFAULTS ;



printf "\nYou have chosen: %s",$optstr[$optchoice] ;
printf "The options for this platform have been written to the file configure.wrf\n" ;
printf "If you wish to change settings, please edit that file.\n" ;
printf "If you wish to change the default options, edit the file:\n\n" ;
printf "     arch/configure.defaults_new\n" ;
printf "\n" ;

open CONFIGURE_WRF, "> configure.wrf" 
  or die "cannot append configure.wrf" ;

print CONFIGURE_WRF "# configure.wrf\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# This file was automatically generated by the configure script in the\n";
print CONFIGURE_WRF "# top level directory. You may make changes to the settings in this\n";
print CONFIGURE_WRF "# file but be aware they will be overwritten each time you run configure.\n";
print CONFIGURE_WRF "# Ordinarily, it is necessary to run configure once, when the code is\n";
print CONFIGURE_WRF "# first installed.\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# To permanently change options, change the settings for your platform\n";
print CONFIGURE_WRF "# in the file arch/configure.defaults_new then rerun configure.\n";
print CONFIGURE_WRF "#\n";
# add preamble
print CONFIGURE_WRF @machopts1  ;
print CONFIGURE_WRF "#--------------------------------------------------------\n";
print CONFIGURE_WRF "#Platform Dependant\n";
print CONFIGURE_WRF "#--------------------------------------------------------\n";
# add platform dependant
printf CONFIGURE_WRF "# $optstr[$optchoice]\n" ;
print CONFIGURE_WRF @machopts2  ;

print CONFIGURE_WRF "#--------------------------------------------------------\n";
print CONFIGURE_WRF "#Postamble\n";
print CONFIGURE_WRF "#--------------------------------------------------------\n";


open ARCH_POSTAMBLE, "< arch/postamble_new" 
  or die "cannot open arch/postamble_new" ;

while ( <ARCH_POSTAMBLE> ) { 
  print CONFIGURE_WRF
}
close ARCH_POSTAMBLE ;
close CONFIGURE_WRF ;

printf "Configuration successful for $sw_os. To build the model './compile_new target'.\n" ;
printf "--------------------------------------------------------------------------------\n" ;


