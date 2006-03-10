#!/usr/bin/perl
#
# Configuration script for WRF code

$sw_devtop = `pwd` ;
$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_phdf5_path=""; 
$sw_rttov_path=""; 
$sw_ldflags=""; 
$sw_compileflags=""; 
$WRFCHEM = 0 ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any

# Transfer arguments to local variables

while ( substr( $ARGV[0], 0, 1 ) eq "-" ) {
  if ( substr( $ARGV[0], 1, 5 ) eq "perl=" ) {
    $sw_perl_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" ) {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 6 ) eq "phdf5=" ) {
    $sw_phdf5_path = substr( $ARGV[0], 7 ) ;
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

# parse the configure.defaults_new file

$validresponse = 0 ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

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
  
  if ( $sw_netcdf_path ) { 
    $_ =~ s:CONFIGURE_NETCDF_PATH:$sw_netcdf_path:g ;
    $_ =~ s:CONFIGURE_WRFIO_NF:wrfio_nf:g ;
    $_ =~ s:CONFIGURE_NETCDF_FLAG:-DNETCDF: ;
    $_ =~ s:CONFIGURE_NETCDF_LIB:-L$sw_netcdf_path/lib -lnetcdf: ;
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

print "end of configure preamble\n";
print "@machopts1\n";

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
printf "These are the default options for this platform:\n" ;
printf "------------------------------------------------------------------------\n" ;
#foreach $f ( @machopts2 )
#{
#  if ( substr( $f , 0 , 8 ) eq "external" ) { last ; }
#  print $f ;
#}
printf "------------------------------------------------------------------------\n" ;
printf "These will be written to the file configure.wrf here in the top-level\n" ;
printf "directory.  If you wish to change settings, please edit that file.\n" ;
printf "If you wish to change the default options, edit the file:\n\n" ;
printf "     arch/configure.defaults_new\n" ;
printf "\n" ;

open CONFIGURE_WRF, "> build/configure.wrf" 
  or die "cannot append build/configure.wrf" ;

print CONFIGURE_WRF "# configure.wrf\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# This file was automatically generated by the configure script in the\n";
print CONFIGURE_WRF "# top level directory. You may make changes to the settings in this\n";
print CONFIGURE_WRF "# file but be aware they will be overwritten each time you run configure.\n";
print CONFIGURE_WRF "# Ordinarily, it is necessary to run configure once, when the code is\n";
print CONFIGURE_WRF "# first installed.\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# To permanently change options, change the settings for your platform\n";
print CONFIGURE_WRF "# in the file arch/configure.defaults then rerun configure.\n";
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

if($sw_os =~ m/darwin/i) {
   chomp $optchoice;

   if($optchoice != 1) {
     open ARCH_POSTAMBLE, "< arch/postamble_wrfvar.mac_g4"
       or die "cannot open arch/postamble_wrfvar.mac_g4" ;
   } else {
     open ARCH_POSTAMBLE, "< arch/postamble_new"
       or die "cannot open arch/postamble_new" ;
   }
} elsif(($sw_os =~ m/crayx1/i) || ($sw_os =~ m/cray1e/i) || ($sw_os =~ m/UNICOS/i)) {
  open ARCH_POSTAMBLE, "< arch/postamble_wrfvar.cray1e"
    or die "cannot open arch/postamble_wrfvar.cray1e" ;
} else {
  open ARCH_POSTAMBLE, "< arch/postamble_new" 
    or die "cannot open arch/postamble_new" ;
}

while ( <ARCH_POSTAMBLE> ) { 
  print CONFIGURE_WRF
}
close ARCH_POSTAMBLE ;
close CONFIGURE_WRF ;

printf "Configuration successful for $sw_os. To build the model type compile . \n" ;
printf "------------------------------------------------------------------------\n" ;


