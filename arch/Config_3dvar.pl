#!/usr/bin/perl
#
# Configuration script for 3DVAR prototype code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#

$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any

while ( substr( $ARGV[0], 0, 1 ) eq "-" )
 {
  if ( substr( $ARGV[0], 1, 5 ) eq "perl=" )
  {
    $sw_perl_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" )
  {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" )
  {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" )
  {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  shift @ARGV ;
 }


# parse the configure.wrf file

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults_3dvar" 
      or die "Cannot open ./arch/configure.defaults_3dvar for reading" ;
while ( <CONFIGURE_DEFAULTS> )
{
    @inline = ( @inline, $_ ) ;
}
close CONFIGURE_DEFAULTS ;

$validresponse = 0 ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  foreach $_ ( @inline )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 ) )
    {
      $optstr[$opt] = substr($_,6) ;
      $optstr[$opt] =~ s/^[ 	]*// ;
      if ( substr( $optstr[$opt], 0,4 ) ne "NULL" )
      {
        printf "  %2d.  %s",$opt,$optstr[$opt] ;
        $opt++ ;
      }
    }
  }

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response >= 1 && $response <= $opt ) 
  { $validresponse = 1 ; }
  else
  { printf("\nInvalid response (%d)\n",$response);}
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;

$latchon = 0 ;
foreach $_ ( @inline )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    $latchon = 0 ;
  }
  if ( $latchon == 1 )
  {
    $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    if ( $sw_netcdf_path ) 
      { $_ =~ s/CONFIGURE_WRFIO_NF/wrfio_nf/g ;
	$_ =~ s:CONFIGURE_NETCDF_FLAG:-DNETCDF: ;
	$_ =~ s:CONFIGURE_NETCDF_LIB_PATH:-L../external/io_netcdf -lwrfio_nf -L$sw_netcdf_path/lib -lnetcdf: ;
	 }
    else                   
      { $_ =~ s/CONFIGURE_WRFIO_NF//g ;
	$_ =~ s:CONFIGURE_NETCDF_FLAG::g ;
	$_ =~ s:CONFIGURE_NETCDF_LIB_PATH::g ;
	 }
    @machopts = ( @machopts, $_ ) ;
  }
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0 )
  {
    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    if ( $x eq $optstr[$optchoice] )
    {
      $latchon = 1 ;
    }
  }
}

printf "\nYou have chosen: %s",$optstr[$optchoice] ;
printf "These are the default options for this platform:\n" ;
printf "------------------------------------------------------------------------\n" ;
foreach $f ( @machopts )
{
  if ( substr( $f , 0 , 8 ) eq "external" ) { last ; }
  print $f ;
}
printf "------------------------------------------------------------------------\n" ;
printf "These will be written to the file configure.wrf here in the top-level\n" ;
printf "directory.  If you wish to change settings, please edit that file.\n" ;
printf "If you wish to change the default options, edit the file:\n\n" ;
printf "     arch/configure.defaults_3dvar\n" ;
printf "\n" ;

open CONFIGURE_3DVAR, "> configure.wrf" or die "cannot append configure.wrf" ;
open ARCH_PREAMBLE, "< arch/preamble_3dvar" or die "cannot open arch/preamble_3dvar" ;
while ( <ARCH_PREAMBLE> ) { print CONFIGURE_3DVAR } ;
close ARCH_PREAMBLE ;
printf CONFIGURE_3DVAR "# Settings for %s", $optstr[$optchoice] ;
print CONFIGURE_3DVAR @machopts  ;

if($sw_os =~ m/darwin/i)
{
   chomp $optchoice;

   if($optchoice != 1)
   {
      open ARCH_POSTAMBLE, "< arch/postamble_3dvar.mac_g4"
      or die "cannot open arch/postamble_3dvar.mac_g4" ;
   }
   else
   {
      open ARCH_POSTAMBLE, "< arch/postamble_3dvar"
      or die "cannot open arch/postamble_3dvar" ;
   }
}
elsif($sw_os =~ m/crayx1/i)
{
   open ARCH_POSTAMBLE, "< arch/postamble_3dvar.crayx1"
   or die "cannot open arch/postamble_3dvar.crayx1" ;
}
else
{
   open ARCH_POSTAMBLE, "< arch/postamble_3dvar" or die "cannot open arch/postamble_3dvar" ;
}

while ( <ARCH_POSTAMBLE> ) { print CONFIGURE_3DVAR } ;
close ARCH_POSTAMBLE ;
close CONFIGURE_3DVAR ;

printf "Configuration successful. To build the model type compile . \n" ;
printf "------------------------------------------------------------------------\n" ;



