!
Program rttov_ascii2bin_coef
  ! Description:
  ! converts an ascii RTTOV coefficient file to binary format
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_coeffname.interface"
#include "rttov_opencoeff.interface"
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"
#include "rttov_writecoef.interface"

  ! Local variables
  !-------------------
  ! coeffiecnt structure
  Type( rttov_coef ) :: coef        ! coefficients

  ! Instrument triplet for "classical" creation of coefficient filename
  Integer(Kind=jpim) :: instrument(3)
  ! Logical units for input/output
  Integer(Kind=jpim) :: file_id
  ! error return code for subroutines
  Integer(Kind=jpim) :: errorstatus
  ! character string for file name
  Character(len=128) :: coeffname
  ! number of channels to prosess
  Integer(Kind=jpim) :: nchannels
  Integer(Kind=jpim), Pointer :: channels(:)

  !- End of header --------------------------------------------------------

  ! let the subroutine choose a logical unit for the file
  file_id = 0

  Write(*,*) 'enter platform, satid, instrument'
  Read(*,*) instrument
  Write(*,*) 'enter the number of channels (0 for all) '
  Read(*,*) nchannels
  If( nchannels /= 0 ) Then
     Write(*,*) 'enter a list of channels '
     Allocate ( channels( nchannels ) )
     Read(*,*) channels(:)
  Endif

  ! get the file name from instrument triplet
  Call rttov_coeffname ( errorstatus, instrument, coeffname)
  Write(*,*) 'ASCII coefficient file ',coeffname
  ! open the file in ASCII mode
  Call rttov_opencoeff (errorstatus, coeffname, file_id)
  ! read the coefficients for all channels
  If ( nchannels /= 0 ) Then
     Call rttov_readcoeffs  (errorstatus, coef, file_id = file_id, channels = channels)
     Call rttov_initcoeffs  (errorstatus, coef)
  Else
     Call rttov_readcoeffs  (errorstatus, coef, file_id = file_id)
     Call rttov_initcoeffs  (errorstatus, coef)
  Endif
  Close ( unit = file_id )


  ! get the binary file name from instrument triplet
  ! open, write and close
  Call rttov_coeffname ( errorstatus, instrument, coeffname, lbinary=.True.)
  Write(*,*) 'Binary coefficient file ',coeffname
  Call rttov_opencoeff (errorstatus, coeffname, file_id, for_output=.True., lbinary=.True.)
  Call Rttov_writecoef (errorstatus, coef,      file_id, lbinary=.True.)
  Close ( unit = file_id )


  Stop
End Program rttov_ascii2bin_coef
