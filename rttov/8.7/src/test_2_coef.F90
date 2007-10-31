!
Program test_2_coef
  ! Description:
  ! Tests the reading of a coefficent file for all channels or
  ! for a selection.
  ! Input are:
  !    the triplet for identification (platform, satellite, instrument)
  !    flag for acees to binary or ASCII coefficent file
  !    number of channels (0= all   -n for first n channels)
  !    channels selection (if input number of channels is >0)
  !
  ! The program invites you to check the memory allocation and
  ! to press return to exit
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

  ! Imported Parameters:
  Use rttov_const, Only :   &
       & errorstatus_success

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_coeffname.interface"
#include "rttov_opencoeff.interface"
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"
#include "rttov_errorreport.interface"


  ! Local variables
  !-------------------
  Type( rttov_coef ) :: coef         ! coefficients

  ! Instrument triplet for "classical" creation of coefficient filename
  Integer(Kind=jpim) :: instrument(3)
  ! Logical units for input/output
  Integer(Kind=jpim) :: file_id
  ! error return code for subroutines
  Integer(Kind=jpim) :: errorstatus
  ! character string for file name
  Character(len=128) :: coeffname
  !ascii / binary option
  Integer(Kind=jpim) :: iascii
  Logical :: lbinary
  ! number of channels to prosess
  Integer(Kind=jpim) :: nchannels
  Integer(Kind=jpim), Pointer :: channels(:)
  Integer(Kind=jpim) :: i
!- End of header --------------------------------------------------------



  ! Now open a new ASCII coefficient file with a selection of channels
  Write(*,*) 'enter platform, satid, instrument  '
  Read(*,*) instrument
  Write(*,*) 'enter ascii or binary (0/1)  '
  Read(*,*) iascii
  Write(*,*) 'enter the number of channels (0 for all, -n for first n channels) '
  Read(*,*) nchannels
  If( nchannels > 0 ) Then
     Write(*,*) 'enter a list of channels '
     Allocate ( channels( nchannels ) )
     Read(*,*) channels(:)
  ElseIf( nchannels < 0 ) Then
     nchannels = -nchannels
     Allocate ( channels( nchannels ) )
     Do i = 1, nchannels
        channels(i) = i
     End Do
  Endif
  If(iascii == 0) Then
     lbinary = .False.
  Else
     lbinary = .True.
  Endif

  ! get the file name from instrument triplet
  Call rttov_coeffname (errorstatus, instrument, coeffname, lbinary)
  ! let the subroutine choose a logical unit for the file
  file_id = 0

  If( errorstatus == errorstatus_success ) Then
     ! open the file in ASCII mode
     Call rttov_opencoeff (errorstatus, coeffname, file_id, lbinary=lbinary)

     If( errorstatus == errorstatus_success ) Then
        ! read the coefficients for the selection of channels
        Call Rttov_errorreport( errorstatus_success, 'start reading', 'main')
        If ( nchannels /= 0 ) Then
           Call rttov_readcoeffs  (errorstatus, coef, file_id = file_id, channels = channels)
           Call rttov_initcoeffs  (errorstatus, coef)
        Else
           Call rttov_readcoeffs  (errorstatus, coef, file_id = file_id)
           Call rttov_initcoeffs  (errorstatus, coef)
        Endif
        Call Rttov_errorreport( errorstatus, 'end reading', 'main')
     End If

     If( errorstatus == errorstatus_success ) Then
        Write(*,*) 'you can check the memory allocation, then press return '
        Read(*,*)
     End If
  End If

  Stop
End Program test_2_coef
