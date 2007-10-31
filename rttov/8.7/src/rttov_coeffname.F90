!
Subroutine rttov_coeffname (errorstatus, instrument, coeffname, lbinary)
  ! Description:
  !
  ! Returns the file name of a coefficent file for the instrument given
  ! in argument.
  ! Instrument refers to an array of 3 integers defining the satellite platform,
  ! satellite number and instrument number.
  ! The optional logical argument lbinary determines the filename extension
  ! and expected data storage. If lbinary is false or not present the file
  ! is assumed as a sequential formatted, in other case it is sequential
  ! unformatted. The default option is ASCII file.
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
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & nplatforms          ,&
       & ninst               ,&
       & inst_name           ,&
       & platform_name

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Intent (in) :: instrument(3)  ! (platform, sat_id, inst) numbers
  Logical, Optional, Intent (in) :: lbinary         ! if binary file wanted

  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out)       :: errorstatus! return code
  Character (*), Intent (out) :: coeffname  ! filename of the coefficient file



  ! Local Scalars:
  Integer(Kind=jpim) :: platform
  Integer(Kind=jpim) :: sat_id
  Integer(Kind=jpim) :: inst
  Character (len=4)  :: ext         ! filename extension
  Character (len=2)  :: ch_sat_id
  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_coeffname '
  !- End of header --------------------------------------------------------

  coeffname  = 'no_name'
  errorstatus       = errorstatus_success

  ! Consider lbinary option to create the extension character string
  If(Present(lbinary)) Then
     If(lbinary) Then
        ext = '.bin'
     Else
        ext = '.dat'
     Endif
  Else
     ext = '.dat'
  Endif

  ! expand instrument triplet
  platform = instrument(1)
  sat_id   = instrument(2)
  inst     = instrument(3)

  ! Test sat_id and convert to string
  If( sat_id < 10 .And. sat_id > 0 ) Then
     ! one digit
     Write(ch_sat_id,'(i1)') sat_id
  Else If( sat_id >= 10 .And. sat_id < 99 ) Then
     ! two digits
     Write(ch_sat_id,'(i2)') sat_id
  Else
     ! ERROR and exit
     Write(errMessage,"('invalid sat_id: ', i4)") sat_id
     errorstatus = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  Endif

  ! Test platform number
  If( platform <= 0 .Or. platform > nplatforms ) Then
     ! ERROR and exit
     Write(errMessage,"('invalid platform number: ', i4)") platform
     errorstatus = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  Endif

  ! Test instrument number  (0 is HIRS)
  If( inst < 0 .Or. inst > ninst) Then
     ! ERROR and exit
     Write(errMessage,"('invalid instrument number: ', i4)") inst
     errorstatus = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  Endif

  ! create the file name "rtcoef_platform_satellite_inst.dat"
  coeffname = 'rtcoef_'                // &
        & Trim(platform_name(platform)) // &
        & '_'                           // &
        & Trim(ch_sat_id)               // &
        & '_'                           // &
        & Trim(inst_name(inst))         // &
        & ext
 ! write(0,'("RTTOV_COEFFNAME: ",A)')coeffname



End Subroutine rttov_coeffname
