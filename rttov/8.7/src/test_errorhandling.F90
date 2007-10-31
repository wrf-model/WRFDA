!
Program test_errorhandling
  !
  ! Description:
  ! Main code to test functionalities of RTTOV error handling
  ! Start with default values for error logical unit and verbosity
  ! Send messages for all error levels
  ! Change the verboisty level from Max to Min and always send the same
  ! verbosity messages
  ! The user should verify the correct effects of the calls
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
  !  1.0       10/01/03  Original (P Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  ! Imported Type Definitions:
  Use rttov_const, Only :   &
       & errorstatus_info    ,&
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & errorstatus_warning ,&
       & default_err_unit

  Use parkind1, Only : jpim     ,jprb
  Implicit None
#include "rttov_errorhandling.interface"
#include "rttov_errorreport.interface"

  !- Local variables
  Integer(Kind=jpim) :: ios
  Integer(Kind=jpim) :: Err_Unit       ! Logical error unit
  Integer(Kind=jpim) :: verbosity_level
  Character (len=80) :: errMessage
  Character (len=18) :: NameOfRoutine = 'test_errorhandling'
  !- End of header --------------------------------------------------------

  ! 1 Default error unit
  !---------------------
  Write(*,*) 'Test with default error logical unit number ', default_err_unit
  Write(*,*) ' control results on this unit (should be on screen) '
  Write(*,*) 'If no message appears on screen then change the default_err_unit'
  Write(*,*) ' in rttov_const.f90 according to your system'

  Err_unit = -1

  ! 1.1 default verbosity level
  !----------------------------
  Write(*,*) ' '
  Write(*,*) 'test with default verbosity level '
  verbosity_level = -1

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 1.2 verbosity level 0
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 0 (no output)'
  verbosity_level = 0

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see nothing '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 1.3 verbosity level 1
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 1 (Fatal only)'
  verbosity_level = 1

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see Fatal message '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 1.4 verbosity level 2
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 2 (Warning)'
  verbosity_level = 2

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see Warning and Fatal '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 1.5 verbosity level 3
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 3 (Information)'
  verbosity_level = 3

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see all messages '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)

  ! 2 error unit number 10
  !-----------------------
  Write(*,*) ' '
  Write(*,*) ' '
  Write(*,*) ' '
  Write(*,*) 'test with error logical unit number 10'
  Write(*,*) 'on file test_errorhandling.lst'
  Write(*,*) 'control results on this unit '
  Err_unit = 10
  Open (unit=Err_unit, file='test_errorhandling.lst',iostat=ios)
  If(ios /= 0 ) Then
     Write(*,*) 'error opening output file test_errorhandling.lst'
     Write(*,*) 'iostatus is ',ios
     Stop
  End If

  ! 2.1 default verbosity level
  !----------------------------
  Write(*,*) ' '
  Write(*,*) 'test with default verbosity level '
  Write(Err_unit,*) 'test with default verbosity level '
  verbosity_level = -1

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 2.2 verbosity level 0
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 0 (no output)'
  Write(Err_unit,*) 'test with verbosity level 0 (no output)'
  verbosity_level = 0

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see nothing '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 2.3 verbosity level 1
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 1 (Fatal only)'
  Write(Err_unit,*) 'test with verbosity level 1 (Fatal only)'
  verbosity_level = 1

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see Fatal message '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 2.4 verbosity level 2
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 2 (Warning)'
  Write(Err_unit,*) 'test with verbosity level 2 (Warning)'
  verbosity_level = 2

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see Warning and Fatal '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)


  ! 2.5 verbosity level 3
  !----------------------
  Write(*,*) ' '
  Write(*,*) 'test with verbosity level 3 (Information)'
  Write(Err_unit,*) 'test with verbosity level 3 (Information)'
  verbosity_level = 3

  call rttov_errorhandling ( Err_unit, verbosity_level)

  Write(*,*) 'now the code will output messages for all error levels '
  Write(*,*) 'you should see all messages '
  errMessage="test"
  Call Rttov_ErrorReport (errorstatus_info   , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_success, errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_fatal  , errMessage, NameOfRoutine)
  Call Rttov_ErrorReport (errorstatus_warning, errMessage, NameOfRoutine)

  Stop
End Program Test_errorhandling
