!
Program test_coef
  ! Description:
  ! Main code to test functionalities for coefficients (open/read/write)
  ! Read from standard input the identification triplet platform, satid, instrument
  ! Open the ASCII coef file
  ! Read the coeff file (rttov_readcoeffs without opening)
  ! Writes to rtcoef_1_ascii.out file the same coeff structure in ASCII format
  ! Writes to rtcoef_1_binary.out file the same coeff structure in binary format
  ! Opens rtcoef_1_binary.out file, reads it and writes it in ASCII format
  !   in file rtcoef_1_ascii_2.out
  ! Read from standard input a new identification triplet platform, satid, instrument
  ! Read from standard input a number of channels and a list of channels
  ! Open the corresponding ASCII coef file for the list of channels
  ! Read the coeff file (rttov_readcoeffs without opening and for a list of channels)
  ! Writes to rtcoef_2_ascii.out file the coeff structure in ASCII format
  !
  ! The user can check by editor that the ascii output are correct.
  ! rtcoef_1_ascii.out and rtcoef_1_ascii_2.out should be the same
  !     They can be different from original file for comments
  !     and units of reference profile and profile limits
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
  ! coeffiecnt structure for 2 instruments
  Type( rttov_coef ), Allocatable :: coef(:)          ! coefficients

  ! Instrument triplet for "classical" creation of coefficient filename
  Integer(Kind=jpim) :: instrument(3)
  ! Logical units for input/output
  Integer(Kind=jpim) :: file_id
  Integer(Kind=jpim) :: file_out =20
  ! coefficient index
  Integer(Kind=jpim) :: icoef
  ! error return code for subroutines
  Integer(Kind=jpim) :: errorstatus
  ! if open file is for output
  Logical :: for_output = .True.
  ! character string for file name
  Character(len=128) :: coeffname
  ! number of channels to prosess
  Integer(Kind=jpim)          :: nchannels
  Integer(Kind=jpim), Pointer :: channels(:)

  ! reserve memory for 4 instruments
  Allocate ( coef (4) )

  !- End of header --------------------------------------------------------

  ! Instrument ONE
  icoef = 1
  Write(*,*) 'enter platform, satid, instrument for coef ',icoef
  Read(*,*) instrument
  !instrument = (/ 1, 15, 5 /)

  ! get the file name from instrument triplet
  Call rttov_coeffname (errorstatus, instrument, coeffname)

  Write(*,*) 'coeffname ',coeffname

  ! let the subroutine choose a logical unit for the file
  file_id = 0

  ! open the file in ASCII mode
  Call rttov_opencoeff (errorstatus, coeffname, file_id)

  ! read the coefficients for all channels
  Call rttov_readcoeffs  (errorstatus, coef(icoef), file_id = file_id)
  Call rttov_initcoeffs  (errorstatus, coef(icoef))

  ! Open an output file and store the coefficents in ASCII format
  Open ( unit = file_out, file = 'rtcoef_1_ascii.out' )
  Call Rttov_writecoef (errorstatus, coef(icoef), file_out)
  Close ( unit = file_out )

  ! Open another file and store the coefficents in BINARY format
  file_out = file_out+1
  Call rttov_opencoeff (errorstatus, 'rtcoef_1_binary.out', file_out, for_output, lbinary=.True.)
  Call Rttov_writecoef (errorstatus, coef(icoef), file_out, lbinary=.True.)
  Close ( unit = file_out )

  ! Open the previous binary file, read the coefficients and write in an ASCII
  ! file. This allows us to compare the 2 ASCII files
  ! rtcoef_1_ascii.out and rtcoef_1_ascii_2.out which should be the same
  file_id = 0
  icoef = 2
  Call rttov_opencoeff  (errorstatus, 'rtcoef_1_binary.out', file_id, lbinary=.True.)
  Call rttov_readcoeffs  (errorstatus, coef(icoef), file_id = file_id)
  Call rttov_initcoeffs  (errorstatus, coef(icoef))
  Open ( unit = file_out, file = 'rtcoef_1_ascii_2.out' )
  Call Rttov_writecoef (errorstatus, coef(icoef), file_out)
  Close ( unit = file_out )

  ! Now open a new ASCII coefficient file with a selection of channels
  Write(*,*) 'enter platform, satid, instrument  '
  Read(*,*) instrument
  Write(*,*) 'enter the number of channels '
  Read(*,*) nchannels
  Write(*,*) 'enter a list of channels '
  Allocate ( channels( nchannels ) )
  Read(*,*) channels(:)

  icoef = 3
  ! get the file name from instrument triplet
  Call rttov_coeffname (errorstatus, instrument, coeffname)
  ! let the subroutine choose a logical unit for the file
  file_id = 0


  ! open the file in ASCII mode
  Call rttov_opencoeff (errorstatus, coeffname, file_id)

  ! read the coefficients for the selection of channels
  Call rttov_readcoeffs  (errorstatus, coef(icoef), file_id = file_id, channels = channels)
  Call rttov_initcoeffs  (errorstatus, coef(icoef))

  ! Open an output file and store the coefficents in ASCII format
  Open ( unit = file_out, file = 'rtcoef_2_ascii.out' )
  Call Rttov_writecoef (errorstatus, coef(icoef), file_out)
  Close ( unit = file_out )

  Stop
End Program test_coef
