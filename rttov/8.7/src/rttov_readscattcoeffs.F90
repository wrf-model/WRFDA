!+ routine to read Mie coeficient file
!
Subroutine rttov_readscattcoeffs  (&
      & errorstatus,   &! out
      & coef_rttov,    &! in
      & coef_scatt,    &! out
      & file_id       ) ! in Optional

  !
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
  ! Description:
  ! to initialise Mie look-up table
  !
  ! Method:
  !
  ! Current code owner: saf nwp
  !
  ! History:
  ! version   date        comment
  ! -------   ----        -------
  !   1.0    09/2002      RTTOV7 compatible  (ECMWF)
  !   1.1    05/2003      RTTOV7.3 compatible (ECMWF)
  !   1.2    10/2004      Change stop to return (J Cameron)
  !   1.3    10/2004      Make file_id optional in analogy with rttov_readcoeffs (J Cameron)

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef, &
       & rttov_scatt_coef

  Use rttov_const, Only :   &
       & inst_name           ,&
       & platform_name       ,&
       & errorstatus_info    ,&
       & errorstatus_success ,&
       & errorstatus_fatal

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_findnextsection.interface"
#include "rttov_skipcommentline.interface"
#include "rttov_opencoeff.interface"

  ! subroutine arguments
  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out) :: errorstatus             ! return code

  ! scalar arguments with optional intent(in):
  Integer(Kind=jpim), Optional, Intent (in)  :: file_id       ! file logical unit number

  ! array arguments with intent(in):
  Type( rttov_coef ), Intent (in) :: coef_rttov               ! clear-sky coefficients

  ! array arguments with intent(out):
  Type( rttov_scatt_coef ), Intent (out) :: coef_scatt        ! coefficients

! local variables
  Integer(Kind=jpim)    :: file_lu, io_status, inst, platform, i, j, k
  Logical               :: existence
  Logical               :: file_toclose
  Logical               :: file_open
  Character (len=32)    :: NameOfRoutine = 'rttov_readscattcoeffs' ! name for error message
  Character (len=132)   :: ErrMessage    ! error message string
  Character (len=256)   :: coeffname     ! file name for coefficient file
  Character (len=21)    :: section

  !- End of header --------------------------------------------------------

  errorstatus = errorstatus_success

  If ( Present (file_id) ) Then
     ! Scatt coefficient file has been opened externally
     file_lu = file_id
     file_toclose = .FALSE.

     Inquire( file_lu, OPENED = file_open )
     If ( .NOT. file_open ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "File is not open on unit: ",i5 )' ) file_lu
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
  Else
     ! Open the scatt coefficients internally
     file_lu = 0
     file_toclose = .TRUE.

     platform = coef_rttov % id_platform
     inst     = coef_rttov % id_inst
     coeffname = 'mietable_'//Trim(platform_name(platform))//'_'//Trim(inst_name(inst))//'.dat'

     Inquire( FILE = coeffname, EXIST = existence )
     If ( .NOT. existence ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "Coefficient file, ", a, " not found." )' ) &
             & Trim( coeffname )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     Call rttov_opencoeff (errorstatus, coeffname, file_lu)

     If (errorstatus /= errorstatus_success) Then
        ! rttov_opencoeff will have already reported an error
        errorstatus = errorstatus_fatal
        Return
     Endif
  Endif

  readfile: Do
     Call rttov_findnextsection( file_lu, io_status, section )
     If ( io_status < 0 ) Exit readfile !end-of-file

     ! error message if any problem when reading
     errMessage = 'io status while reading section '//section
     Call rttov_skipcommentline ( file_lu, io_status )
     If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
     Endif

     Select Case( Trim(section) )


     Case( 'IDENTIFICATION' )
        Read(file_lu,*)  ! platform instrument in id
        Read(file_lu,*)  ! platform instrument in letters
        Read(file_lu,*)  ! sensor type [ir,mw,hi]
        Read(file_lu,*)  ! RTTOV compatibility version
        Read(file_lu,*)  ! version
        Read(file_lu,*)  ! creation date

     Case( 'DIMENSIONS')
        Read(file_lu,*)  coef_scatt%mfreqm,  coef_scatt%mtype,  coef_scatt%mtemp,  coef_scatt%mwc
        If (coef_scatt%mtype /= 4) Then
          errorstatus = errorstatus_fatal
          errMessage = 'Wrond number of hydrometeors in parameter file (should ne 4)'
              ! liquid prec., solid prec., ice water, liquid water, water vapour
          Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
          Return
        Endif
        coef_scatt % nhydro = coef_scatt%mtype + 1

     Case( 'FREQUENCIES')
        Allocate (coef_scatt % mie_freq(coef_scatt%mfreqm))
        Read(file_lu,*)  coef_scatt%mie_freq (:)

     Case( 'HYDROMETEOR')
        Read(file_lu,*)

     Case( 'CONVERSIONS')
        Read(file_lu,*) coef_scatt%conv_rain(:)
        Read(file_lu,*) coef_scatt%conv_sp  (:)
        coef_scatt%conv_rain(:) = 1._JPRB/coef_scatt%conv_rain(:)
        coef_scatt%conv_sp  (:) = 1._JPRB/coef_scatt%conv_sp  (:)
        Read(file_lu,*) coef_scatt%conv_liq(:)
        Read(file_lu,*) coef_scatt%conv_ice(:)
        Read(file_lu,*)
        Read(file_lu,*) coef_scatt%offset_temp_rain
        Read(file_lu,*) coef_scatt%offset_temp_sp
        Read(file_lu,*) coef_scatt%offset_temp_liq
        Read(file_lu,*) coef_scatt%offset_temp_ice
        Read(file_lu,*)
        Read(file_lu,*) coef_scatt%scale_water, coef_scatt%offset_water
        coef_scatt%scale_water = 1._JPRB/coef_scatt%scale_water
        coef_scatt%offset_water = - coef_scatt%offset_water
        coef_scatt%from_scale_water = 10**( 1._JPRB / coef_scatt%scale_water )

     Case( 'EXTINCTION')
        Allocate (coef_scatt % ext(coef_scatt%mfreqm, coef_scatt%mtype, coef_scatt%mtemp, coef_scatt%mwc))
        ! The loops should be inverted for better efficiency, but generation program currently not appropriate
        Do i = 1, coef_scatt%mfreqm
          Do j = 1, coef_scatt%mtype
            Do k = 1, coef_scatt%mtemp
              Read(file_lu,'(5(1x,e23.16))') coef_scatt % ext(i,j,k,:)
            Enddo
          Enddo
        Enddo

     Case( 'ALBEDO')
        Allocate (coef_scatt % ssa(coef_scatt%mfreqm, coef_scatt%mtype, coef_scatt%mtemp, coef_scatt%mwc))
        Do i = 1, coef_scatt%mfreqm
          Do j = 1, coef_scatt%mtype
            Do k = 1, coef_scatt%mtemp
              Read(file_lu,'(5(1x,e23.16))') coef_scatt % ssa(i,j,k,:)
            Enddo
          Enddo
        Enddo

     Case( 'ASYMMETRY')
        Allocate (coef_scatt % asp(coef_scatt%mfreqm, coef_scatt%mtype, coef_scatt%mtemp, coef_scatt%mwc))
        Do i = 1, coef_scatt%mfreqm
          Do j = 1, coef_scatt%mtype
            Do k = 1, coef_scatt%mtemp
              Read(file_lu,'(5(1x,e23.16))') coef_scatt % asp(i,j,k,:)
            Enddo
          Enddo
        Enddo

     Case default
        Cycle readfile

     End Select

  End Do readfile

  If ( file_toclose ) Then
     Close ( unit = file_lu )
  Endif

End Subroutine rttov_readscattcoeffs
