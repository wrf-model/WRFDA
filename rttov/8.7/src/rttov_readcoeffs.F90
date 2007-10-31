!
Subroutine rttov_readcoeffs  (&
      & errorstatus,   &! out
      & coef,          &! out
      & instrument,    &! in Optional
      & kmyproc,       &! in Optional
      & kioproc,       &! in Optional
      & file_id,       &! in Optional
      & channels      ) ! in Optional
  ! Description:
  !
  ! Read an ASCII or binary coefficient file and allocate coeff structure
  !   arrays according to the optional list of channels.
  !!!!!!!
  ! This version can run in a distibuted mode :
  ! IO PE will read the data which are broadcasted to the other pes.
  ! Be careful any 'reading' modifications in rttov_readcoeffs_ascii or
  ! rttov_readcoeffs_binary may have to be reported in rttov_distribcoeffs
  !!!!!!!
  ! The optional arguments instrument and file_id determines whether the
  !   file is already opened or not
  ! if  "instrument" is present the routine will try to open
  !         the corresponding binary file  (extension .bin) in read only mode.
  !         If it fails then it tries to open the ASCII file (extension .dat)
  !         File is closed before return.
  ! if  "instrument" is not present but file_id is present the  routine will
  !  access to the coefficient file already opened with the logical unit file_id.
  !  The ASCII/binary test is performed by reading the first characters, binary
  !  files will always start by "%RTTOV_COEFF" characters. An ASCII file cannot
  !  contain such a string at the beginning of the file because it will be
  !  considered as a section name which will not be recognised. File is NOT
  !  closed on return.
  ! The user can provide an optional list of channels in "channels" argument
  !  array to reduce the output coefficient structure to this list. This
  ! can be important for reducing the memory allocation required when running
  ! with advanced IR sounders (e.g. AIRS or IASI). If the user
  !  wants all channels the "channels" argument shall not be present.
  !
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
  !  1.1       02/01/2003  A few comments added (R Saunders)
  !  1.2       03/05/2004  Add specific RTTOV8 CO2 variable (P. Brunel)
  !  1.3       02/06/2004  Change tests on id_comp_lvl == 7 by tests on fmv_model_ver (P. Brunel)
  !  1.4       08/09/2004  Change ascii/binary file test to use Inquire (J. Cameron)
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
       & version             ,&
       & release             ,&
       & minor_version       ,&
       & errorstatus_info    ,&
       & errorstatus_success ,&
       & errorstatus_fatal


  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_coeffname.interface"
#include "rttov_opencoeff.interface"
#include "rttov_errorreport.interface"
#include "rttov_readcoeffs_binary.interface"
#include "rttov_readcoeffs_ascii.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Optional, Intent(in) :: kmyproc  ! logical processor id
  Integer(Kind=jpim), Optional, Intent(in) :: kioproc  ! procs dedicated for io
  Integer(Kind=jpim), Optional, Intent (in) :: instrument(3)  ! (platform, satellite identification, instrument) number
  Integer(Kind=jpim), Optional, Intent (in) :: file_id      ! file logical unit number
  Integer(Kind=jpim), Optional, Intent (in) :: channels(:)      ! list of channels to extract


  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out) :: errorstatus       ! return code
  Type( rttov_coef ), Intent (out) :: coef   ! coefficients

  ! Local Scalars:
  Logical           :: file_toclose
  Logical           :: file_binary
  Logical           :: existence
  Logical,save      :: first=.true.
  Integer(Kind=jpim)   :: file_lu
  Integer(Kind=jpim)   :: imyproc,iioproc

  Character (len=256):: coeffname  ! file name for coefficient file
  Character (len=20) :: file_form
  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_readcoeffs'


  !- End of header --------------------------------------------------------

  ! 0 Initialise variables
  !---------------------------------------------
  errorstatus     = errorstatus_success

  If ( .Not. Present (kmyproc) ) Then
     imyproc = 1
  Else
     imyproc = kmyproc
  Endif

  If ( .Not. Present (kioproc) ) Then
     iioproc = 1
  Else
     iioproc = kioproc
  Endif

  If (imyproc == iioproc ) then
     file_toclose = .False.
     file_binary  = .False.

     If ( .Not. Present (file_id) ) Then
        file_lu = 0
     Else
        file_lu = file_id
     Endif

     If( first ) Then
        Write( errMessage, '( "RTTOV library version ",i2,1x,i1,".",i1 )' )&
              & version, release, minor_version
        Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)
        first = .false.
     End If

     ! 1 Beginning of coefficient opening sequence
     !---------------------------------------------

     ! test arguments instrument and file_id to decide whether to open
     ! the file or not.
     If ( Present (instrument ) ) Then

        ! Binary filename
        Call rttov_coeffname ( errorstatus, instrument, coeffname, lbinary = .True.  )
        If ( errorstatus /= errorstatus_success ) Then
           Return
        Endif

        ! test existence of binary file
        Inquire( FILE = coeffname, EXIST = existence )
        If ( existence ) Then
           Write( errMessage, '( "open binary coefficient file ",a )' )&
           & Trim(coeffname)
           Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)
           ! Open binary file
           Call rttov_opencoeff ( errorstatus, coeffname, file_lu, lbinary = .True. )
           If ( errorstatus /= errorstatus_success ) Then
              ! Binary open fails, try ASCII access
              ! ASCII filename
              Call rttov_coeffname ( errorstatus, instrument, coeffname )
              If ( errorstatus /= errorstatus_success ) Then
                 Return
              Endif
              ! Open ASCII file
              Call rttov_opencoeff ( errorstatus, coeffname, file_lu)
              If ( errorstatus /= errorstatus_success ) Then
                 Return
              Endif
           Endif

        Else
           ! Try to open ASCII format
           ! ASCII filename
           Call rttov_coeffname ( errorstatus, instrument, coeffname )
           If ( errorstatus /= errorstatus_success ) Then
              Return
           Endif
           Write( errMessage, '( "open ASCII coefficient file ",a )' )&
           & Trim(coeffname)
           Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)
           ! Open ASCII file
           Call rttov_opencoeff ( errorstatus, coeffname, file_lu)
           If ( errorstatus /= errorstatus_success ) Then
              Return
           Endif

        End If
        file_toclose = .True.

     Else
        ! instrument argument missing
        If ( .Not. Present (file_id) ) Then
           ! file_id argument missing
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "instrument and file_id missing arguments." )' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Return
        Endif
     Endif

     ! Find out if the file is ascii or binary
     ! The inquire should work even if the file was opened externally
     INQUIRE(file_lu,FORM=file_form)
     IF ( file_form == 'FORMATTED' ) THEN
       file_binary = .FALSE.
     ELSEIF ( file_form == 'UNFORMATTED' ) THEN
       file_binary = .TRUE.
     ELSE
       errorstatus = errorstatus_fatal
       Write( errMessage, '(a)' ) 'Unknown file format: '//file_form
       CALL Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
       RETURN
     ENDIF

     ! End of coefficient opening sequence
     !-------------------------------------
  Endif


  ! 2 initialize coef structure for all single types
  !---------------------------------------------
  coef % id_platform       = 0
  coef % id_sat            = 0
  coef % id_inst           = 0
  coef % id_sensor         = 0
  coef % id_comp_lvl       = 0
  coef % id_creation_date  = (/ 0, 0, 0 /)
  coef % id_creation       = 'xxxx'
  coef % id_Common_name    = 'xxxx'
  coef % fmv_model_def     = 'xxxx'
  coef % fmv_model_ver     = 0
  coef % fmv_chn           = 0
  coef % fmv_gas           = 0
  coef % nmixed            = 0
  coef % nwater            = 0
  coef % nozone            = 0
  coef % nwvcont           = 0
  coef % nco2              = 0
  coef % nn2o              = 0
  coef % nco               = 0
  coef % nch4              = 0
  coef % nlevels           = 0
  coef % fc_speedl         = 0
  coef % fc_planck_c1      = 0
  coef % fc_planck_c2      = 0
  coef % fc_sat_height     = 0
  coef % fastem_ver        = 0
  coef % fastem_coef_nb    = 0
  coef % ssirem_ver        = 0

  Nullify(coef % gaz_units)
  Nullify(coef % mixedgas)
  Nullify(coef % watervapour)
  Nullify(coef % ozone)
  Nullify(coef % wvcont)
  Nullify(coef % co2)
  Nullify(coef % n2o)
  Nullify(coef % co)
  Nullify(coef % ch4)
  Nullify(coef % fmv_gas_id)
  Nullify(coef % fmv_gas_pos)
  Nullify(coef % fmv_var)
  Nullify(coef % fmv_lvl)
  Nullify(coef % ff_ori_chn)
  Nullify(coef % ff_val_chn)
  Nullify(coef % ff_cwn)
  Nullify(coef % ff_bco)
  Nullify(coef % ff_bcs)
  Nullify(coef % ff_gam)
  Nullify(coef % fastem_polar)
  Nullify(coef % ssirem_chn)
  Nullify(coef % ssirem_a0)
  Nullify(coef % ssirem_a1)
  Nullify(coef % ssirem_a2)
  Nullify(coef % ssirem_xzn1)
  Nullify(coef % ssirem_xzn2)
  Nullify(coef % fastem_coef)
  Nullify(coef % ref_prfl_t)
  Nullify(coef % ref_prfl_mr)
  Nullify(coef % lim_prfl_p)
  Nullify(coef % lim_prfl_tmax)
  Nullify(coef % lim_prfl_tmin)
  Nullify(coef % lim_prfl_gmax)
  Nullify(coef % lim_prfl_gmin)
  Nullify(coef % ref_prfl_p)

  If (imyproc == iioproc ) then
    ! 3 Read binary file
    !-------------------
    If( file_binary ) Then
       If( Present ( channels ) ) Then
          Call rttov_readcoeffs_binary  (&
                & errorstatus,   &! out
                & coef,          &! inout
                & file_lu,       &! in
                & channels = channels ) ! in Optional
       Else
          Call rttov_readcoeffs_binary  (&
                & errorstatus,   &! out
                & coef,          &! inout
                & file_lu       ) ! in
       Endif

    ! 4 If no Binary file then read ASCII file
    !-----------------------------------------
    Else
       If( Present ( channels ) ) Then
          Call rttov_readcoeffs_ascii  (&
                & errorstatus,   &! out
                & coef,          &! inout
                & file_lu,       &! in
                & channels = channels ) ! in Optional
       Else
          Call rttov_readcoeffs_ascii  (&
                & errorstatus,   &! out
                & coef,          &! inout
                & file_lu       ) ! in
       Endif

    Endif

    If( errorstatus /= errorstatus_success ) then
       ! Do not add any fatal/warning messages
       Return
    End If

    If( file_toclose ) Then
       Close ( unit = file_lu )
    Endif

  Write( errMessage, '( "fast model version compatibility ",i2 )' )coef % fmv_model_ver
  Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)
  Endif


End Subroutine rttov_readcoeffs
