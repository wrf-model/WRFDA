!
Subroutine rttov_readcoeffs_binary  (&
      & errorstatus,   &! out
      & coef,          &! inout
      & file_lu,       &! in
      & channels      ) ! in Optional
  ! Description:
  !
  ! Read an binary coefficient file and fills coeff structure
  !   arrays according to the optional list of channels.
  !
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
  !  1.2       24/01/2003  add tests on all read statements (P Brunel)
  !                        one record per channel for coefficients in binary format
  !                        New header to allow checking R4<->R8
  !  1.3       06/05/2003  Remove "optional" attribute of argument file_lu (P Brunel)
  !  1.4       02/06/2004  New format for FMV section with RTTOV8  (P. Brunel)
  !  1.5       15/06/2004  Corrected array dimension for coef % fmv_gas_pos (R Saunders)
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
       & rttov_magic_string  ,&
       & rttov_magic_number  ,&
       & version_compatible_min  ,&
       & version_compatible_max  ,&
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & gas_id_mixed        ,&
       & gas_id_watervapour  ,&
       & gas_id_ozone        ,&
       & gas_id_wvcont       ,&
       & gas_id_co2          ,&
       & gas_id_n2o          ,&
       & gas_id_co           ,&
       & gas_id_ch4          ,&
       & ngases_max


  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Intent (in)           :: file_lu     ! file logical unit number
  Integer(Kind=jpim), Optional, Intent (in) :: channels(:) ! list of channels to extract

  ! scalar arguments with intent(inout):
  Type( rttov_coef ), Intent (inout) :: coef    ! coefficients

  ! scalar arguments with intent(out):
  Integer(Kind=jpim), Intent (out) :: errorstatus          ! return code

  ! Local Scalars:
  Integer(Kind=jpim)           :: file_channels
  Logical                      :: all_channels
  Integer(Kind=jpim)           :: io_status
  Integer(Kind=jpim)           :: alloc_status(11)
  Integer(Kind=jpim)           :: n
  Integer(Kind=jpim)           :: chn
  Integer(Kind=jpim)           :: i

  ! pointers for generic inputs
  Real(Kind=jprb),    Pointer :: values0(:)
  Real(Kind=jprb),    Pointer :: values1(:)
  Real(Kind=jprb),    Pointer :: values2(:)
  Real(Kind=jprb),    Pointer :: values3(:)
  Real(Kind=jprb),    Pointer :: values4(:)
  Integer(Kind=jpim), Pointer :: ivalues0(:)
  Integer(Kind=jpim), Pointer :: ivalues1(:)

  Character (len=16) :: bin_check_string
  Real(Kind=jprb)    :: bin_check_number
  Real(Kind=jprb)    :: bin_check_value
  Character (len=80) :: errMessage
  Character (len=23) :: NameOfRoutine = 'rttov_readcoeffs_binary'

  !- End of header --------------------------------------------------------

  ! 0 Initialise variables
  !---------------------------------------------
  errorstatus     = errorstatus_success
  alloc_status(:) = 0

  ! test presence of channels argument
  If( Present ( channels ) ) Then
     all_channels = .False.
  Else
     all_channels = .True.
  Endif


  ! 3 Read binary file
  !-------------------
  ! Binary file
  Read(file_lu, iostat = io_status ) bin_check_string, bin_check_number
  If(io_status /= 0) Then
     errMessage = 'io status while reading header'
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  ! Verification of header string
  if ( bin_check_string /= rttov_magic_string ) Then
     errMessage = 'Wrong header string in file'
     errorstatus = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If


  ! Verification of single/double precision using a 5 digit number
  ! with exponent 12, which is always Ok for single precision
  bin_check_value = 1._JPRB - abs ( bin_check_number - rttov_magic_number )
  if ( bin_check_value > 1.01_JPRB .or. bin_check_value < 0.99_JPRB ) Then
     errMessage = 'File created with a different real precision (R4<->R8)'
     errorstatus = errorstatus_fatal
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  errMessage = 'io status while reading IDENTIFICATION'
  Read(file_lu, iostat = io_status )&
        & coef % id_platform, &
        & coef % id_sat,      &
        & coef % id_inst,     &
        & coef % id_sensor
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif
  Read(file_lu, iostat = io_status )&
        & coef % id_comp_lvl,      &
        & coef % id_creation_date, &
        & coef % id_creation,      &
        & coef % id_Common_name
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  errMessage = 'io status while reading FAST_MODEL_VARIABLES'
  If( coef % id_comp_lvl <= 7 ) then
    Read(file_lu, iostat = io_status )&
       & coef % fmv_model_def, &
       & coef % fmv_chn,       &
       & coef % fmv_gas
    coef % fmv_model_ver = 7
  Else
    Read(file_lu, iostat = io_status )&
       & coef % fmv_model_def, &
       & coef % fmv_model_ver, &
       & coef % fmv_chn,       &
       & coef % fmv_gas
  Endif

  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  ! Error if the compatibility version of the coefficient file
  ! is not in the range defined by the constant module
  If(    coef % id_comp_lvl < version_compatible_min .Or.&
        & coef % id_comp_lvl > version_compatible_max ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage,&
           & '( "Version of coefficient file is incompatible with RTTOV library")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  Endif

  ! Take care of the user list of channels
  ! file_channels store the number of channels in the file
  ! coef % fmv_chn is the number of channels that the user requests
  file_channels = coef % fmv_chn
  If( .Not. all_channels ) Then
     coef % fmv_chn = Size( channels )
  Endif

  Allocate ( coef % fmv_gas_id ( coef % fmv_gas ), stat=alloc_status(1))
  Allocate ( coef % fmv_gas_pos( ngases_max ), stat=alloc_status(2))
  Allocate ( coef % fmv_var ( coef % fmv_gas ), stat=alloc_status(3))
  Allocate ( coef % fmv_lvl ( coef % fmv_gas ), stat=alloc_status(4))
  Allocate ( coef % ff_ori_chn( coef % fmv_chn ), stat=alloc_status(5) )
  Allocate ( coef % ff_val_chn( coef % fmv_chn ), stat=alloc_status(6) )
  Allocate ( coef % ff_cwn( coef % fmv_chn ), stat=alloc_status(7) )
  Allocate ( coef % ff_bco( coef % fmv_chn ), stat=alloc_status(8) )
  Allocate ( coef % ff_bcs( coef % fmv_chn ), stat=alloc_status(9) )
  Allocate ( coef % ff_gam( coef % fmv_chn ), stat=alloc_status(10) )
  Allocate ( coef % gaz_units( coef % fmv_gas ), stat=alloc_status(11) )
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "allocation of coefs arrays")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If


  Read(file_lu, iostat = io_status )&
        & coef % fmv_gas_id,  &
        & coef % fmv_gas_pos, &
        & coef % fmv_var,     &
        & coef % fmv_lvl
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  Do n = 1, coef % fmv_gas
     Select Case( coef % fmv_gas_id(n) )
     Case( gas_id_mixed )
        coef % nmixed  = coef % fmv_var(n)
        coef % nlevels = coef % fmv_lvl(n)
     Case( gas_id_watervapour )
        coef % nwater  = coef % fmv_var(n)
     Case( gas_id_ozone )
        coef % nozone  = coef % fmv_var(n)
     Case( gas_id_wvcont )
        coef % nwvcont  = coef % fmv_var(n)
     Case( gas_id_co2 )
        coef % nco2  = coef % fmv_var(n)
     Case( gas_id_n2o )
        coef % nn2o  = coef % fmv_var(n)
     Case( gas_id_co )
        coef % nco  = coef % fmv_var(n)
     Case( gas_id_ch4 )
        coef % nch4  = coef % fmv_var(n)
     End Select
  End Do

  errMessage = 'io status while reading GAZ_UNITS'
  Read(file_lu, iostat = io_status )&
        & coef % gaz_units
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  errMessage = 'io status while reading FILTER_FUNCTIONS'
  If( all_channels ) Then
     Read(file_lu, iostat = io_status )&
           & coef % ff_ori_chn, &
           & coef % ff_val_chn, &
           & coef % ff_cwn,     &
           & coef % ff_bco,     &
           & coef % ff_bcs,     &
           & coef % ff_gam
     If(io_status /= 0) Then
        Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
        errorstatus = errorstatus_fatal
        Return
     Endif
  Else
     Allocate ( ivalues0( file_channels ), stat=alloc_status(1) )
     Allocate ( ivalues1( file_channels ), stat=alloc_status(2) )
     Allocate (  values0( file_channels ), stat=alloc_status(3) )
     Allocate (  values1( file_channels ), stat=alloc_status(4) )
     Allocate (  values2( file_channels ), stat=alloc_status(5) )
     Allocate (  values3( file_channels ), stat=alloc_status(6) )
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of ff coefs arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     Read(file_lu, iostat = io_status )&
           & ivalues0, &
           & ivalues1, &
           & values0,  &
           & values1,  &
           & values2,  &
           & values3
     If(io_status /= 0) Then
        Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
        errorstatus = errorstatus_fatal
        Return
     Endif
     coef % ff_ori_chn(:) = ivalues0 ( channels(:) )
     coef % ff_val_chn(:) = ivalues1 ( channels(:) )
     coef % ff_cwn(:)     =  values0 ( channels(:) )
     coef % ff_bco(:)     =  values1 ( channels(:) )
     coef % ff_bcs(:)     =  values2 ( channels(:) )
     coef % ff_gam(:)     =  values3 ( channels(:) )
     Deallocate ( ivalues0, stat=alloc_status(1) )
     Deallocate ( ivalues1, stat=alloc_status(2) )
     Deallocate (  values0, stat=alloc_status(3) )
     Deallocate (  values1, stat=alloc_status(4) )
     Deallocate (  values2, stat=alloc_status(5) )
     Deallocate (  values3, stat=alloc_status(6) )
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "deallocation of ff coefs arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
  Endif

  errMessage = 'io status while reading FUNDAMENTAL_CONSTANTS'
  Read(file_lu, iostat = io_status )&
        & coef % fc_speedl,    &
        & coef % fc_planck_c1, &
        & coef % fc_planck_c2, &
        & coef % fc_sat_height
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  errMessage = 'io status while reading EMISSIVITY model versions'
  Read(file_lu, iostat = io_status )&
        & coef % fastem_ver, &
        & coef % ssirem_ver
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  errMessage = 'io status while reading FASTEM'
  If( coef % fastem_ver >= 1 ) Then
     Read(file_lu, iostat = io_status ) coef % fastem_coef_nb
     If(io_status /= 0) Then
        Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
        errorstatus = errorstatus_fatal
        Return
     Endif
     Allocate ( coef % fastem_coef  ( coef % fastem_coef_nb ), stat=alloc_status(1) )
     Allocate ( coef % fastem_polar ( coef % fmv_chn ), stat=alloc_status(2) )
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of fastem coefs arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     If( all_channels ) Then
        Read(file_lu, iostat = io_status )&
              & coef % fastem_coef ,&
              & coef % fastem_polar
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     Else
        Allocate ( ivalues0( file_channels ), stat=alloc_status(1) )
        If( Any(alloc_status /= 0) ) Then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "allocation of fastem coefs arrays")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Return
        End If
        Read(file_lu, iostat = io_status )&
              & coef % fastem_coef,&
              & ivalues0
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
        coef % fastem_polar(:) = ivalues0( channels (:) )
        Deallocate ( ivalues0, stat=alloc_status(1) )
        If( Any(alloc_status /= 0) ) Then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "deallocation of fastem coefs arrays")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Return
        End If
     Endif
  Endif

  errMessage = 'io status while reading SSIREM'
  If( coef % ssirem_ver >= 1 ) Then
     Allocate ( coef % ssirem_chn ( coef % fmv_chn ), stat=alloc_status(1) )
     Allocate ( coef % ssirem_a0  ( coef % fmv_chn ), stat=alloc_status(2) )
     Allocate ( coef % ssirem_a1  ( coef % fmv_chn ), stat=alloc_status(3) )
     Allocate ( coef % ssirem_a2  ( coef % fmv_chn ), stat=alloc_status(4) )
     Allocate ( coef % ssirem_xzn1( coef % fmv_chn ), stat=alloc_status(5) )
     Allocate ( coef % ssirem_xzn2( coef % fmv_chn ), stat=alloc_status(6) )
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of ssirem coefs arrays")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     If( all_channels ) Then
        Read(file_lu, iostat = io_status )&
              & coef % ssirem_chn, &
              & coef % ssirem_a0,  &
              & coef % ssirem_a1,  &
              & coef % ssirem_a2,  &
              & coef % ssirem_xzn1,&
              & coef % ssirem_xzn2
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     Else
        Allocate ( ivalues0( file_channels ), stat=alloc_status(1) )
        Allocate (  values0( file_channels ), stat=alloc_status(2) )
        Allocate (  values1( file_channels ), stat=alloc_status(3) )
        Allocate (  values2( file_channels ), stat=alloc_status(4) )
        Allocate (  values3( file_channels ), stat=alloc_status(5) )
        Allocate (  values4( file_channels ), stat=alloc_status(6) )
        If( Any(alloc_status /= 0) ) Then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "allocation of ssirem coefs arrays")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Return
        End If
        Read(file_lu, iostat = io_status )&
              & ivalues0, &
              & values0,  &
              & values1,  &
              & values2,  &
              & values3,  &
              & values4
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
        coef % ssirem_chn(:)    = ivalues0 ( channels(:) )
        coef % ssirem_a0(:)     =  values0 ( channels(:) )
        coef % ssirem_a1(:)     =  values1 ( channels(:) )
        coef % ssirem_a2(:)     =  values2 ( channels(:) )
        coef % ssirem_xzn1(:)   =  values3 ( channels(:) )
        coef % ssirem_xzn2(:)   =  values4 ( channels(:) )
        Deallocate ( ivalues0, stat=alloc_status(1) )
        Deallocate (  values0, stat=alloc_status(2) )
        Deallocate (  values1, stat=alloc_status(3) )
        Deallocate (  values2, stat=alloc_status(4) )
        Deallocate (  values3, stat=alloc_status(5) )
        Deallocate (  values4, stat=alloc_status(6) )
        If( Any(alloc_status /= 0) ) Then
           errorstatus = errorstatus_fatal
           Write( errMessage, '( "deallocation of ssirem coefs arrays")' )
           Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
           Return
        End If
     Endif
  Endif

  Allocate ( coef % ref_prfl_p ( coef % fmv_lvl(gas_id_mixed) ), stat=alloc_status(1) )
  Allocate ( coef % ref_prfl_t ( coef % fmv_lvl(gas_id_mixed), coef % fmv_gas ), stat=alloc_status(2) )
  Allocate ( coef % ref_prfl_mr( coef % fmv_lvl(gas_id_mixed), coef % fmv_gas ), stat=alloc_status(3) )

  Allocate ( coef % lim_prfl_p( coef % fmv_lvl(gas_id_mixed) ), stat=alloc_status(4) )
  Allocate ( coef % lim_prfl_tmax( coef % fmv_lvl(gas_id_mixed) ), stat=alloc_status(5) )
  Allocate ( coef % lim_prfl_tmin( coef % fmv_lvl(gas_id_mixed) ), stat=alloc_status(6) )
  Allocate ( coef % lim_prfl_gmin( coef % fmv_lvl(gas_id_mixed), coef % fmv_gas ), stat=alloc_status(7) )
  Allocate ( coef % lim_prfl_gmax( coef % fmv_lvl(gas_id_mixed), coef % fmv_gas ), stat=alloc_status(8) )
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "allocation of fmv coefs arrays")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  errMessage = 'io status while reading REFERENCE PROFILE'
  Read(file_lu, iostat = io_status )&
        & coef % ref_prfl_p, &
        & coef % ref_prfl_t, &
        & coef % ref_prfl_mr
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  errMessage = 'io status while reading PROFILE LIMITS'
  Read(file_lu, iostat = io_status )&
        & coef % lim_prfl_p,    &
        & coef % lim_prfl_tmax, &
        & coef % lim_prfl_tmin, &
        & coef % lim_prfl_gmax, &
        & coef % lim_prfl_gmin
  If(io_status /= 0) Then
     Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
     errorstatus = errorstatus_fatal
     Return
  Endif

  ! FAST COEFFICIENT section
  errMessage = 'io status while reading Mixed gases coefs'
  If ( coef % nmixed > 0 ) Then
     Allocate (                &
           & coef % mixedgas (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nmixed   ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of mixed gaz coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % mixedgas(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % mixedgas(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do

  Endif


  errMessage = 'io status while reading Water vapour coefs'
  If ( coef % nwater > 0 ) Then
     Allocate (                &
           & coef % watervapour (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nwater   ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of water vapour coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % watervapour(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % watervapour(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do

  Endif

  errMessage = 'io status while reading Ozone coefs'
  If ( coef % nozone > 0 ) Then
     Allocate (                &
           & coef % ozone (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nozone   ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of ozone coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % ozone(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % ozone(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  errMessage = 'io status while reading WV continuum coefs'
  If ( coef % nwvcont > 0 ) Then
     Allocate (                &
           & coef % wvcont (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nwvcont   ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of WV continuum coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % wvcont(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % wvcont(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  errMessage = 'io status while reading CO2 coefs'
  If ( coef % nco2 > 0 ) Then
     Allocate (                &
           & coef % co2      (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nco2     ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of CO2 coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % co2(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % co2(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  errMessage = 'io status while reading N2O coefs'
  If ( coef % nn2o > 0 ) Then
     Allocate (                &
           & coef % n2o      (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nn2o     ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of N2O coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % n2o(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % n2o(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  errMessage = 'io status while reading CO coefs'
  If ( coef % nco > 0 ) Then
     Allocate (                &
           & coef % co       (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nco      ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of CO coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % co(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % co(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  errMessage = 'io status while reading CH4 coefs'
  If ( coef % nch4 > 0 ) Then
     Allocate (                &
           & coef % ch4      (  &
           & coef % nlevels  ,  &
           & coef % fmv_chn  ,  &
           & coef % nch4     ), &
           & stat= alloc_status(1))
     If( Any(alloc_status /= 0) ) Then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "allocation of CH4 coefs array")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If

     i = 1
     Do chn = 1, file_channels
        If( all_channels ) Then
           Read(file_lu, iostat = io_status ) coef % ch4(: , chn , :)
        Else If( chn == channels( i ) ) Then
           Read(file_lu, iostat = io_status ) coef % ch4(: , i , :)
           If( i < coef % fmv_chn ) Then
              i = i + 1
           End If
        Else
           Read(file_lu, iostat = io_status )
        End If
        If(io_status /= 0) Then
           Call Rttov_ErrorReport (io_status, errMessage, NameOfRoutine)
           errorstatus = errorstatus_fatal
           Return
        Endif
     End Do
  Endif

  !
  ! Here add reading of new sections for binary format in order to keep compatibility with
  ! previous versions
  !



End Subroutine rttov_readcoeffs_binary
