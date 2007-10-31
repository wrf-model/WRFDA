!
Subroutine rttov_setup (&
      & errorstatus,      &! out
      & Err_unit,         &! in
      & verbosity_level,  &! in
      & ninst,            &! in
      & coef,             &! out
      & instrument,       &! in
      & channels         ) ! in Optional
  !
  ! Description:
  !
  ! Setup routine for RTTOV
  ! Handling of error messages. (rttov_errorhandling)
  ! Read coefficients (rttov_readcoeffs)
  !
  ! Error messages will be sent on the optional unit number errunit.
  !      Default is the value defined in the module for constants.
  !
  ! The levels of verbosity are
  !  0 = no error messages output
  !  1 = FATAL errors only printed. these are errors which
  !      mean that profile should be aborted (e.g. unphysical
  !      profile input)
  !  2 = WARNING errors only printed. Errors which can allow
  !      the computation to continue but the results may be
  !      suspect (e.g. profile outside basis profile limits)
  !  3 = INFORMATION messages which inform the user about
  !      the computation
  !
  ! For each instrument:
  ! Read an ASCII or binary coefficient file and allocate coeff structure
  !   arrays according to the optional list of channels.
  ! The user can provide an optional list of channels in "channels" argument
  !  array to reduce the output coefficient structure to this list. This
  ! can be important for reducing the memory allocation required when running
  ! with advanced IR sounders (e.g. AIRS or IASI). If the user
  !  wants all channels the "channels" argument shall not be present.
  !
  !
  ! Copyright:
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
  !
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version    Date       Comment
  !  1.0    10/03/2003   Original code (P Brunel)
  !
  ! Code Description:
  !   FORTRAN 90, following AAPP standards
  !
  ! Declarations
  !
  ! Global variables:
  ! Modules used:
  !
  Use rttov_const, Only :   &
       & errorstatus_fatal

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorhandling.interface"
#include "rttov_errorreport.interface"
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"
  !
  ! Subroutine arguments
  !   Scalar arguments with intent(in):
  Integer(Kind=jpim), Intent (in) :: Err_Unit        ! Logical error unit (<0 for default)
  Integer(Kind=jpim), Intent (in) :: verbosity_level ! (<0 for default)
  Integer(Kind=jpim), Intent (in) :: ninst           ! number of RTTOV ids / instruments  requested
  Integer(Kind=jpim), Intent (in) :: instrument(:,:) ! Instrument triplet
         ! first dimension  : (platform, satellite identification, instrument) number
         ! second dimension : nsat
  Integer(Kind=jpim), Optional, Intent (in) :: channels(:,:)   ! list of channels to extract (channels,msat)

  ! scalar arguments with intent(out):
  Integer(Kind=jpim),            Intent (out) :: errorstatus(ninst) ! return code
  Type( rttov_coef ), Intent (out) :: coef(ninst)        ! coefficients



  ! Local scalars/arrays
  Integer(Kind=jpim) :: dimchans ! size of array channels for channels dimension
  Integer(Kind=jpim) :: inst     ! instrument loop index
  Integer(Kind=jpim) :: nchans   ! number of requested channels per instrument (0 = all)
  Integer(Kind=jpim) :: alloc_status ! de/allocation status
  Integer(Kind=jpim) :: i        ! loop index
  Integer(Kind=jpim), allocatable :: channels_list(:) ! list of requested channels
  Character (len=80) :: errMessage
  Character (len=12) :: NameOfRoutine = 'rttov_setup '
  !- End of header --------------------------------------------------------

  ! Error is set to fatal, in case of return
  ! before processing all instruments
  ! Readcoeffs will reset it to success
  errorstatus(:) = errorstatus_fatal

  ! Error Handling setup routine
  call rttov_errorhandling(Err_Unit, verbosity_level)

  ! Check optional argument channels
  If( Present ( channels ) ) Then
     dimchans = Size( channels, dim=1 )
  Else
     dimchans = 0
  End If

  Do inst = 1, ninst

     ! Finds the last non null channel for ninst
     nchans = 0
     if( Present ( channels ) ) Then
        do i = 1, dimchans
           if( channels(i, inst) > 0 ) then
              nchans  = nchans + 1
           endif
        End Do
     Endif

     If( nchans > 0 ) Then
        ! Some channels wanted, create a list of the
        ! selected channels without O values

        ! Allocate intermediate channels list
        Allocate ( channels_list ( nchans ), stat= alloc_status)
        If( alloc_status /= 0 ) Then
           errorstatus(inst) = errorstatus_fatal
           Write( errMessage, '( "allocation of intermediate channels list")' )
           Call Rttov_ErrorReport (errorstatus(inst), errMessage, NameOfRoutine)
           Return
        End If

        ! Create intermediate channels list (use nchans var. again)
        nchans = 0
        do i = 1, dimchans
           if( channels(i, ninst) > 0 ) then
              nchans  = nchans + 1
              channels_list(nchans) = channels(i, inst)
           endif
        End Do

        ! Read coefficients
        Call rttov_readcoeffs  (  &
              & errorstatus(inst),        &! out
              & coef(inst),               &! inout
              & instrument(:,inst),       &! in
              & channels = channels_list ) ! in
        Call rttov_initcoeffs  ( &
              & errorstatus(inst),        &! out
              & coef(inst)               ) ! inout

        Deallocate ( channels_list , stat=alloc_status )
        If( alloc_status /= 0 ) Then
           errorstatus(inst) = errorstatus_fatal
           Write( errMessage, '( "deallocation of intermediate channels list")' )
           Call Rttov_ErrorReport (errorstatus(inst), errMessage, NameOfRoutine)
           Return
        End If


     Else
        ! All channels , read coefficients
        Call rttov_readcoeffs  (  &
              & errorstatus(inst),  &! out
              & coef(inst),         &! out
              & instrument(:,inst) ) ! in
        Call rttov_initcoeffs  ( &
              & errorstatus(inst),        &! out
              & coef(inst)               ) ! inout

     Endif

  End Do



End Subroutine rttov_setup
