!
Subroutine rttov_calcemis_ir( &
     & profiles,      &! in
     & geometry,      &! in
     & coef,          &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & channels,      &! in
     & lprofiles,     &! in
     & calcemis,      &! in
     & emissivity    ) ! inout
  ! Description:
  ! To compute IR surface emissivities for all channels and all
  ! profiles if desired
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
  ! RTTOV-6 IR surface emissivity report, V. Sherlock at:
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/papers/isem6.pdf
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Comments added (R Saunders)
  !  1.2       29/03/2005  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  ! Imported Parameters:
  Use rttov_const, Only : &
       & surftype_land, &
       & surftype_seaice

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & geometry_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(geometry_Type), Intent(in) ,Target   :: geometry(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coef
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: channels(nchannels)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nchannels)
  Logical,             Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),     Intent(inout) :: emissivity(nchannels)



  !local variables:
  Type(profile_Type),  Pointer   :: prof
  Type(geometry_Type), Pointer   :: geom

  Integer(Kind=jpim) :: j, chan

  !- End of header --------------------------------------------------------

  ! Loop on all channels
  Do j = 1, nchannels
     If ( .Not. calcemis(j) ) Cycle

     chan = channels(j)

     ! point to corresponding profile and geometry structures
     prof => profiles( lprofiles(j) )
     geom => geometry( lprofiles(j) )

     !-----------------------------------------
     !1. Use a fixed value over land and seaice
     !-----------------------------------------
     If ( prof % skin % surftype == surftype_land ) Then
        emissivity(j) = 0.98_JPRB
     Else If ( prof % skin % surftype == surftype_seaice ) Then
        emissivity(j) = 0.99_JPRB
     Else

        !------------------------------------------------------------------
        !2. Over sea, emissivity is a polynomial in normalized zenith angle
        ! ISEM6 model
        !------------------------------------------------------------------

        emissivity(j) =   coef % ssirem_a0  (chan) -  &
                         & coef % ssirem_a1  (chan) *  &
                                & geom % normzen ** coef % ssirem_xzn1(chan) - &
                         & coef % ssirem_a2  (chan) *  &
                                & geom % normzen ** coef % ssirem_xzn2(chan)

     End If
  End Do



End Subroutine rttov_calcemis_ir
