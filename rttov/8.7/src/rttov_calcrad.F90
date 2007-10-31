!
Subroutine rttov_calcrad( &
     & addcosmic,      &! in
     & nchannels,      &! in
     & nfrequencies,   &! in
     & nprofiles,      &! in
     & channels,       &! in
     & polarisations,  &! in
     & lprofiles,      &! in
     & profiles,       &! in
     & coeffs,         &! in
     & rad_cosmic,     &! out
     & rad_skin,       &! out
     & rad_surfair,    &! out
     & rad_layer      ) ! out
  ! Description:
  ! To convert an array of atmospheric temperatures
  !   to planck radiances in many channels
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
  ! Method: Uses band correction factors to convert T to radiance
  !         which have been precomputed for each channel and are read from
  !         the RT coefficient file.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  ! 1.0    01/12/2002  New F90 code with structures (P Brunel A Smith)
  !                      based on PLNCX of previous RTTOV versions
  ! 1.1    02/01/2003  Comments added (R Saunders)
  ! 1.2    26/09/2003  Multiple polarisations (S English)
  ! 1.3    11/02/2005  Code vectorisation improved (D Dent)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  ! Imported Parameters:
  Use rttov_const, Only: &
       & tcosmic

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim), Intent(in)   :: nprofiles   ! number of profiles
  Type(profile_Type), Intent(in) ,Target   :: profiles(nprofiles)! profiles
  Type(rttov_coef),   Intent(in)   :: coeffs   ! coefficients (Planck)
  Integer(Kind=jpim), Intent(in)   :: nfrequencies  ! Number of processed radiances
  Integer(Kind=jpim), Intent(in)   :: nchannels  ! Number of processed radiances
  Integer(Kind=jpim), Intent(in)   :: channels(nfrequencies)  ! Array of channel indices.
  Integer(Kind=jpim), Intent(in)   :: polarisations(nchannels,3)  ! Array of channel indices.
  Integer(Kind=jpim), Intent(in)   :: lprofiles(nfrequencies) ! Array of profile indices.
  Logical,            Intent(in)   :: addcosmic            ! switch for adding cosmic background

  Real(Kind=jprb), Intent(out) :: rad_cosmic(nchannels)  ! cosmic background radiance
  Real(Kind=jprb), Intent(out) :: rad_skin(nchannels)    ! surface skin radiance
  Real(Kind=jprb), Intent(out) :: rad_surfair(nchannels) ! 2m air radiance
  Real(Kind=jprb), Intent(out) :: rad_layer( coeffs%nlevels , nchannels ) ! layer radiances
  ! radiances are expressed in mw/cm-1/ster/sq.m
  ! and temperatures in Kelvin
  !local variables:
  Real(Kind=jprb) :: t_effective    ! effective temperature
  Real(Kind=jprb) :: t_effective_1(nfrequencies)    ! effective temperature
  Real(Kind=jprb) :: t_effective_2(nfrequencies)  ! effective temperature
  Real(Kind=jprb) :: t_effective_3(coeffs%nlevels,nfrequencies)    ! effective temperature
  Real(Kind=jprb) :: rad_skin_freq, rad_surfair_freq
  Real(Kind=jprb) :: rad_skin_f(nfrequencies), rad_surfair_f(nfrequencies)
  Real(Kind=jprb) :: rad_layer_f(coeffs%nlevels,nfrequencies)
  Real(Kind=jprb) :: rad_layer_freq(coeffs%nlevels),rad_cosmic_freq
  Integer(Kind=jpim) :: chan,i,lev,jpol,npol,ipol  ! loop indices
  Type(profile_Type),  Pointer   :: prof ! pointer on profile structures

  !- End of header ------------------------------------------------------

  If ( addcosmic ) Then
!cdir nodep
     Do i = 1, nfrequencies
        chan = channels(i)
        t_effective = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * tcosmic
        rad_cosmic_freq = coeffs%planck1(chan) / &
             & ( Exp( coeffs%planck2(chan)/t_effective ) - 1.0_JPRB )
        ipol = polarisations(i,1)
        npol = polarisations(i,3)
        Do jpol=ipol, ipol+npol-1
           rad_cosmic(jpol) = rad_cosmic_freq
        End Do
     End Do
  Else
     rad_cosmic(:) = 0.0_JPRB
  End If

  Do i = 1, nfrequencies

     chan = channels(i)
     ! point to corresponding profile (temp. for pressure levels, 2m, skin)
     prof => profiles( lprofiles(i) )
     t_effective_1(i) = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % skin % t
     t_effective_2(i) = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % s2m % t
     Do lev = 1, coeffs%nlevels
        t_effective_3(lev,i) = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % t(lev)
     End Do
  End Do

!cdir nodep
  Do i = 1, nfrequencies

     chan = channels(i)

     rad_skin_f(i) = coeffs%planck1(chan) / &
          & ( Exp( coeffs%planck2(chan)/t_effective_1(i) ) - 1.0_JPRB )

     rad_surfair_f(i) = coeffs%planck1(chan) / &
          & ( Exp( coeffs%planck2(chan)/t_effective_2(i) ) - 1.0_JPRB )

  End Do
  Do i = 1, nfrequencies
     chan = channels(i)
     Do lev = 1, coeffs%nlevels
        rad_layer_f(lev,i) = coeffs%planck1(chan) / &
        & ( Exp( coeffs%planck2(chan)/t_effective_3(lev,i)  ) - 1.0_JPRB )
     End Do
  End Do
     !
  Do i = 1, nfrequencies

     ipol = polarisations(i,1)
     npol = polarisations(i,3)
     Do jpol=ipol, ipol+npol-1
        rad_skin(jpol) = rad_skin_f(i)
        rad_surfair(jpol) = rad_surfair_f(i)
        Do lev = 1, coeffs%nlevels
           rad_layer(lev,jpol)=rad_layer_f(lev,i)
        End Do
     End Do
  End Do

End Subroutine rttov_calcrad
