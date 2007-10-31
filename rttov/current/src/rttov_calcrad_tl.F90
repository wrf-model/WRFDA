Subroutine rttov_calcrad_tl( &
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & profiles,        &! in
     & profiles_tl,     &! in
     & coeffs,          &! in
     & rad_skin,        &! in
     & rad_surfair,     &! in
     & rad_layer,       &! in
     & rad_skin_tl,     &! out
     & rad_surfair_tl,  &! out
     & rad_layer_tl    ) ! out
  !
  ! Description:
  ! TL code to convert an array of atmospheric temperatures
  !   to planck radiances in many channels
  ! No TL on Rad Cosmic, tl = 0.
  !
  ! derivative of Planck function with respect to temperature is
  !
  !                                     C2 * Nu
  !              C1 * C2 * Nu**4 * Exp( ------- )
  !                                        T
  ! B'(T,Nu) = ------------------------------------- dT
  !                     (      C2 * Nu       )**2
  !               T**2 *( Exp( ------- ) - 1 )
  !                     (         T          )
  !
  !
  ! which can be reduced to the following, with
  !  C1 = C1 * Nu**3
  !  C2 = C2 * Nu
  !
  !              C2 * B(T,Nu) * (C1 + B(T,Nu))
  !  B'(T,Nu) =  ----------------------------- dT
  !                        C1 * T**2
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
  ! 1.3    29/03/2005  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:

  !
  Use rttov_types, only : &
    & rttov_coef     ,&
    & profile_type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(profile_Type),  Intent(in) ,Target   :: profiles_tl(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coeffs
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies  ! Number of processed radiances
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)  ! Array of channel indices.
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Real(Kind=jprb),    Intent(in) :: rad_skin(nchannels)
  Real(Kind=jprb),    Intent(in) :: rad_surfair(nchannels)
  Real(Kind=jprb),    Intent(in) :: rad_layer( coeffs%nlevels , nchannels )
  Real(Kind=jprb),    Intent(out) :: rad_skin_tl(nchannels)
  Real(Kind=jprb),    Intent(out) :: rad_surfair_tl(nchannels)
  Real(Kind=jprb),    Intent(out) :: rad_layer_tl( coeffs%nlevels , nchannels )



  !local variables:
  Real(Kind=jprb) :: t_effective
  Real(Kind=jprb) :: t_effective_tl
  Real(Kind=jprb) :: rad_skin_freq_tl, rad_surfair_freq_tl, rad_layer_freq_tl(coeffs%nlevels)
  Integer(Kind=jpim) :: chan,i,lev,jpol,kpol,npol,ipol  ! loop indices
  Type(profile_Type),  Pointer   :: prof
  Type(profile_Type),  Pointer   :: prof_tl

  !- End of header --------------------------------------------------------

  Do i = 1, nfrequencies
     chan = channels(i)
     ipol=polarisations(i,1)
     npol = polarisations(i,3)
      ! point to corresponding profile and geometry structures
     prof    => profiles( lprofiles(i) )
     prof_tl => profiles_tl( lprofiles(i) )

     t_effective    = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % skin % t
     t_effective_tl = coeffs%ff_bcs(chan) * prof_tl % skin % t
     rad_skin_freq_tl =( coeffs%planck2(chan) * rad_skin(ipol)          * &
                     & ( coeffs%planck1(chan) + rad_skin(ipol) )       / &
                     & ( coeffs%planck1(chan) * t_effective**2 ))   * &
                    & t_effective_tl

     t_effective       = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % s2m % t
     t_effective_tl    = coeffs%ff_bcs(chan) * prof_tl % s2m % t
     rad_surfair_freq_tl =( coeffs%planck2(chan) * rad_surfair(ipol)       * &
                        & ( coeffs%planck1(chan) + rad_surfair(ipol) )    / &
                        & ( coeffs%planck1(chan) * t_effective**2 ))   * &
                      & t_effective_tl

     Do lev = 1, coeffs%nlevels
        t_effective    = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * prof % t(lev)
        t_effective_tl = coeffs%ff_bcs(chan) * prof_tl % t(lev)
        rad_layer_freq_tl(lev) =( coeffs%planck2(chan) * rad_layer(lev,ipol)    * &
                               & ( coeffs%planck1(chan) + rad_layer(lev,ipol) ) / &
                               & ( coeffs%planck1(chan) * t_effective**2 ) ) * &
                             & t_effective_tl
     End Do
     !
     Do jpol=ipol, ipol+npol-1
       rad_skin_tl(jpol) = rad_skin_freq_tl
       rad_surfair_tl(jpol) = rad_surfair_freq_tl
       Do lev = 1, coeffs%nlevels
          rad_layer_tl(lev,jpol)=rad_layer_freq_tl(lev)
       End Do
     End Do
  End Do

End Subroutine rttov_calcrad_tl
