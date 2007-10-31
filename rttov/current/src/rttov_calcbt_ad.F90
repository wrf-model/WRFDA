Subroutine rttov_calcbt_ad( &
     & nfrequencies,    &! in
     & nchannels,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & coeffs,          &! in
     & rad,             &! in
     & rad_ad          ) ! inout
  !
  ! Description:
  ! To convert an array of radiances in many channels
  ! to equivalent black body brightness temperatures.
  ! Planck function is applied with a "central wave number"
  ! Temperature is corrected by "band corrections" coefficients
  ! derivative of inverse Planck function with respect to radiance is
  !
  !                             C1 * C2 * Nu**4
  ! B-1'(R,Nu) = --------------------------------------------- dR
  !                     (    C1 * Nu**3) (     C1 * Nu**3 )**2
  !               R**2 *(1 + ----------) ( Ln( ---------- )
  !                     (         R    ) (        R       )
  !
  ! which can be reduced to the following, with
  !  C1 = C1 * Nu**3
  !  C2 = C2 * Nu
  !
  !                  C1 * B-1(R,Nu)**2
  ! B-1'(R,Nu) = ----------------------- dR
  !                  C2 * R * (R + C1)
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
  ! Method: Uses band correction coefficients for each channel
  !         read in from RT coefficient file.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  ! 1.0    01/12/2002  New F90 code with structures (P Brunel A Smith)
  !                      based on BRIGV of previous RTTOV versions
  ! 1.1    02/01/2003  Comments added (R Saunders)
  ! 1.2    08/01/2004  Polarisation added (S English)
  ! 1.3    29/03/2005  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_types, only : &
       & rttov_coef     ,&
       & radiance_type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim), Intent(in)         :: nfrequencies
  Integer(Kind=jpim), Intent(in)         :: nchannels
  Integer(Kind=jpim), Intent(in)         :: channels(nfrequencies)
  Integer(Kind=jpim), Intent(in)         :: polarisations(nchannels,3)
  Type(rttov_coef),    Intent(in)    :: coeffs
  Type(radiance_Type), Intent(in)    :: rad  ! rad%total rad%clear
  Type(radiance_Type), Intent(inout) :: rad_ad
    ! output rad_ad%total  only
    ! input  rad_ad%bt
      ! Clear BT and Rad are ignored in AD



  !local variables:
  Real(Kind=jprb) :: tstar
  Real(Kind=jprb) :: tstar_ad
  Real(Kind=jprb) :: radtotal
  Integer(Kind=jpim) :: chan,i,npol,ipol,ipz

  !- End of header --------------------------------------------------------

  Do i = 1, nfrequencies
     ipol = polarisations(i, 1)
     npol = polarisations(i, 3)
     chan = channels(i)
     !total
     Do ipz = 1, npol
        ! total radiance
        ! T star for direct model
        tstar = coeffs%ff_bco(chan) + coeffs%ff_bcs(chan) * rad%bt(ipol+ipz-1)
        radtotal = rad%total(ipol+ipz-1)
        if (ipz > 2) then
           tstar = tstar + coeffs%ff_bcs(chan) * 0.5*(rad%bt(ipol) + rad%bt(ipol+1))
           radtotal = radtotal + 0.5*(rad%total(ipol) + rad%total(ipol+1))
        EndIf
        ! AD
        tstar_ad = rad_ad%bt(ipol+ipz-1) / coeffs%ff_bcs(chan)
        rad_ad%total(ipol+ipz-1) = coeffs%planck1(chan) * tstar**2      /&
                   & ( coeffs%planck2(chan) * radtotal *     &
                      & ( radtotal + coeffs%planck1(chan) ) )&
                   & * tstar_ad
     End Do
  End Do

End Subroutine rttov_calcbt_ad
