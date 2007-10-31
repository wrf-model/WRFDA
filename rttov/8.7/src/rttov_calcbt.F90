!
Subroutine rttov_calcbt( &
     & nfrequencies,   &! in
     & nchannels,      &! in
     & channels,       &! in
     & polarisations,  &! in
     & coeffs,         &! in
     & rad            ) ! inout
  ! Description:
  ! To convert an array of radiances in many channels
  ! to equivalent black body brightness temperatures.
  ! Planck function is applied with a "central wave number"
  ! Temperature is corrected by "band corrections" coefficients
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
  ! 1.3    28/02/2004  Improved vectorisation (D Dent)
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
       & rttov_coef        ,&
       & radiance_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in) :: nchannels          ! Number of processed radiances
  Integer(Kind=jpim),  Intent(in) :: nfrequencies       ! Number of processed radiances
  Integer(Kind=jpim),  Intent(in) :: channels(nfrequencies)! Array of channel indices.
  Integer(Kind=jpim),  Intent(in) :: polarisations(nchannels, 3)! Array of channel indices.

  Type(rttov_coef),    Intent(in)    :: coeffs             ! Coefficients
  Type(radiance_Type), Intent(inout) :: rad       ! input radiances and output BT

  ! radiances are expressed in mw/cm-1/ster/sq.m
  ! and temperatures in Kelvin

  !local variables:
  Real(Kind=jprb) :: tstore1,tstore2
  Real(Kind=jprb) :: radtotal, radclear
  Integer(Kind=jpim) :: chan,i,ipol,ipz,npol,signrad

  !- End of header ------------------------------------------------------

  Do i = 1, nfrequencies
     chan = channels(i)
     ipol = polarisations(i, 1)
     npol = polarisations(i, 3)
  !total
  !Note we must add average of 1st two elements of Stokes vector to differences in 3rd/4th before conversion and remove afterwards.
!cdir nodep
!cdir novector
     Do ipz = 1, npol
        radtotal = rad%total(ipol+ipz-1)
   !clear
        radclear = rad%clear(ipol+ipz-1)
        if (ipz > 2) then
          radtotal = radtotal + 0.5*(rad%total(ipol) + rad%total(ipol+1))
          radclear = radclear + 0.5*(rad%clear(ipol) + rad%clear(ipol+1))
        End If
        tstore1 = coeffs%planck2(chan) / Log( 1+coeffs%planck1(chan)/radtotal )
        tstore2 = coeffs%planck2(chan) / Log( 1+coeffs%planck1(chan)/radclear )
        rad%bt(ipol+ipz-1) = ( tstore1 - coeffs%ff_bco(chan) ) / coeffs%ff_bcs(chan)
        rad%bt_clear(ipol+ipz-1) = ( tstore2-coeffs%ff_bco(chan) ) / coeffs%ff_bcs(chan)
        if (ipz > 2) then
           rad%bt(ipol+ipz-1)    = rad%bt(ipol+ipz-1) - 0.5*(rad%bt(ipol) + rad%bt(ipol+1))
          rad%bt_clear(ipol+ipz-1) = rad%bt_clear(ipol+ipz-1) - 0.5*(rad%bt_clear(ipol) + rad%bt_clear(ipol+1))
        EndIf
     End Do
  End Do

End Subroutine rttov_calcbt
