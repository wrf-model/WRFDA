!
Subroutine rttov_calcemis_mw_tl ( &
     & profiles,         &! in
     & profiles_tl,      &! in
     & geometry,         &! in
     & coef,             &! in
     & nfrequencies,     &! in
     & nchannels,        &! in
     & nprofiles,        &! in
     & channels,         &! in
     & polarisations,    &! in
     & lprofiles,        &! in
     & transmission,     &! in
     & transmission_tl,  &! in
     & calcemis,         &! in
     & emissivity_tl,    &! inout
     & reflectivity_tl  ) ! out
  ! Description:
  ! Tangent Linear of rttov_calcemis_mw
  ! To compute MW surface emissivities for all channels and all
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
  ! FASTEM-1 English and Hewison 1998.
  ! FASTEM-2 Deblonde and English 2001.
  ! FASTEM-3 English 2003.
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Comments added (R Saunders)
  !  1.2       26/09/2003  Polarimetric code and Fastem-3 (S English)
  !  1.3       18/08/2004  Added some _JPRB to constants (S English)
  !  1.4       29/03/2005  Add end of header comment (J. Cameron)
  !  1.5       14/10/2005  Reintroduce -r 121:122 changes, see -r 133:134
  !                        Fixing bug in azimuth angles > 270 (J Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  !
  ! Imported Parameters:
  Use rttov_const, Only :   &
       & pi                  ,&
       & surftype_sea

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & transmission_Type  ,&
       & geometry_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(geometry_Type), Intent(in) ,Target   :: geometry(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coef
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)

  Type(transmission_Type), Intent(in):: transmission
  Logical,             Intent(in)    :: calcemis(nchannels)

  Type(profile_Type),  Intent(in) ,Target   :: profiles_tl(nprofiles)
  Type(transmission_Type), Intent(in)    :: transmission_tl
  Real(Kind=jprb),         Intent(inout) :: emissivity_tl(nchannels)
  Real(Kind=jprb),         Intent(out)   :: reflectivity_tl(nchannels)



  !local constants:
  Real(Kind=jprb), Parameter :: quadcof(4,2) = Reshape( &
       & (/ 0.0_JPRB, 1.0_JPRB, 1.0_JPRB, 2.0_JPRB, &
       & 1.0_JPRB, -1.0_JPRB, 1.0_JPRB, -1.0_JPRB  /), (/4,2/) )
  Real(Kind=jprb), Parameter :: freqfixed(4) = Reshape( &
       & (/ 7.0_JPRB, 10.0_JPRB, 19.0_JPRB, 37.0_JPRB /), (/4/) )

  !local variables:
  Real(Kind=jprb) :: tcelsius
  Real(Kind=jprb) :: tcelsius_sq
  Real(Kind=jprb) :: tcelsius_cu
  Real(Kind=jprb) :: f1,f2
  Real(Kind=jprb) :: del1,del2
  Real(Kind=jprb) :: einf
  Real(Kind=jprb) :: fen,fen_sq
  Real(Kind=jprb) :: den1,den2
  Real(Kind=jprb) :: perm_free
  Real(Kind=jprb) :: sigma
  Real(Kind=jprb) :: perm_real1,perm_real2
  Real(Kind=jprb) :: perm_imag1,perm_imag2,perm_imag3
  Real(Kind=jprb) :: perm_Real,perm_imag
  Real(Kind=jprb) :: perm_static,perm_infinite
  Real(Kind=jprb) :: freq_ghz,freq_ghz_sq
  Real(Kind=jprb) :: fresnel_v_Real,fresnel_v_imag
  Real(Kind=jprb) :: fresnel_h_Real,fresnel_h_imag
  Real(Kind=jprb) :: fresnel_v,fresnel_h
  Real(Kind=jprb) :: small_rough_cor,foam_cor
  Real(Kind=jprb) :: large_rough_cor(2)
  Real(Kind=jprb) :: small_rough,large_rough
  Real(Kind=jprb) :: variance,varm
  Real(Kind=jprb) :: wind10
  Real(Kind=jprb) :: wind10_sq,windsec
  Real(Kind=jprb) :: wind10_direction, windangle, windratio ! Note wind azimuth is in radians
  Real(Kind=jprb) :: opdpsfc,freqr
  Real(Kind=jprb) :: zrough_v,zrough_h
  Real(Kind=jprb) :: zreflmod_v,zreflmod_h
  Real(Kind=jprb) :: delta,delta2
  Real(Kind=jprb) :: qdepol,emissfactor
  Real(Kind=jprb) :: emissfactor_v,emissfactor_h
  Real(Kind=jprb) :: emissstokes(nfrequencies,4)
  Real(Kind=jprb) :: emissstokes_tl(nfrequencies,4)
  Real(Kind=jprb) :: reflectstokes_tl(nfrequencies,4)
  Real(Kind=jprb) :: zc(12),zx(9)
  Real(Kind=jprb) :: azimuthal_emiss,u19,phi,dfreq
  Real(Kind=jprb) :: tbfixed(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e,a2e,a3e    ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb), Pointer :: c(:)
  Complex(Kind=jprb) :: perm1,perm2
  Complex(Kind=jprb) :: rhth,rvth
  Complex(Kind=jprb) :: permittivity
  Integer(Kind=jpim) :: i,j,chan,istokes,ifreq,m
  Integer(Kind=jpim) :: iquadrant    ! Determines which quadrant (NE, SE, SW, NW) the wind is blowing to
  Integer(Kind=jpim) :: pol_id    ! polarisation indice
  Integer(Kind=jpim) :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  Integer(Kind=jpim) :: jcof,jcofm1
  Type(profile_Type),  Pointer    :: prof
  Type(profile_Type),  Pointer    :: prof_tl
  Type(geometry_Type), Pointer    :: geom


  Real(Kind=jprb) :: tcelsius_tl
  Real(Kind=jprb) :: tcelsius_sq_tl
  Real(Kind=jprb) :: tcelsius_cu_tl
  Real(Kind=jprb) :: f1_tl, f2_tl
  Real(Kind=jprb) :: del1_tl, del2_tl
  Real(Kind=jprb) :: einf_tl
  Real(Kind=jprb) :: fen_tl, fen_sq_tl
  Real(Kind=jprb) :: den1_tl, den2_tl
  Real(Kind=jprb) :: sigma_tl
  Real(Kind=jprb) :: perm_real1_tl, perm_real2_tl
  Real(Kind=jprb) :: perm_imag1_tl, perm_imag2_tl, perm_imag3_tl
  Real(Kind=jprb) :: perm_Real_tl, perm_imag_tl
  Real(Kind=jprb) :: perm_static_tl, perm_infinite_tl
  Real(Kind=jprb) :: fresnel_v_Real_tl, fresnel_v_imag_tl
  Real(Kind=jprb) :: fresnel_h_Real_tl, fresnel_h_imag_tl
  Real(Kind=jprb) :: fresnel_v_tl, fresnel_h_tl
  Real(Kind=jprb) :: small_rough_cor_tl, foam_cor_tl
  Real(Kind=jprb) :: large_rough_cor_tl(2)
  Real(Kind=jprb) :: small_rough_tl, large_rough_tl
  Real(Kind=jprb) :: variance_tl, varm_tl
  Real(Kind=jprb) :: wind10_tl
  Real(Kind=jprb) :: wind10_sq_tl, windsec_tl
  Real(Kind=jprb) :: wind10_direction_tl, windangle_tl, windratio_tl ! Note wind azimuth is in radians
  Real(Kind=jprb) :: opdpsfc_tl, freqr_tl
  Real(Kind=jprb) :: zrough_v_tl, zrough_h_tl
  Real(Kind=jprb) :: zreflmod_v_tl, zreflmod_h_tl
  Real(Kind=jprb) :: delta_tl, delta2_tl
  Real(Kind=jprb) :: qdepol_tl, emissfactor_tl
  Real(Kind=jprb) :: emissfactor_v_tl, emissfactor_h_tl
  Real(Kind=jprb) :: zx_tl(9)
  Real(Kind=jprb) :: azimuthal_emiss_tl,u19_tl,phi_tl
  Real(Kind=jprb) :: tbfixed_tl(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed_tl(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated_tl(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e_tl,a2e_tl,a3e_tl,atot     ! coefficients used in azimuthal emissivity model
  Complex(Kind=jprb) :: perm1_tl, perm2_tl
  Complex(Kind=jprb) :: rhth_tl, rvth_tl
  Complex(Kind=jprb) :: permittivity_tl
  Integer(Kind=jpim) :: wanted_fastem_ver,iii  ! user fastem version request

!- End of header --------------------------------------------------------

  ! If the coefficent file contains FASTEM 2 it contains also FASTEM 1 but
  ! the version choosen is given by coef % fastem_ver value
  wanted_fastem_ver = coef % fastem_ver

  !If a TL value of emissivity is passed to the routine
  ! this means that there is no need to compute it
  Where(emissivity_tl(:) /= 0.0_JPRB)
     reflectivity_tl(:) = - emissivity_tl(:)
  End Where

  !Loop over channels
  Do i = 1, nfrequencies
     ichannel=polarisations(i,1)
     If ( .Not. (calcemis(ichannel) .And. emissivity_tl(ichannel) == 0.0_JPRB) ) Cycle
     chan        = channels(i)
     prof        => profiles( lprofiles(i) )
     prof_tl     => profiles_tl( lprofiles(i) )
     geom        => geometry( lprofiles(i) )
     pol_id = coef % fastem_polar(chan) + 1

     !-------------------------------
     !0. Point to fastem coefficients
     !-------------------------------

     c => coef % fastem_coef

     !---------------
     !1. Sea surfaces
     !---------------

     If ( prof % skin % surftype == surftype_sea ) Then
        !-------------------------------------------
        !1.1 Calculate channel independent variables
        !-------------------------------------------
        ! no TL on wind direction, but TL on wind speed
        wind10_sq =   prof % s2m % u * prof % s2m % u +&
              & prof % s2m % v * prof % s2m % v
        wind10    = Sqrt( wind10_sq )
        windsec   = wind10 * geom%seczen
        wind10_sq_tl = 2.0_JPRB * prof % s2m %u * prof_tl % s2m % u + 2.0_JPRB * prof % s2m %v * prof_tl % s2m % v

        If( wind10 > 0._JPRB ) Then
           wind10_tl  = 0.5_JPRB * wind10_sq_tl/wind10
        Else
           wind10_tl = 0.0_JPRB
        Endif

        windsec_tl   = wind10_tl * geom%seczen

        !Set values for temperature polynomials (convert from kelvin to celsius)
        tcelsius = prof % skin % t - 273.15_JPRB
        tcelsius_sq = tcelsius * tcelsius     !quadratic
        tcelsius_cu = tcelsius_sq * tcelsius  !cubic

        tcelsius_tl    = prof_tl % skin % t
        tcelsius_sq_tl = 2 * tcelsius * tcelsius_tl
        tcelsius_cu_tl = 3 * tcelsius_sq * tcelsius_tl

        !Define two relaxation frequencies, f1 and f2
        f1 = c(1) + c(2) * tcelsius + c(3) * tcelsius_sq
        f2 = c(4) + c(5) * tcelsius + c(6) * tcelsius_sq + c(7) * tcelsius_cu
        f1_tl =  c(2) * tcelsius_tl + c(3) * tcelsius_sq_tl
        f2_tl =  c(5) * tcelsius_tl + c(6) * tcelsius_sq_tl + c(7) * tcelsius_cu_tl


        !Static permittivity estatic = del1+del2+einf
        del1 = c(8)  + c(9)  * tcelsius + c(10) * tcelsius_sq + c(11) * tcelsius_cu
        del2 = c(12) + c(13) * tcelsius + c(14) * tcelsius_sq + c(15) * tcelsius_cu
        einf = c(18) + c(19) * tcelsius
        del1_tl = c(9)  * tcelsius_tl + c(10) * tcelsius_sq_tl + c(11) * tcelsius_cu_tl
        del2_tl = c(13) * tcelsius_tl + c(14) * tcelsius_sq_tl + c(15) * tcelsius_cu_tl
        einf_tl = c(19) * tcelsius_tl


        freq_ghz    = coef % frequency_ghz(chan)
        freq_ghz_sq = freq_ghz * freq_ghz

        !-----------------------------------------------------
        !1.2 calculate permittivity using double-debye formula
        !-----------------------------------------------------

        fen          = 2.0_JPRB * c(20) * freq_ghz * 0.001_JPRB
        fen_sq       = fen*fen
        den1         = 1.0_JPRB + fen_sq * f1 * f1
        den2         = 1.0_JPRB + fen_sq * f2 * f2
        perm_real1   = del1 / den1
        perm_real2   = del2 / den2
        perm_imag1   = del1 * fen * f1 / den1
        perm_imag2   = del2 * fen * f2 / den2
        perm_free    = 8.854E-03_JPRB
        sigma        = 2.906_JPRB + 0.09437_JPRB * tcelsius
        perm_imag3   = sigma / (2.0_JPRB * c(20) * perm_free * freq_ghz)
        perm_Real    = perm_real1 + perm_real2 + einf
        perm_imag    = perm_imag1 + perm_imag2 + perm_imag3
        permittivity = Cmplx(perm_Real,perm_imag,jprb)

        den1_tl         = 2 * fen_sq * f1 * f1_tl
        den2_tl         = 2 * fen_sq * f2 * f2_tl
        perm_real1_tl   = (den1 * del1_tl - del1 * den1_tl) / (den1 * den1)
        perm_real2_tl   = (den2 * del2_tl - del2 * den2_tl) / (den2 * den2)
        perm_imag1_tl   = fen * ( den1 * ( del1_tl * f1 + del1 * f1_tl)&
              & - (del1 * f1 * den1_tl) )  / (den1 * den1)
        perm_imag2_tl   = fen * ( den2 * ( del2_tl * f2 + del2 * f2_tl)&
              & - (del2 * f2 * den2_tl) )  / (den2 * den2)
        sigma_tl        = 0.09437_JPRB * tcelsius_tl
        perm_imag3_tl   = sigma_tl / (2.0_JPRB * c(20) * perm_free * freq_ghz)
        perm_Real_tl    = perm_real1_tl + perm_real2_tl + einf_tl
        perm_imag_tl    = perm_imag1_tl + perm_imag2_tl + perm_imag3_tl
        permittivity_tl = Cmplx(perm_Real_tl,perm_imag_tl,jprb)

        !-------------------------------------------------------------
        !1.3 calculate complex reflection coefficients and corrections
        !-------------------------------------------------------------


        !1.3.1) Fresnel reflection coefficients
        !------

        perm1          = sqrt(permittivity - geom%sinzen_sq)
        perm2          = permittivity * geom%coszen
        rhth           = (geom%coszen-perm1) / (geom%coszen+perm1)
        rvth           = (perm2-perm1) / (perm2+perm1)
        !    fresnel_v_real = dble(rvth)
        fresnel_v_Real = Real(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel_v      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel_h      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag


        perm1_tl          = 0.5_JPRB * permittivity_tl / perm1
        perm2_tl          = permittivity_tl * geom%coszen
        rhth_tl           = - 2 * geom%coszen * perm1_tl / (geom%coszen+perm1)**2
        rvth_tl           = 2 * (perm1 * perm2_tl - perm1_tl * perm2) / (perm2+perm1)**2
        !    fresnel_v_real_tl = dble(rvth_tl)
        fresnel_v_Real_tl = Real(rvth_tl)
        fresnel_v_imag_tl = Aimag(rvth_tl)
        fresnel_v_tl      = 2 * fresnel_v_Real * fresnel_v_Real_tl + &
              & 2 * fresnel_v_imag * fresnel_v_imag_tl
        !    fresnel_h_real_tl = dble(rhth_tl)
        fresnel_h_Real_tl = Real(rhth_tl)
        fresnel_h_imag_tl = Aimag(rhth_tl)
        fresnel_h_tl      = 2 * fresnel_h_Real * fresnel_h_Real_tl + &
              & 2 * fresnel_h_imag * fresnel_h_imag_tl

        !1.3.2) Small scale correction to reflection coefficients
        !------

        If (freq_ghz >= 15.0) Then
           small_rough_cor = Exp( c(21) * wind10 * geom % coszen_sq / (freq_ghz_sq) )
           small_rough_cor_tl = small_rough_cor * c(21) * wind10_tl * geom % coszen_sq / (freq_ghz_sq)
        Else
           small_rough_cor    = 1.0
           small_rough_cor_tl = 0.0
        End If

        !1.3.3) Large scale geometric correction
        !------

        !Point to correct coefficients for this version. There are 36 altogether.
        !Those for FASTEM-2 are stored in section 24:59 of the array, those for
        !FASTEM1 in section 60:95.
        If ( wanted_fastem_ver == 2 ) Then
           c => coef%fastem_coef(24:59)
        Else
           c => coef%fastem_coef(60:95)
        End If
        Do j = 1, 12
           zc(j) = c(j*3-2) + c(j*3-1)*freq_ghz + c(j*3)*freq_ghz_sq
        End Do
        !Point back to all coefficients again
        c => coef%fastem_coef

        large_rough_cor(1) = &
             & (zc(1)                  + &
              & zc(2) * geom%seczen    + &
              & zc(3) * geom%seczen_sq + &
              & zc(4) * wind10         + &
              & zc(5) * wind10_sq      + &
              & zc(6) * windsec) / 100._JPRB
        large_rough_cor(2) = &
             & (zc(7)                   + &
              & zc(8)  * geom%seczen    + &
              & zc(9)  * geom%seczen_sq + &
              & zc(10) * wind10         + &
              & zc(11) * wind10_sq      + &
              & zc(12) * windsec) / 100._JPRB
        !    large_rough_cor(:) = large_rough_cor(:) * 0.01

        large_rough_cor_tl(1) =   &
             & (zc(4) * wind10_tl     + &
              & zc(5) * wind10_sq_tl  + &
              & zc(6) * windsec_tl ) /100._JPRB
        large_rough_cor_tl(2) =      &
             & (zc(10) * wind10_tl    + &
              & zc(11) * wind10_sq_tl + &
              & zc(12) * windsec_tl) /100._JPRB

        ! For Fastem-3 do not compute rough surface effects if theta > 60 degrees
        If ( wanted_fastem_ver <= 2.0_JPRB .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) then
           emissstokes(i,1) = 1.0_JPRB - fresnel_v * small_rough_cor + large_rough_cor(1)
           emissstokes(i,2) = 1.0_JPRB - fresnel_h * small_rough_cor + large_rough_cor(2)
           emissstokes_tl(i,1) = - fresnel_v_tl * small_rough_cor    &
                 & - fresnel_v    * small_rough_cor_tl &
                 & + large_rough_cor_tl(1)
           emissstokes_tl(i,2) = - fresnel_h_tl * small_rough_cor    &
                 & - fresnel_h    * small_rough_cor_tl &
                 & + large_rough_cor_tl(2)
        Else
           emissstokes(i,1) = 1.0_JPRB - fresnel_v
                 emissstokes(i,2) = 1.0_JPRB - fresnel_h
                 emissstokes_tl(i,1) = - fresnel_v_tl
                 emissstokes_tl(i,2) = - fresnel_h_tl
        End If

        emissstokes(i,3) = 0.0_JPRB
        emissstokes(i,4) = 0.0_JPRB
        emissstokes_tl(i,3) = 0.0_JPRB
        emissstokes_tl(i,4) = 0.0_JPRB

        !Apply foam correction
        foam_cor  = c(22) * ( wind10 ** c(23) )
        foam_cor_tl  =  c(22) * c(23) * wind10_tl * ( wind10 ** (c(23)-1.0_JPRB) )


        ! Be careful do TL first because the next 2 lines of the direct model
        ! have variables in input/output of the statement

        emissstokes_tl(i,1) = emissstokes_tl(i,1)-foam_cor_tl*emissstokes(i,1)-foam_cor*emissstokes_tl(i,1)+foam_cor_tl
        emissstokes_tl(i,2) = emissstokes_tl(i,2)-foam_cor_tl*emissstokes(i,2)-foam_cor*emissstokes_tl(i,2)+foam_cor_tl
        emissstokes(i,1) = emissstokes(i,1) - foam_cor*emissstokes(i,1) + foam_cor
        emissstokes(i,2) = emissstokes(i,2) - foam_cor*emissstokes(i,2) + foam_cor

        If ( wanted_fastem_ver == 3) then
           ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)
           ! Assume 19m wind = 10m wind for now (fix later)
           u19=wind10
           if (prof % s2m % u >= 0.0_JPRB .AND. prof % s2m % v >= 0.0_JPRB) iquadrant=1
           if (prof % s2m % u >= 0.0_JPRB .AND. prof % s2m % v < 0.0_JPRB) iquadrant=2
           if (prof % s2m % u < 0.0_JPRB .AND. prof % s2m % v >= 0.0_JPRB) iquadrant=4
           if (prof % s2m % u < 0.0_JPRB .AND. prof % s2m % v < 0.0_JPRB) iquadrant=3

           if (abs(prof % s2m % v) >= 0.0001_JPRB) then
              windratio=prof % s2m % u/prof % s2m % v
           else
              windratio=0.0_JPRB
              if (abs(prof % s2m % u) > 0.0001_JPRB) then
                 windratio=999999.0_JPRB*prof % s2m % u
              endif
           endif

           windangle=atan(windratio)
           wind10_direction = quadcof(iquadrant,1)*pi+windangle*quadcof(iquadrant,2)
           windratio_tl = 0.0_JPRB

           If (abs(prof % s2m % v) >= 0.0001_JPRB) then
              windratio_tl=(prof%s2m%v * prof_tl%s2m%u - prof_tl%s2m%v * prof%s2m%u)/&
                         & (prof % s2m % v *prof % s2m % v)
           Else
              windratio_tl=0.0_JPRB
              if (abs(prof % s2m % u) > 0.0001_JPRB) then
                 windratio_tl=999999.0_JPRB*prof_tl % s2m % u
              endif
           Endif

           windangle_tl = windratio_tl/(1.0_JPRB+windratio*windratio)
           wind10_direction_tl = windangle_tl*quadcof(iquadrant,2)
           ! Angle between wind direction and satellite azimuthal view angle
           phi = pi - wind10_direction + prof % azangle*pi/180.0_JPRB
           phi_tl = -1.0_JPRB * wind10_direction_tl
           u19_tl = wind10_tl
           tbfixed(:,:,:)=0.0_JPRB
           tbfixed_tl(:,:,:)=0.0_JPRB
           Do ich = 0,15
              a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))
              a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))
              a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))
              a1e_tl = u19_tl*(c(142+ich*12)+u19*(2.0*c(143+ich*12)+3.0*u19*c(144+ich*12)))
              a2e_tl = u19_tl*(c(146+ich*12)+u19*(2.0*c(147+ich*12)+3.0*u19*c(148+ich*12)))
              a3e_tl = u19_tl*(c(150+ich*12)+u19*(2.0*c(151+ich*12)+3.0*u19*c(152+ich*12)))
              i_freq = int(ich/4) + 1   ! 37, 19, 10, 7 GHz
              j_stokes = mod(ich,4) + 1
              tbfixed(j_stokes,i_freq,1) = a1e
              tbfixed(j_stokes,i_freq,2) = a2e
              tbfixed(j_stokes,i_freq,3) = a3e
              tbfixed_tl(j_stokes,i_freq,1) = a1e_tl
              tbfixed_tl(j_stokes,i_freq,2) = a2e_tl
              tbfixed_tl(j_stokes,i_freq,3) = a3e_tl
           End Do
           efixed_tl(:,:,:)=0.0_JPRB
           einterpolated_tl(:,:)=0.0_JPRB

           Do M=1,3
              Do istokes=1,4
                efixed(1,istokes,M)= tbfixed(istokes,4,M)  ! 7   GHz
                efixed(2,istokes,M)= tbfixed(istokes,3,M)  ! 10  GHz
                efixed(3,istokes,M)= tbfixed(istokes,2,M)  ! 19  GHz
                efixed(4,istokes,M)= tbfixed(istokes,1,M)  ! 37  GHz
                efixed_tl(1,istokes,M)= tbfixed_tl(istokes,4,M)  ! 7  GHz
                efixed_tl(2,istokes,M)= tbfixed_tl(istokes,3,M)  ! 10  GHz
                efixed_tl(3,istokes,M)= tbfixed_tl(istokes,2,M)  ! 19  GHz
                efixed_tl(4,istokes,M)= tbfixed_tl(istokes,1,M)  ! 37  GHz
              End Do

              ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz
              If (freq_ghz.le.freqfixed(1)) Then
                einterpolated(:,M)=efixed(1,:,M)
                einterpolated_tl(:,M)=efixed_tl(1,:,M)
              Else If(freq_ghz.ge.freqfixed(4)) then
                einterpolated(:,M)=efixed(4,:,M)
                einterpolated_tl(:,M)=efixed_tl(4,:,M)
              Else
                If(freq_ghz.lt.freqfixed(2)) ifreq=2
                If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
                If(freq_ghz.ge.freqfixed(3)) ifreq=4
                dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
                einterpolated(:,M)=efixed(ifreq-1,:,M)+dfreq*(efixed(ifreq,:,M)-efixed(ifreq-1,:,M))
                einterpolated_tl(:,M)=efixed_tl(ifreq-1,:,M)+dfreq*(efixed_tl(ifreq,:,M)-efixed_tl(ifreq-1,:,M))
              End If
           End Do

           Do istokes = 1,4
              azimuthal_emiss=0.0_JPRB
              azimuthal_emiss_tl=0.0_JPRB
              Do M=1,3
                 If(istokes.le.2) Then
                    azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*&
                    &(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                    azimuthal_emiss_tl=azimuthal_emiss_tl+(einterpolated_tl(istokes,M)*cos(m*phi) -&
                      & einterpolated(istokes,M)*m*sin(m*phi)*phi_tl)*(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                 Else
                    azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*(1.0_JPRB-geom%coszen)/&
                    &(1.0_JPRB - 0.6018_JPRB)
                    azimuthal_emiss_tl=azimuthal_emiss_tl+(einterpolated_tl(istokes,M)*sin(m*phi) +&
                      & einterpolated(istokes,M)*m*cos(m*phi)*phi_tl)*(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                 End If
              End Do
              emissstokes(i,istokes)=emissstokes(i,istokes)+azimuthal_emiss
              emissstokes_tl(i,istokes)=emissstokes_tl(i,istokes)+azimuthal_emiss_tl
           End Do
        End If

! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((wanted_fastem_ver == 2 .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) .And. &
             & transmission % tau_surf(ichannel) < 0.9999_JPRB .And. transmission % tau_surf(ichannel) > 0.00001_JPRB ) Then

           !Convert windspeed to slope variance using the Cox and Munk model
           variance = 0.00512_JPRB * wind10 + 0.0030_JPRB
           varm     = variance * c(138)
           variance = varm * ( c(139) * freq_ghz + c(140) )

           variance_tl = 0.00512_JPRB * wind10_tl
           varm_tl     = variance_tl * c(138)
           variance_tl = varm_tl * ( c(139) * freq_ghz + c(140) )

           If ( variance > varm ) Then
              variance    = varm
              variance_tl = varm_tl
           Endif
           If ( variance < 0.0_JPRB  ) Then
              variance    = 0.0_JPRB
              variance_tl = 0.0_JPRB
           Endif

           !Compute surface to space optical depth
           opdpsfc    = -log(transmission % tau_surf(ichannel)) / geom%seczen
           opdpsfc_tl = -transmission_tl % tau_surf(ichannel) / ( transmission % tau_surf(ichannel) * geom%seczen )

           !Define nine predictors for the effective angle calculation
           zx(1) = 1.0_JPRB
           zx(2) = variance
           zx(4) = 1.0_JPRB / geom%coszen
           zx(3) = zx(2) * zx(4)
           zx(5) = zx(3) * zx(3)
           zx(6) = zx(4) * zx(4)
           zx(7) = zx(2) * zx(2)
           zx(8) = log(opdpsfc)
           zx(9) = zx(8) * zx(8)

           zx_tl(1) = 0._JPRB
           zx_tl(2) = variance_tl
           zx_tl(4) = 0._JPRB
           zx_tl(3) = zx_tl(2) * zx(4)
           zx_tl(5) = 2 * zx_tl(3) * zx(3)
           zx_tl(6) = 2 * zx_tl(4) * zx(4)
           zx_tl(7) = 2 * zx_tl(2) * zx(2)
           zx_tl(8) = opdpsfc_tl / opdpsfc
           zx_tl(9) = 2 * zx_tl(8) * zx(8)

           zrough_v = 1.0_JPRB
           zrough_h = 1.0_JPRB

           zrough_v_tl = 0._JPRB
           zrough_h_tl = 0._JPRB

           Do jcof = 1,7
              jcofm1 = jcof-1
              !Switched h to v Deblonde SSMIS june 7, 2001
              zrough_h = zrough_h + &
                   & zx(jcof) * ( c(96+jcofm1*3) &
                   & + zx(8)    *   c(97+jcofm1*3) &
                   & + zx(9)    *   c(98+jcofm1*3) )
              zrough_v = zrough_v + &
                   & zx(jcof) * ( c(117+jcofm1*3) &
                   & + zx(8)    *   c(118+jcofm1*3) &
                   & + zx(9)    *   c(119+jcofm1*3) )

              zrough_h_tl = zrough_h_tl +         &
                   & zx(jcof)     * (                  &
                   & zx_tl(8)  *   c(97+jcofm1*3)   &
                   & + zx_tl(9)  *   c(98+jcofm1*3) ) &
                   & +  zx_tl(jcof) * ( c(96+jcofm1*3)   &
                   & + zx(8)  *   c(97+jcofm1*3)   &
                   & + zx(9)  *   c(98+jcofm1*3) )
              zrough_v_tl = zrough_v_tl +          &
                   & zx(jcof)     * (                   &
                   & zx_tl(8)  *   c(118+jcofm1*3)   &
                   & + zx_tl(9)  *   c(119+jcofm1*3) ) &
                   & +  zx_tl(jcof) * ( c(117+jcofm1*3)   &
                   & + zx(8)  *   c(118+jcofm1*3)   &
                   & + zx(9)  *   c(119+jcofm1*3) )
           End Do

           zreflmod_v = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v)/(1.0_JPRB-transmission % tau_surf(ichannel))
           zreflmod_h = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h)/(1.0_JPRB-transmission % tau_surf(ichannel))

           zreflmod_v_tl = transmission_tl % tau_surf(ichannel) *&
                 & (-zrough_v * transmission % tau_surf(ichannel)**(zrough_v-1.0_JPRB) * &
                 & (1.0_JPRB-transmission % tau_surf(ichannel))+&
                          & ( 1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v)) &
                 & / (1.0_JPRB-transmission % tau_surf(ichannel))**2

           zreflmod_v_tl = zreflmod_v_tl - &
                & ( transmission % tau_surf(ichannel)**zrough_v * Log(transmission % tau_surf(ichannel)) * zrough_v_tl ) / &
                & (1.0_JPRB-transmission % tau_surf(ichannel))

           zreflmod_h_tl = transmission_tl % tau_surf(ichannel) *&
                & (-zrough_h * transmission % tau_surf(ichannel)**(zrough_h-1.0) * (1.0-transmission % tau_surf(ichannel)) +  &
                &   ( 1.0-transmission % tau_surf(ichannel)**zrough_h)    ) &
                & / (1.0-transmission % tau_surf(ichannel))**2
           zreflmod_h_tl = zreflmod_h_tl - &
                & ( transmission % tau_surf(ichannel)**zrough_h * Log(transmission % tau_surf(ichannel)) * zrough_h_tl ) / &
                & (1.0-transmission % tau_surf(ichannel))

           reflectstokes_tl(i,1)  = zreflmod_v_tl * (1.0-emissstokes(i,1)) - zreflmod_v * emissstokes_tl(i,1)
           reflectstokes_tl(i,2)  = zreflmod_h_tl * (1.0-emissstokes(i,2)) - zreflmod_h * emissstokes_tl(i,2)
!	   zreflmod_v_tl = 0.0
!	   zreflmod_h_tl = 0.0
           reflectstokes_tl(i,3)  = -0.5_JPRB * ((zreflmod_v_tl + zreflmod_h_tl) * emissstokes(i,3) + &
	        & (zreflmod_v + zreflmod_h) * emissstokes_tl(i,3))
           reflectstokes_tl(i,4)  = -0.5_JPRB * ((zreflmod_v_tl + zreflmod_h_tl) * emissstokes(i,4) + &
	        & (zreflmod_v + zreflmod_h) * emissstokes_tl(i,4))
!           reflectstokes_tl(i,3)  =	 -emissstokes_tl(i,3)   
!           reflectstokes_tl(i,4)  =      -emissstokes_tl(i,4)
       Else
           reflectstokes_tl(i,:) = - emissstokes_tl(i,:)
       End If

        !--------------------
        !2. Land/ice surfaces
        !--------------------

     Else

        !Coherent surface scattering model coefficients (input with the profile)
        perm_static   = prof % skin % fastem(1)
        perm_infinite = prof % skin % fastem(2)
        freqr         = prof % skin % fastem(3)
        small_rough   = prof % skin % fastem(4)
        large_rough   = prof % skin % fastem(5)
        chan          = channels(i)
        freq_ghz      = coef % frequency_ghz(chan)

        perm_static_tl   = prof_tl % skin % fastem(1)
        perm_infinite_tl = prof_tl % skin % fastem(2)
        freqr_tl         = prof_tl % skin % fastem(3)
        small_rough_tl   = prof_tl % skin % fastem(4)
        large_rough_tl   = prof_tl % skin % fastem(5)

        !Simple Debye + Fresnel model gives reflectivities
        fen       = freq_ghz / freqr
        fen_sq    = fen * fen
        den1      = 1.0_JPRB + fen_sq
        perm_Real = (perm_static+perm_infinite*fen_sq) / den1
        perm_imag = fen*(perm_static-perm_infinite)    / den1
        permittivity = Cmplx(perm_Real,perm_imag,jprb)
        perm1     = sqrt(permittivity - geom%sinzen_sq)
        perm2     = permittivity * geom%coszen
        rhth      = (geom%coszen - perm1) / (geom%coszen + perm1)
        rvth      = (perm2 - perm1)/(perm2 + perm1)
        !    fresnel_v_real = dble(rvth)
        fresnel_v_Real = Real(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel_v      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel_h      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag

        fen_tl       = -freq_ghz * freqr_tl / freqr**2
        fen_sq_tl    = 2 * fen_tl * fen
        den1_tl      = fen_sq_tl
        perm_Real_tl = &
              & ( den1 * (perm_static_tl + perm_infinite_tl*fen_sq + perm_infinite*fen_sq_tl) -&
                & den1_tl * (perm_static + perm_infinite * fen_sq)) / (den1*den1)

        perm_imag_tl = ( den1 * ( fen_tl * (perm_static - perm_infinite)      + &
                                 & fen * (perm_static_tl - perm_infinite_tl))  - &
                       & den1_tl *  fen * (perm_static - perm_infinite)      ) / &
                       & (den1*den1)
        permittivity_tl   = Cmplx(perm_Real_tl, perm_imag_tl,jprb)
        perm1_tl          = 0.5_JPRB * permittivity_tl / perm1
        perm2_tl          = permittivity_tl * geom%coszen
        rhth_tl           = - 2 * geom%coszen * perm1_tl / (geom%coszen+perm1)**2
        rvth_tl           = 2 * (perm1 * perm2_tl - perm1_tl * perm2) / (perm2+perm1)**2
        !    fresnel_v_real_tl = dble(rvth_tl)
        fresnel_v_Real_tl = Real(rvth_tl)
        fresnel_v_imag_tl = Aimag(rvth_tl)
        fresnel_v_tl      = 2 * fresnel_v_Real * fresnel_v_Real_tl + &
              & 2 * fresnel_v_imag * fresnel_v_imag_tl
        !    fresnel_h_real_tl = dble(rhth_tl)
        fresnel_h_Real_tl = Real(rhth_tl)
        fresnel_h_imag_tl = Aimag(rhth_tl)
        fresnel_h_tl      = 2 * fresnel_h_Real * fresnel_h_Real_tl + &
              & 2 * fresnel_h_imag * fresnel_h_imag_tl


        !Small scale roughness correction
        delta           = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * small_rough
        delta2          = delta * delta
        small_rough_cor = Exp(-delta2*geom%coszen_sq)

        delta_tl           = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * small_rough_tl
        delta2_tl          = 2 * delta * delta_tl
        small_rough_cor_tl = -delta2_tl*geom%coszen_sq * small_rough_cor

        !Large scale roughness correction
        qdepol = 0.35_JPRB - 0.35_JPRB*Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        qdepol_tl =  -0.35_JPRB * (-0.60_JPRB*freq_ghz*2*large_rough_tl*large_rough) *&
              & Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        emissfactor_v = 1.0_JPRB - fresnel_v * small_rough_cor
        emissfactor_h = 1.0_JPRB - fresnel_h * small_rough_cor
        emissfactor   = emissfactor_h - emissfactor_v

        emissstokes(i,1)    = emissfactor_v + qdepol * emissfactor
        emissstokes(i,2)    = emissfactor_h - qdepol * emissfactor
        emissstokes(i,3) = 0.0_JPRB
        emissstokes(i,4) = 0.0_JPRB

        !reflect_v(i)  = 1.0_JPRB - emiss_v(i)
        !reflect_h(i)  = 1.0_JPRB - emiss_h(i)

        emissfactor_v_tl = - fresnel_v_tl * small_rough_cor - fresnel_v * small_rough_cor_tl
        emissfactor_h_tl = - fresnel_h_tl * small_rough_cor - fresnel_h * small_rough_cor_tl
        emissfactor_tl   = emissfactor_h_tl - emissfactor_v_tl
        emissstokes_tl(i,1)    = emissfactor_v_tl + qdepol_tl * emissfactor + qdepol * emissfactor_tl
        emissstokes_tl(i,2)    = emissfactor_h_tl - qdepol_tl * emissfactor - qdepol * emissfactor_tl
        emissstokes_tl(i,3)    = 0.0_JPRB
        emissstokes_tl(i,4)    = 0.0_JPRB

        reflectstokes_tl(i,:)  = - emissstokes_tl(i,:)
      End If

     ! Now return only required polarisations - either the calculated vector (V, H, or full Stokes)
     If (pol_id <= 3 .or. pol_id >= 6) then
        Do ich=1,polarisations(i,3)
           emissivity_tl(ichannel+ich-1)=emissstokes_tl(i,ich)
           reflectivity_tl(ichannel+ich-1)=reflectstokes_tl(i,ich)
         End Do
     End If
     ! Or V-pol only
     If (pol_id == 4) then
        emissivity_tl(ichannel)=emissstokes_tl(i,1)
        reflectivity_tl(ichannel)=reflectstokes_tl(i,1)
     End If
     ! Or H-pol only
     If (pol_id == 5) then
        emissivity_tl(ichannel)=emissstokes_tl(i,2)
        reflectivity_tl(ichannel)=reflectstokes_tl(i,2)
     End If

  End Do

End Subroutine rttov_calcemis_mw_tl
