!
Subroutine rttov_calcemis_mw_k ( &
     & profiles,        &! in
     & profiles_k,      &! inout
     & geometry,        &! in
     & coef,            &! in
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & transmission,    &! in
     & transmission_k,  &! inout
     & calcemis,        &! in
     & emissivity_k,    &! inout
     & reflectivity_k  ) ! inout
  ! Description:
  ! K matrix of rttov_calcemis_mw
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
  ! FASTEM-3 English 2003
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Comments added (R Saunders)
  !  1.2       26/09/2003  Polarimetric code and Fastem-3 (S. English)!
  !  1.3       18/08/2004  Corrected bug in K code (S English)
  !  1.4       29/03/2005  Add end of header comment (J. Cameron)
  !  1.5       14/10/2005  Reintroduce -r 122:123 changes, see -r 133:134
  !                        Fixing bug in azimuith angles > 270. (J Cameron)
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
  Integer(Kind=jpim),      Intent(in)            :: nfrequencies
  Integer(Kind=jpim),      Intent(in)            :: nprofiles
  Integer(Kind=jpim),      Intent(in)            :: nchannels
  Type(profile_Type),      Intent(in) ,Target    :: profiles(nprofiles)
  Type(geometry_Type),     Intent(in) ,Target    :: geometry(nprofiles)
  Type(rttov_coef),        Intent(in)            :: coef
  Integer(Kind=jpim),      Intent(in)            :: channels(nfrequencies)
  Integer(Kind=jpim),      Intent(in)            :: polarisations(nchannels,3)
  Integer(Kind=jpim),      Intent(in)            :: lprofiles(nfrequencies)
  Type(transmission_Type), Intent(in)            :: transmission
  Logical,                 Intent(in)            :: calcemis(nchannels)

  Type(profile_Type),      Intent(inout) ,Target :: profiles_k(nchannels)
  Type(transmission_Type), Intent(inout)         :: transmission_k
  Real(Kind=jprb),         Intent(inout)         :: emissivity_k(nchannels)
  Real(Kind=jprb),         Intent(inout)         :: reflectivity_k(nchannels)

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
  Real(Kind=jprb) :: fresnel(4)
  Real(Kind=jprb) :: small_rough_cor,foam_cor(4)
  Real(Kind=jprb) :: large_rough_cor(4)
  Real(Kind=jprb) :: small_rough,large_rough
  Real(Kind=jprb) :: emiss_save(4)
  Real(Kind=jprb) :: variance,varm
  Real(Kind=jprb) :: wind10
  Real(Kind=jprb) :: wind10_sq,windsec
  Real(Kind=jprb) :: wind10_direction, windangle, windratio ! Note wind azimuth is in radians
  Real(Kind=jprb) :: emissstokes(nfrequencies,4)
  Real(Kind=jprb) :: emissstokes_k(nfrequencies,4)
  Real(Kind=jprb) :: reflectstokes_k(nfrequencies,4)
  Real(Kind=jprb) :: u19,phi,dfreq
  Real(Kind=jprb) :: tbfixed(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed(4,4,3)    ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e,a2e,a3e !,ac,a2c,a3c     ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb) :: opdpsfc,freqr
  Real(Kind=jprb) :: zrough_v,zrough_h
  Real(Kind=jprb) :: zreflmod_v,zreflmod_h
  Real(Kind=jprb) :: delta,delta2
  Real(Kind=jprb) :: qdepol,emissfactor
  Real(Kind=jprb) :: emissfactor_v,emissfactor_h
  Real(Kind=jprb) :: zc(12),zx(9)
  Real(Kind=jprb), Pointer :: c(:)
  Complex(Kind=jprb) :: perm1,perm2
  Complex(Kind=jprb) :: rhth,rvth
  Complex(Kind=jprb) :: permittivity
  Integer(Kind=jpim) :: i,j,chan,istokes,ifreq,m
  Integer(Kind=jpim) :: iquadrant    ! Determines which quadrant (NE, SE, SW, NW) the wind is blowing to
  Integer(Kind=jpim) :: pol_id  ! polarisation indice
  Integer(Kind=jpim) :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  Integer(Kind=jpim) :: jcof,jcofm1
  Type(profile_Type),  Pointer    :: prof
  Type(profile_Type),  Pointer    :: prof_k
  Type(geometry_Type), Pointer    :: geom

  Real(Kind=jprb) :: tcelsius_k(4)
  Real(Kind=jprb) :: tcelsius_sq_k(4)
  Real(Kind=jprb) :: tcelsius_cu_k(4)
  Real(Kind=jprb) :: f1_k(4), f2_k(4)
  Real(Kind=jprb) :: del1_k(4), del2_k(4)
  Real(Kind=jprb) :: einf_k(4)
  Real(Kind=jprb) :: fen_k(4), fen_sq_k(4)
  Real(Kind=jprb) :: den1_k(4), den2_k(4)
  Real(Kind=jprb) :: sigma_k(4)
  Real(Kind=jprb) :: perm_real1_k(4), perm_real2_k(4)
  Real(Kind=jprb) :: perm_imag1_k(4), perm_imag2_k(4), perm_imag3_k(4)
  Real(Kind=jprb) :: perm_Real_k(4), perm_imag_k(4)
  Real(Kind=jprb) :: perm_static_k(4), perm_infinite_k(4)
  Real(Kind=jprb) :: fresnel_v_Real_k, fresnel_v_imag_k
  Real(Kind=jprb) :: fresnel_h_Real_k, fresnel_h_imag_k
  Real(Kind=jprb) :: fresnel_v_k, fresnel_h_k
  Real(Kind=jprb) :: small_rough_cor_k(4), foam_cor_k(4)
  Real(Kind=jprb) :: large_rough_cor_k(4)
  Real(Kind=jprb) :: small_rough_k(4), large_rough_k(4)
  Real(Kind=jprb) :: variance_k(4), varm_k(4)
  Real(Kind=jprb) :: wind10_k(4)
  Real(Kind=jprb) :: wind10_sq_k(4), windsec_k(4)
  Real(Kind=jprb) :: wind10_direction_k(4), windangle_k(4), windratio_k(4) ! Note wind azimuth is in radians
  Real(Kind=jprb) :: azimuthal_emiss_k,azimuthal_emiss,u19_k(4),phi_k(4)
  Real(Kind=jprb) :: tbfixed_k(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed_k(4,4,3)   ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated_k(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e_k,a2e_k,a3e_k     ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb) :: opdpsfc_k(4), freqr_k(4)
  Real(Kind=jprb) :: zrough_v_k, zrough_h_k, zrough_3v_k, zrough_4v_k, zrough_3h_k, zrough_4h_k
  Real(Kind=jprb) :: zreflmod_v_k, zreflmod_h_k, zreflmod_3_k, zreflmod_4_k
  Real(Kind=jprb) :: delta_k(4), delta2_k(4)
  Real(Kind=jprb) :: qdepol_k(4), emissfactor_k
  Real(Kind=jprb) :: emissfactor_v_k, emissfactor_h_k
  Real(Kind=jprb) :: zx_k(9,4)
  Complex(Kind=jprb) :: perm1_k(4), perm2_k(4)
  Complex(Kind=jprb) :: rhth_k, rvth_k
  Complex(Kind=jprb) :: permittivity_k(4)
  Integer(Kind=jpim) :: wanted_fastem_ver  ! user fastem version request
  Integer(Kind=jpim) :: ipol, npol

  Real(Kind=jprb) :: test_variance

  !- End of header --------------------------------------------------------

  ! If the coefficent file contains FASTEM 2 it contains also FASTEM 1 but
  ! the version choosen is given by coef % fastem_ver value
  wanted_fastem_ver = coef % fastem_ver

  !If a TL value of emissivity is passed to the routine
  !Loop over channels

  phi_k(:)=0.0_JPRB
  efixed_k(:,:,:)=0.0_JPRB

  Do i = 1, nfrequencies
     ichannel=polarisations(i,1)
     ipol = polarisations(i,1)
     npol = polarisations(i,3)
     If ( .Not. calcemis(ichannel) ) Cycle

     chan        = channels(i)
     prof        => profiles( lprofiles(i) )
     prof_k      => profiles_k( (i) )
     geom        => geometry( lprofiles(i) )

     !-------------------------------
     !0. Point to fastem coefficients
     !-------------------------------

     c => coef % fastem_coef

     pol_id = coef % fastem_polar(chan) + 1
     Do Ich=1, 4
        reflectstokes_k(i,ich) = 0.0_JPRB
        emissstokes_k(i,ich)   = 0.0_JPRB
     End Do

     If (pol_id <= 3 .or. pol_id >= 6) then
        Do Ich=1, polarisations(i,3)
           reflectstokes_k(i,ich) = reflectivity_k(ichannel+ich-1)
           emissstokes_k(i,ich)   = emissivity_k(ichannel+ich-1)
        End Do
     End If

     If (pol_id == 4) then
        reflectstokes_k(i,1) = reflectivity_k(ichannel)
        emissstokes_k(i,1)   = emissivity_k(ichannel)
     End If

     If (pol_id == 5) then
        reflectstokes_k(i,2) = reflectivity_k(ichannel)
        emissstokes_k(i,2)   = emissivity_k(ichannel)
     End If

     wind10_k(:) = 0.0_JPRB
     wind10_direction_k(:) = 0.0_JPRB

     !---------------
     !1. Sea surfaces
     !---------------

     If ( prof % skin % surftype == surftype_sea ) Then

        !-------------------------------------------
        !1.1 Calculate channel independent variables
        !-------------------------------------------
        wind10_sq =   prof % s2m % u * prof % s2m % u +&
              & prof % s2m % v * prof % s2m % v
        wind10    = Sqrt( wind10_sq )
        windsec   = wind10 * geom%seczen
        if (prof % s2m % u >= 0.0_JPRB .AND. prof % s2m % v >= 0.0_JPRB) iquadrant=1
        if (prof % s2m % u >= 0.0_JPRB .AND. prof % s2m % v < 0.0_JPRB) iquadrant=2
        if (prof % s2m % u < 0.0_JPRB .AND. prof % s2m % v >= 0.0_JPRB) iquadrant=4
        if (prof % s2m % u < 0.0_JPRB .AND. prof % s2m % v < 0.0_JPRB) iquadrant=3

        If (abs(prof % s2m % v) >= 0.0001_JPRB) then
          windratio=prof % s2m % u/prof % s2m % v
        Else
          windratio=0.0_JPRB
          If (abs(prof % s2m % u) > 0.0001_JPRB) then
            windratio=999999.0_JPRB*prof % s2m % u
          Endif
        Endif

        windangle=atan(windratio)
        wind10_direction = quadcof(iquadrant,1)*pi+windangle*quadcof(iquadrant,2)

        !Set values for temperature polynomials (convert from kelvin to celsius)
        tcelsius = prof % skin % t - 273.15_JPRB
        tcelsius_sq = tcelsius * tcelsius     !quadratic
        tcelsius_cu = tcelsius_sq * tcelsius  !cubic

        !Define two relaxation frequencies, f1 and f2
        f1 = c(1) + c(2) * tcelsius + c(3) * tcelsius_sq
        f2 = c(4) + c(5) * tcelsius + c(6) * tcelsius_sq + c(7) * tcelsius_cu

        !Static permittivity estatic = del1+del2+einf
        del1 = c(8)  + c(9)  * tcelsius + c(10) * tcelsius_sq + c(11) * tcelsius_cu
        del2 = c(12) + c(13) * tcelsius + c(14) * tcelsius_sq + c(15) * tcelsius_cu
        einf = c(18) + c(19) * tcelsius

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
        fresnel(1)      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel(2)      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag
        fresnel(3)      = 0.0_JPRB
        fresnel(4)      = 0.0_JPRB

        !1.3.2) Small scale correction to reflection coefficients
        !------

        If (freq_ghz >= 15.0) Then
           small_rough_cor = Exp( c(21) * wind10 * geom % coszen_sq / (freq_ghz_sq) )
        Else
           small_rough_cor = 1.0
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
        large_rough_cor(3) = 0.0_JPRB
        large_rough_cor(4) = 0.0_JPRB

        ! Introduce emiss_v_save and emiss_h_save arrays to be able
        ! to simplify further AD code
        Do Ich=1,4
           emiss_save(Ich) = 1.0_JPRB - fresnel(Ich) * small_rough_cor + large_rough_cor(Ich)
        End Do

        !Apply foam correction
        foam_cor(1)  = c(22) * ( wind10 ** c(23) )
        foam_cor(2)  = c(22) * ( wind10 ** c(23) )
        !Currently ignore foam effects on 3rd and 4th elements.
        foam_cor(3)  = 0.0_JPRB
        foam_cor(4)  = 0.0_JPRB

        Do Ich=1,4
           emissstokes(i,Ich) = emiss_save(Ich) - foam_cor(Ich)*emiss_save(Ich) + foam_cor(Ich)
        End Do
        emissstokes(i,3) = 0.0
        emissstokes(i,4) = 0.0

        ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((wanted_fastem_ver == 2 .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) .And. &
             & transmission % tau_surf(ichannel) < 0.9999_JPRB .And. transmission % tau_surf(ichannel) > 0.00001_JPRB ) Then

           !Convert windspeed to slope variance using the Cox and Munk model
           variance = 0.00512_JPRB * wind10 + 0.0030_JPRB
           varm     = variance * c(138)
           variance = varm * ( c(139) * freq_ghz + c(140) )

           test_variance = variance
           If ( variance > varm ) Then
              variance    = varm
           Endif
           If ( variance < 0.0_JPRB  ) Then
              variance    = 0.0_JPRB
           Endif

           !Compute surface to space optical depth
           opdpsfc    = -log(transmission % tau_surf(ichannel)) / geom%seczen

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

           zrough_v = 1.0_JPRB
           zrough_h = 1.0_JPRB

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
           End Do

           zreflmod_v = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v) /&
           & (1.0_JPRB-transmission % tau_surf(ichannel))
           zreflmod_h = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h) /&
           & (1.0_JPRB-transmission % tau_surf(ichannel))

           !reflect_v(i)  = zreflmod_v * (1.0-emiss_v(i))
           !reflect_h(i)  = zreflmod_h * (1.0-emiss_h(i))

        !Else
           !reflect_v(i) = 1.0 - emiss_v(i)
           !reflect_h(i) = 1.0 - emiss_h(i)

        End If

           !.......end of forward part....................................
           !
           ! * Now run K code of fastem
           !
        ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((wanted_fastem_ver == 2 .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) .And. &
             & transmission % tau_surf(ichannel) < 0.9999_JPRB .And. transmission % tau_surf(ichannel) > 0.00001_JPRB ) Then

        If ( wanted_fastem_ver == 3) then
          ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)
          ! Angle between wind direction and satellite azimuthal view angle
          ! Assume 19m wind = 10m wind for now (fix later).
          phi = pi - wind10_direction + prof % azangle*pi/180.0_JPRB
          u19=wind10
          Do ich = 0,15
             a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))
             a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))
             a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))

             i_freq = int(ich/4) + 1    ! 37, 19, 10, 7 GHz
             j_stokes = mod(ich,4) + 1
             tbfixed(j_stokes,i_freq,1) = a1e
             tbfixed(j_stokes,i_freq,2) = a2e
             tbfixed(j_stokes,i_freq,3) = a3e
          End Do

          Do M=1,3
             Do ifreq=1,4
                efixed(1,ifreq,M)= tbfixed(ifreq,4,M)  ! 7   GHz
                efixed(2,ifreq,M)= tbfixed(ifreq,3,M)  ! 10  GHz
                efixed(3,ifreq,M)= tbfixed(ifreq,2,M)  ! 19  GHz
                efixed(4,ifreq,M)= tbfixed(ifreq,1,M)  ! 37  GHz
             End Do

             ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz

             If (freq_ghz.le.freqfixed(1)) Then
                einterpolated(:,M)=efixed(1,:,M)
             Else If(freq_ghz.ge.freqfixed(4)) then
                einterpolated(:,M)=efixed(4,:,M)
             Else
                If(freq_ghz.lt.freqfixed(2)) ifreq=2
                If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
                If(freq_ghz.ge.freqfixed(3)) ifreq=4
                dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
                einterpolated(:,M)=efixed(ifreq-1,:,M)+dfreq*(efixed(ifreq,:,M)-efixed(ifreq-1,:,M))
             EndIf
          EndDo
          Do istokes = 1,4
             azimuthal_emiss=0.0_JPRB
             Do M=1,3
                If(istokes.le.2) Then
                   azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*(1.0_JPRB-geom%coszen)&
                  &/(1.0_JPRB - 0.6018_JPRB)
                Else
                   azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*(1.0_JPRB-geom%coszen)&
                  &/(1.0_JPRB - 0.6018_JPRB)
                End If
             End Do
             emissstokes(i,istokes)=emissstokes(i,istokes)+azimuthal_emiss
	     
          End Do
	EndIf  
	   zreflmod_v_k = reflectstokes_k(i,1) * (1.0_JPRB-emissstokes(i,1))
           zreflmod_h_k = reflectstokes_k(i,2) * (1.0_JPRB-emissstokes(i,2))
           zreflmod_3_k = - 1.0_JPRB * reflectstokes_k(i,3) * emissstokes(i,3) 
           zreflmod_4_k = - 1.0_JPRB * reflectstokes_k(i,4) * emissstokes(i,4) 

           emissstokes_k(i,4) = emissstokes_k(i,4) - 0.5_JPRB * (zreflmod_v + zreflmod_h) * reflectstokes_k(i,4)
           emissstokes_k(i,3) = emissstokes_k(i,3) - 0.5_JPRB * (zreflmod_v + zreflmod_h) * reflectstokes_k(i,3)
           emissstokes_k(i,2) = emissstokes_k(i,2) - reflectstokes_k(i,2) * zreflmod_h
           emissstokes_k(i,1) = emissstokes_k(i,1) - reflectstokes_k(i,1) * zreflmod_v
           zrough_v_k = -zreflmod_v_k * &
                 & ( transmission % tau_surf(ichannel)**zrough_v * Log(transmission % tau_surf(ichannel)) ) / &
                 & (1.0_JPRB-transmission % tau_surf(ichannel))

           zrough_h_k = - zreflmod_h_k * &
                 & ( transmission % tau_surf(ichannel)**zrough_h * Log(transmission % tau_surf(ichannel)) ) / &
                 & (1.0_JPRB-transmission % tau_surf(ichannel))

	   If (npol >= 2) Then
	   
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) + zreflmod_v_k *&
             & (-zrough_v * transmission % tau_surf(ipol)**(zrough_v-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_v)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2
	     
              transmission_k % tau_surf(ipol+1) = transmission_k % tau_surf(ipol+1) + zreflmod_h_k *&
             & (-zrough_h * transmission % tau_surf(ipol)**(zrough_h-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_h)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2

	   EndiF
	   
	   If (npol >= 3) Then
	   
                  zrough_3v_k = - zreflmod_3_k * &
            & (0.5* (transmission % tau_surf(ichannel+2)**zrough_v) &
	     * Log(transmission % tau_surf(ichannel+2)) ) / (1.0_JPRB-transmission % tau_surf(ichannel+2))

                 zrough_4v_k = -zreflmod_4_k * &
            & (0.5*(transmission % tau_surf(ichannel+3)**zrough_v) &
	    * Log(transmission % tau_surf(ichannel+3)) ) / (1.0_JPRB-transmission % tau_surf(ichannel+3))

                 zrough_3h_k = - zreflmod_3_k * &
            & (0.5* (transmission % tau_surf(ichannel+2)**zrough_h) &
	     * Log(transmission % tau_surf(ichannel+2)) ) / (1.0_JPRB-transmission % tau_surf(ichannel+2))

                 zrough_4h_k = -zreflmod_4_k * &
            & (0.5*( transmission % tau_surf(ichannel+3)**zrough_h) &
	    * Log(transmission % tau_surf(ichannel+3)) ) / (1.0_JPRB-transmission % tau_surf(ichannel+3))


                transmission_k % tau_surf(ipol+2) = transmission_k % tau_surf(ipol+2) + 0.5_JPRB * zreflmod_3_k *&
             & (-zrough_h * transmission % tau_surf(ipol)**(zrough_h-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_h)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2

                 transmission_k % tau_surf(ipol+2) = transmission_k % tau_surf(ipol+2) + 0.5_JPRB * zreflmod_3_k *&
             & (-zrough_v * transmission % tau_surf(ipol)**(zrough_v-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_v)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2
                 transmission_k % tau_surf(ipol+3) = transmission_k % tau_surf(ipol+3) + 0.5_JPRB * zreflmod_4_k *&
             & (-zrough_h * transmission % tau_surf(ipol)**(zrough_h-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_h)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2
	     
                 transmission_k % tau_surf(ipol+3) = transmission_k % tau_surf(ipol+3) + 0.5_JPRB * zreflmod_4_k *&
             & (-zrough_v * transmission % tau_surf(ipol)**(zrough_v-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ipol)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ipol)**zrough_v)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ipol))**2
           End If
           If (npol == 1 .and. pol_id == 5) Then
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) + zreflmod_h_k *&
             & (-zrough_h * transmission % tau_surf(ichannel)**(zrough_h-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ichannel)) +  &
             &   ( 1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h)           ) &
             & / (1.0_JPRB-transmission % tau_surf(ichannel))**2
           End If

           If (npol == 1 .and. pol_id == 4) Then
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) + zreflmod_v_k *&
             & (-zrough_v * transmission % tau_surf(ichannel)**(zrough_v-1.0_JPRB) * &
             &                       (1.0_JPRB-transmission % tau_surf(ichannel)) +  &
             &      ( 1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v)        ) &
             & / (1.0_JPRB-transmission % tau_surf(ichannel))**2
           End If
          zx_k(:,:) = 0.0_JPRB
           Do jcof = 1,7
              jcofm1 = jcof-1
              !Switched h to v Deblonde SSMIS june 7, 2001

              zx_k(9,1) = zx_k(9,1) + zrough_v_k * zx(jcof) * c(119+jcofm1*3)
              zx_k(8,1) = zx_k(8,1) + zrough_v_k * zx(jcof) * c(118+jcofm1*3)
              zx_k(jcof,1) = zrough_v_k *&
           & (    c(117+jcofm1*3)   &
           &  + zx(8) * c(118+jcofm1*3)   &
           &  + zx(9) * c(119+jcofm1*3) )

              zx_k(9,2) = zx_k(9,2) + zrough_h_k * zx(jcof) * c(98+jcofm1*3)
              zx_k(8,2) = zx_k(8,2) + zrough_h_k * zx(jcof) * c(97+jcofm1*3)
              zx_k(jcof,2) = zrough_h_k *&
           & (       c(96+jcofm1*3)   &
           &  + zx(8)  *   c(97+jcofm1*3)   &
              + zx(9)  *   c(98+jcofm1*3) )
	      
 	      If (npol >= 3) Then
                 zx_k(9,3) = zx_k(9,3) +  zrough_3v_k * zx(jcof) * c(119+jcofm1*3)
                 zx_k(8,3) = zx_k(8,3) +  zrough_3v_k * zx(jcof) * c(118+jcofm1*3)
                 zx_k(jcof,3) =  zrough_3v_k *&
           & (    c(117+jcofm1*3)   &
           &  + zx(8) * c(118+jcofm1*3)   &
           &  + zx(9) * c(119+jcofm1*3) )

                 zx_k(9,3) = zx_k(9,3) +  zrough_3h_k * zx(jcof) * c(98+jcofm1*3)
                 zx_k(8,3) = zx_k(8,3) +  zrough_3h_k * zx(jcof) * c(97+jcofm1*3)
                 zx_k(jcof,3) = zx_k(jcof,3) + zrough_3h_k *&
           & (       c(96+jcofm1*3)   &
           &  + zx(8)  *   c(97+jcofm1*3)   &
              + zx(9)  *   c(98+jcofm1*3) )
	      
                 zx_k(9,4) = zx_k(9,4) +  zrough_4v_k * zx(jcof) * c(119+jcofm1*3)
                 zx_k(8,4) = zx_k(8,4) +  zrough_4v_k * zx(jcof) * c(118+jcofm1*3)
                 zx_k(jcof,4) =  zrough_4v_k *&
           & (    c(117+jcofm1*3)   &
           &  + zx(8) * c(118+jcofm1*3)   &
           &  + zx(9) * c(119+jcofm1*3) )

                 zx_k(9,4) = zx_k(9,4) +  zrough_4h_k * zx(jcof) * c(98+jcofm1*3)
                 zx_k(8,4) = zx_k(8,4) +  zrough_4h_k * zx(jcof) * c(97+jcofm1*3)
                 zx_k(jcof,4) = zx_k(jcof,4) +  zrough_4h_k *&
           & (       c(96+jcofm1*3)   &
           &  + zx(8)  *   c(97+jcofm1*3)   &
              + zx(9)  *   c(98+jcofm1*3) )
	      EndIf   
           End Do
	   
           zrough_v_k = 0.0_JPRB
           zrough_h_k = 0.0_JPRB

           !Define nine predictors for the effective angle calculation

           zx_k(8,:) = zx_k(8,:) + zx_k(9,:) * 2 * zx(8)

           opdpsfc_k(:) = zx_k(8,:) / opdpsfc
           zx_k(2,:) = zx_k(2,:) + zx_k(7,:) * 2 * zx(2)

           zx_k(4,:) = zx_k(4,:) + zx_k(6,:) * 2 * zx(4)

           zx_k(3,:) = zx_k(3,:) + zx_k(5,:) * 2 * zx(3)

           zx_k(2,:) = zx_k(2,:) + zx_k(3,:) * zx(4)

           zx_k(4,:) = 0.0_JPRB

           variance_k(:) = zx_k(2,:)

           zx_k(1,:) = 0.0_JPRB

           !Compute surface to space optical depth
           If (npol >= 2) Then
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) - opdpsfc_k(1) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
              transmission_k % tau_surf(ipol+1) = transmission_k % tau_surf(ipol+1) - opdpsfc_k(2) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
           End If
           If (npol >= 3) Then
              transmission_k % tau_surf(ipol+2) = transmission_k % tau_surf(ipol+2) - opdpsfc_k(3) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
              transmission_k % tau_surf(ipol+3) = transmission_k % tau_surf(ipol+3) - opdpsfc_k(4) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
           End If
           If (npol == 1 .and. pol_id == 5) Then
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) - opdpsfc_k(2) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
           End If
           If (npol == 1 .and. pol_id == 4) Then
              transmission_k % tau_surf(ipol) = transmission_k % tau_surf(ipol) - opdpsfc_k(1) /&
              & ( transmission % tau_surf(ichannel) * geom%seczen )
           End If

           If ( test_variance < varm ) Then
              varm_k(:) = variance_k(:) * ( c(139) * freq_ghz + c(140) )
           Else
              varm_k(:) = variance_k(:)
           Endif

           variance_k(:) = varm_k(:) * c(138)
           wind10_k(:) = wind10_k(:) + variance_k(:) * 0.00512_JPRB

        Else
           emissstokes_k(i,:) =  emissstokes_k(i,:) - reflectstokes_k(i,:)

        End If

        If ( wanted_fastem_ver == 3) then
          einterpolated_k(:,:) = 0.0_JPRB
          phi_k(:)=0.0_JPRB
          Do istokes=1,4
             azimuthal_emiss_k=emissstokes_k(i,istokes)
             Do M=1,3
                If(istokes.le.2) Then
                   einterpolated_k(istokes,M)= azimuthal_emiss_k*cos(m*phi)*(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                   phi_k(istokes)=phi_k(istokes) - azimuthal_emiss_k*einterpolated(istokes,M)*m*sin(m*phi)*&
                    (1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                Else
                   einterpolated_k(istokes,M)= azimuthal_emiss_k*sin(m*phi)*(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                   phi_k(istokes)=phi_k(istokes) + azimuthal_emiss_k*einterpolated(istokes,M)*m*cos(m*phi)*&
                    (1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                End If
             Enddo
          End Do

          efixed_k(:,:,:) = 0.0_JPRB
          Do M=1,3
             If (freq_ghz.le.freqfixed(1)) Then
                efixed_k(1,:,M)=einterpolated_k(:,M)
             Else If(freq_ghz.ge.freqfixed(4)) then
                efixed_k(4,:,M)=einterpolated_k(:,M)
             Else
                If(freq_ghz.lt.freqfixed(2)) ifreq=2
                If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
                If(freq_ghz.ge.freqfixed(3)) ifreq=4
                dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
                Do istokes=1,4
                   efixed_k(ifreq,istokes,M)=einterpolated_k(istokes,M)*dfreq
                   efixed_k(ifreq-1,istokes,M)=einterpolated_k(istokes,M)*(1.0_JPRB-dfreq)
                End Do
             End If

             Do istokes=1,4
                tbfixed_k(istokes,4,M)= efixed_k(1,istokes,M)  ! 7   GHz
                tbfixed_k(istokes,3,M)= efixed_k(2,istokes,M)  ! 10  GHz
                tbfixed_k(istokes,2,M)= efixed_k(3,istokes,M)  ! 19  GHz
                tbfixed_k(istokes,1,M)= efixed_k(4,istokes,M)  ! 37  GHz
             End Do
          End Do

          u19_k(:)=0.0_JPRB
          Do ich = 0,15
             i_freq = int(ich/4) + 1    ! 37, 19, 10, 7 GHz
             j_stokes = mod(ich,4) + 1
             a3e_k = tbfixed_k(j_stokes,i_freq,3)
             a2e_k = tbfixed_k(j_stokes,i_freq,2)
             a1e_k = tbfixed_k(j_stokes,i_freq,1)

             u19_k(j_stokes) = u19_k(j_stokes) + a3e_k*(c(150+ich*12)+ u19*(2.0*c(151+ich*12)+3.0*u19*c(152+ich*12)))
             u19_k(j_stokes) = u19_k(j_stokes) + a2e_k*(c(146+ich*12)+ u19*(2.0*c(147+ich*12)+3.0*u19*c(148+ich*12)))
             u19_k(j_stokes) = u19_k(j_stokes) + a1e_k*(c(142+ich*12)+ u19*(2.0*c(143+ich*12)+3.0*u19*c(144+ich*12)))
          End Do

          wind10_k(:) = wind10_k(:) + u19_k(:)
          wind10_direction_k(:) = -1.0_JPRB * phi_k(:)
        End If

        foam_cor_k(:)         = emissstokes_k(i,:) * (1. - emiss_save(:))
        emissstokes_k(i,:)    = emissstokes_k(i,:) * (1. - foam_cor(:))

        !Apply foam correction
         wind10_k(:) = wind10_k(:) + foam_cor_k(:) *&
              & c(22) * c(23) * ( wind10 ** (c(23)-1.0_JPRB) )

        !1.3.3) Large scale geometric correction
        !------

        small_rough_cor_k(:) = 0.0_JPRB
        large_rough_cor_k(:) = 0.0_JPRB
        windsec_k(:)         = 0.0_JPRB
        wind10_sq_k(:)       = 0.0_JPRB

        fresnel_v_k       = -emissstokes_k(i,1) * small_rough_cor
        small_rough_cor_k(1) = -emissstokes_k(i,1) * fresnel(1)
        large_rough_cor_k(1) =  emissstokes_k(i,1)

        fresnel_h_k       = -emissstokes_k(i,2) * small_rough_cor
        small_rough_cor_k(2) = -emissstokes_k(i,2) * fresnel(2)
        large_rough_cor_k(2) =  emissstokes_k(i,2)

        windsec_k(1)         =  large_rough_cor_k(1) * zc(6) / 100._JPRB
        wind10_sq_k(1)        =  large_rough_cor_k(1) * zc(5) / 100._JPRB
        wind10_k(1)          =  wind10_k(1)    + large_rough_cor_k(1) * zc(4) / 100._JPRB

        windsec_k(2)         =  large_rough_cor_k(2) * zc(12) / 100._JPRB
        wind10_sq_k(2)        =  large_rough_cor_k(2) * zc(11) / 100._JPRB
        wind10_k(2)          =  wind10_k(2) + large_rough_cor_k(2) * zc(10) / 100._JPRB

        !1.3.2) Small scale correction to reflection coefficients
        !------

        If (freq_ghz >= 15.0) Then
           wind10_k(:) = wind10_k(:) + small_rough_cor_k(:) *&
                & small_rough_cor * c(21) * geom % coszen_sq / (freq_ghz_sq)
        End If

        !1.3.1) Fresnel reflection coefficients
        !------
        fresnel_h_real_k = fresnel_h_k * 2 * fresnel_h_real
        fresnel_h_imag_k = fresnel_h_k * 2 * fresnel_h_imag

        rhth_k = CMPLX(fresnel_h_real_k, -fresnel_h_imag_k,jprb)

        fresnel_v_real_k = fresnel_v_k * 2 * fresnel_v_real
        fresnel_v_imag_k = fresnel_v_k * 2 * fresnel_v_imag

        rvth_k = CMPLX(fresnel_v_real_k, -fresnel_v_imag_k,jprb)

        perm1_k(:) = 0.0_JPRB
        perm2_k(:) = 0.0_JPRB

        perm1_k(1) = - rvth_k * 2 * perm2 / (perm2+perm1)**2
        perm2_k(1) =   rvth_k * 2 * perm1 / (perm2+perm1)**2

        perm1_k(2) = - rhth_k * 2 * geom%coszen / (geom%coszen+perm1)**2

        permittivity_k(:) = perm2_k(:) * geom%coszen

        permittivity_k(:) = permittivity_k(:) + perm1_k(:) * 0.5_JPRB / perm1

        !-----------------------------------------------------
        !1.2 calculate permittivity using double-debye formula
        !-----------------------------------------------------

        perm_Real_k(:) =  Real(  permittivity_k(:) )
        perm_imag_k(:) = -Aimag( permittivity_k(:) )

        perm_imag1_k(:) = perm_imag_k(:)
        perm_imag2_k(:) = perm_imag_k(:)
        perm_imag3_k(:) = perm_imag_k(:)

        einf_k(:)       = perm_real_k(:)
        perm_real1_k(:) = perm_real_k(:)
        perm_real2_k(:) = perm_real_k(:)
        sigma_k(:) = perm_imag3_k(:) / (2.0_JPRB * c(20) * perm_free * freq_ghz)
        tcelsius_k(:) = 0.09437_JPRB * sigma_k(:)

        del2_k(:) =  perm_imag2_k(:) * fen * den2 * f2  / (den2 * den2)
        den2_k(:) = -perm_imag2_k(:) * fen * del2 * f2  / (den2 * den2)
        f2_k(:)   =  perm_imag2_k(:) * fen * den2 * del2/ (den2 * den2)

        del1_k(:) =  perm_imag1_k(:) * fen * den1 * f1  / (den1 * den1)
        den1_k(:) = -perm_imag1_k(:) * fen * del1 * f1  / (den1 * den1)
        f1_k(:)   =  perm_imag1_k(:) * fen * den1 * del1/ (den1 * den1)

        del2_k(:) = del2_k(:) + perm_real2_k * den2 / (den2 * den2)
        den2_k(:) = den2_k(:) - perm_real2_k * del2 / (den2 * den2)

        del1_k(:) = del1_k(:) + perm_real1_k * den1 / (den1 * den1)
        den1_k(:) = den1_k(:) - perm_real1_k * del1 / (den1 * den1)

        f2_k(:) = f2_k(:) + den2_k(:) * 2 * fen_sq * f2
        f1_k(:) = f1_k(:) + den1_k(:) * 2 * fen_sq * f1

        !Static permittivity estatic = del1+del2+einf
        tcelsius_k(:) = tcelsius_k(:) + c(19) * einf_k(:)

        tcelsius_k(:)    = tcelsius_k(:) + del2_k(:) * c(13)
        tcelsius_sq_k(:) = del2_k(:) * c(14)
        tcelsius_cu_k(:) = del2_k(:) * c(15)

        tcelsius_k(:)    = tcelsius_k(:)    + del1_k(:) * c(9)
        tcelsius_sq_k(:) = tcelsius_sq_k(:) + del1_k(:) * c(10)
        tcelsius_cu_k(:) = tcelsius_cu_k(:) + del1_k(:) * c(11)

        !Define two relaxation frequencies, f1 and f2
        tcelsius_k(:)    = tcelsius_k(:)    + f2_k(:) * c(5)
        tcelsius_sq_k(:) = tcelsius_sq_k(:) + f2_k(:) * c(6)
        tcelsius_cu_k(:) = tcelsius_cu_k(:) + f2_k(:) * c(7)

        tcelsius_k(:)    = tcelsius_k(:)    + f1_k(:) * c(2)
        tcelsius_sq_k(:) = tcelsius_sq_k(:) + f1_k(:) * c(3)

        !Set values for temperature polynomials (convert from kelvin to celsius)
        tcelsius_k(:)    = tcelsius_k(:) + tcelsius_cu_k(:) * 3 * tcelsius_sq



        tcelsius_k(:)    = tcelsius_k(:) + tcelsius_sq_k(:) * 2 * tcelsius

        wind10_k(:) = wind10_k(:) + windsec_k(:) * geom%seczen
        windangle_k(:) = wind10_direction_k(:)*quadcof(iquadrant,2)
        windratio_k(:) = 0.0_JPRB
        if (abs(prof % s2m % v) >= 0.0001_JPRB) windratio_k(:) = windangle_k(:)/&
                                    & (1.0_JPRB+windratio*windratio)

        If (prof % s2m % v >= 0.0001_JPRB) then
           If (npol >= 2) Then
              Do Istokes = 1, npol
                       prof_k      => profiles_k( (ipol+istokes-1) )
                 prof_k % s2m % u = prof_k % s2m % u + windratio_k(istokes)*prof % s2m % v /&
                                  & (prof % s2m % v *prof % s2m % v)
                 prof_k % s2m % v = prof_k % s2m % v - windratio_k(istokes)*prof % s2m % u /&
                                  & (prof % s2m % v *prof % s2m % v)
              End Do
           End If
           If (npol == 1) Then
              prof_k      => profiles_k( (ipol) )
              prof_k % s2m % u = prof_k % s2m % u + windratio_k(pol_id-3)*prof % s2m % v /&
                & (prof % s2m % v *prof % s2m % v)
              prof_k % s2m % v = prof_k % s2m % v - windratio_k(pol_id-3)*prof % s2m % u /&
                & (prof % s2m % v *prof % s2m % v)
           End If
        Else
          If (abs(prof % s2m % u) > 0.0001_JPRB) then
            If (npol >= 2) Then
                 Do Istokes = 1, npol
                     prof_k      => profiles_k( (ipol+istokes-1) )
                     prof_k % s2m % u=prof_k % s2m % u + 999999.0*windratio_k(istokes)
                 End Do
              End If
              If (npol == 1) Then
                 prof_k      => profiles_k( (ipol) )
                 prof_k % s2m % u=prof_k % s2m % u + 999999.0*windratio_k(pol_id-3)
            Endif
          Endif
        Endif

        wind10_k(:) = wind10_k(:) + wind10_sq_k(:) * 2 * wind10

        If( wind10 > 0. ) Then
           wind10_sq_k(:)  = 0.5_JPRB*wind10_k(:)/wind10
        Else
           wind10_sq_k = 0.0_JPRB

        Endif

        If (npol >= 2) Then
           Do Istokes = 1, npol
               prof_k      => profiles_k( (ipol+istokes-1) )
               prof_k % s2m % u = prof_k % s2m % u + 2 * wind10_sq_k(istokes) * prof % s2m % u
               prof_k % s2m % v = prof_k % s2m % v + 2 * wind10_sq_k(istokes) * prof % s2m % v
               prof_k % skin % t = prof_k % skin % t + tcelsius_k(istokes)
           End Do
        End If
        If (npol == 1) Then
            prof_k  => profiles_k( (ipol) )
            prof_k % s2m % u = prof_k % s2m % u + 2 * wind10_sq_k(pol_id-3) * prof % s2m % u
            prof_k % s2m % v = prof_k % s2m % v + 2 * wind10_sq_k(pol_id-3) * prof % s2m % v
            prof_k % skin % t = prof_k % skin % t + tcelsius_k(pol_id-3)
        End If
        prof_k % skin % fastem(:) = 0.0_JPRB
	

     Else
        !--------------------
        !2. Land/ice surfaces
        !--------------------

        !Coherent surface scattering model coefficients (input with the profile)
        perm_static   = prof % skin % fastem(1)
        perm_infinite = prof % skin % fastem(2)
        freqr        = prof % skin % fastem(3)
        small_rough   = prof % skin % fastem(4)
        large_rough   = prof % skin % fastem(5)
        chan        = channels(i)
        freq_ghz      = coef % frequency_ghz(chan)

        !Simple Debye + Fresnel model gives reflectivities
        fen    = freq_ghz / freqr
        fen_sq    = fen * fen
        den1    = 1.0_JPRB + fen_sq
        perm_Real = (perm_static+perm_infinite*fen_sq) / den1
        perm_imag = fen*(perm_static-perm_infinite)    / den1
        permittivity = Cmplx(perm_Real,perm_imag,jprb)
        perm1    = sqrt(permittivity - geom%sinzen_sq)
        perm2    = permittivity * geom%coszen
        rhth    = (geom%coszen - perm1) / (geom%coszen + perm1)
        rvth    = (perm2 - perm1)/(perm2 + perm1)
        !    fresnel_v_real = dble(rvth)
        fresnel_v_Real = Real(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel(1)  = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel(2)  = fresnel_h_Real * fresnel_h_Real + &
      &        fresnel_h_imag * fresnel_h_imag

        !Small scale roughness correction
        delta    = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * small_rough
        delta2    = delta * delta
        small_rough_cor = Exp(-delta2*geom%coszen_sq)

        !Large scale roughness correction
        qdepol = 0.35_JPRB - 0.35_JPRB*Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        emissfactor_v = 1.0_JPRB - fresnel(1) * small_rough_cor
        emissfactor_h = 1.0_JPRB - fresnel(2) * small_rough_cor
        emissfactor   = emissfactor_h - emissfactor_v
        emissstokes(i,1)    = emissfactor_v + qdepol * emissfactor
        emissstokes(i,2)    = emissfactor_h - qdepol * emissfactor


        !.......end of forward part....................................
        !
        ! * Now run K code of fastem
        !

        emissstokes_k(i,2) = emissstokes_k(i,2) - reflectstokes_k(i,2)
        emissstokes_k(i,1) = emissstokes_k(i,1) - reflectstokes_k(i,1)

        emissfactor_h_k =  emissstokes_k(i,2)
        qdepol_k(2)  = -emissstokes_k(i,2) * emissfactor
        emissfactor_k  = -emissstokes_k(i,2) * qdepol

        emissfactor_v_k = emissstokes_k(i,1)
        qdepol_k(1)  = emissstokes_k(i,1) * emissfactor
        emissfactor_k  = emissfactor_k +emissstokes_k(i,1) * qdepol
        qdepol_k(3)     = 0.0_JPRB
        qdepol_k(4)     = 0.0_JPRB

        emissfactor_v_k = emissfactor_v_k - emissfactor_k
        emissfactor_h_k = emissfactor_h_k + emissfactor_k

        fresnel_h_k    = -emissfactor_h_k * small_rough_cor
        small_rough_cor_k(2) = -emissfactor_h_k * fresnel(2)

        fresnel_v_k    = -emissfactor_v_k * small_rough_cor
        small_rough_cor_k(1) = -emissfactor_v_k * fresnel(1)
        small_rough_cor_k(4) = 0.0_JPRB
        small_rough_cor_k(3) = 0.0_JPRB

        !Large scale roughness correction
        large_rough_k(:) = qdepol_k(:) * 0.35_JPRB * 0.60_JPRB*freq_ghz*2*large_rough *&
           & Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        !Small scale roughness correction
        delta2_k(:) = -small_rough_cor_k(:) * geom%coszen_sq * small_rough_cor
        delta_k(:)  = delta2_k(:)* 2 * delta
        small_rough_k(:) = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * delta_k(:)

        !1.3.1) Fresnel reflection coefficients
        !Simple Debye + Fresnel model gives reflectivities
        !------
        fresnel_h_real_k = fresnel_h_k * 2 * fresnel_h_real
        fresnel_h_imag_k = fresnel_h_k * 2 * fresnel_h_imag

        rhth_k = CMPLX(fresnel_h_real_k, -fresnel_h_imag_k, jprb)

        fresnel_v_real_k = fresnel_v_k * 2 * fresnel_v_real
        fresnel_v_imag_k = fresnel_v_k * 2 * fresnel_v_imag

        rvth_k = CMPLX(fresnel_v_real_k, -fresnel_v_imag_k, jprb)

        perm1_k(1) = - rvth_k * 2 * perm2 / (perm2+perm1)**2
        perm2_k(1) =   rvth_k * 2 * perm1 / (perm2+perm1)**2

        perm1_k(2) = - rhth_k * 2 * geom%coszen / (geom%coszen+perm1)**2
        perm2_k(2) = 0.0_JPRB
        perm1_k(3) = 0.0_JPRB
        perm1_k(4) = 0.0_JPRB
        perm2_k(3) = 0.0_JPRB
        perm2_k(4) = 0.0_JPRB

        permittivity_k(:) = perm2_k(:) * geom%coszen

        permittivity_k(:) = permittivity_k(:) + perm1_k(:) * 0.5_JPRB / perm1

        perm_Real_k(:) =  Real(  permittivity_k(:) )
        perm_imag_k(:) = -Aimag( permittivity_k(:) )

        fen_k(:)    =  perm_imag_k(:) *&
              & (perm_static - perm_infinite)/ den1
        perm_static_k(:)  =  perm_imag_k(:) *&
              & fen / den1
        perm_infinite_k(:)   = -perm_imag_k(:) *&
               & fen / den1
        den1_k(:)      = -perm_imag_k(:) *&
           & fen * (perm_static - perm_infinite)/ (den1*den1)

        perm_static_k(:)  = perm_static_k(:)   + perm_real_k(:) / den1
        perm_infinite_k(:)   = perm_infinite_k(:) + perm_real_k(:) *&
              & fen_sq / den1
        fen_sq_k(:)  =         perm_real_k(:) *&
              & perm_infinite / den1
        den1_k(:)    = den1_k(:)    - perm_real_k(:) *&
           &  (perm_static + perm_infinite * fen_sq) / (den1*den1)


        fen_sq_k(:)   = fen_sq_k(:) + den1_k(:)

        fen_k(:)   = fen_k(:) + fen_sq_k(:) * 2* fen

        freqr_k(:)    =  -fen_k(:) * freq_ghz / freqr**2

        If (npol >= 2) Then
            Do Istokes = 1, npol
                prof_k      => profiles_k( (ipol+istokes-1) )
                prof_k % skin % fastem(1) = prof_k % skin % fastem(1) +&
                     & perm_static_k(Istokes)
                prof_k % skin % fastem(2) = prof_k % skin % fastem(2) +&
                     & perm_infinite_k(Istokes)
                prof_k % skin % fastem(3) = prof_k % skin % fastem(3) +&
                     & freqr_k(Istokes)
                prof_k % skin % fastem(4) = prof_k % skin % fastem(4) +&
                     & small_rough_k(Istokes)
                prof_k % skin % fastem(5) = prof_k % skin % fastem(5) +&
                     & large_rough_k (Istokes)
            End Do

        End If
        If (npol == 1) Then
            prof_k  => profiles_k( (ipol) )
            prof_k % skin % fastem(1) = prof_k % skin % fastem(1) +&
           & perm_static_k(1) + perm_static_k(2)
            prof_k % skin % fastem(2) = prof_k % skin % fastem(2) +&
               & perm_infinite_k(1) + perm_infinite_k(2)
            prof_k % skin % fastem(3) = prof_k % skin % fastem(3) +&
               & freqr_k(1) +  freqr_k(2)
            prof_k % skin % fastem(4) = prof_k % skin % fastem(4) +&
               & small_rough_k(1) +  small_rough_k(2)
            prof_k % skin % fastem(5) = prof_k % skin % fastem(5) +&
               & large_rough_k (1) + large_rough_k (2)
        End If

     End If

  End Do

  emissivity_k(:) = emissivity_k(:)  - reflectivity_k(:)

End Subroutine rttov_calcemis_mw_k
