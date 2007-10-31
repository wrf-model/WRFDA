!
Subroutine rttov_calcemis_mw_ad ( &
     & profiles,         &! in
     & profiles_ad,      &! inout
     & geometry,         &! in
     & coef,             &! in
     & nfrequencies,     &! in
     & nchannels,        &! in
     & nprofiles,        &! in
     & channels,         &! in
     & polarisations,    &! in
     & lprofiles,        &! in
     & transmission,     &! in
     & transmission_ad,  &! inout
     & calcemis,         &! in
     & emissivity_ad,    &! inout
     & reflectivity_ad  ) ! inout
  ! Description:
  ! Adjoint of rttov_calcemis_mw
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
  !  1.2       26/09/2003  Polarimetric code and Fastem-3 (S. English)
  !  1.3       18/08/2004  Fixed bug in adjoint (S English)
  !  1.4       29/03/2005  Add end of header comment (J. Cameron)
  !  1.5       14/10/2005  Reintroduce -r 122:123 changes, see -r 133:134).
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
  Integer(Kind=jpim),      Intent(in)    :: nprofiles
  Integer(Kind=jpim),      Intent(in)    :: nfrequencies
  Integer(Kind=jpim),      Intent(in)    :: nchannels
  Type(profile_Type),      Intent(in) ,Target   :: profiles(nprofiles)
  Type(geometry_Type),     Intent(in) ,Target   :: geometry(nprofiles)
  Type(rttov_coef),        Intent(in)    :: coef
  Integer(Kind=jpim),      Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),      Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),      Intent(in)    :: lprofiles(nfrequencies)
  Type(transmission_Type), Intent(in)    :: transmission
  Logical,                 Intent(in)    :: calcemis(nchannels)

  Type(profile_Type),      Intent(inout) ,Target   :: profiles_ad(nprofiles)
  Type(transmission_Type), Intent(inout) :: transmission_ad
  Real(Kind=jprb),         Intent(inout) :: emissivity_ad(nchannels)
  Real(Kind=jprb),         Intent(inout) :: reflectivity_ad(nchannels)

  !local constants:
  Real(Kind=jprb), Parameter :: quadcof(4,2) = Reshape( &
       & (/ 0.0_JPRB, 1.0_JPRB, 1.0_JPRB, 2.0_JPRB, &
       & 1.0_JPRB, -1.0_JPRB, 1.0_JPRB, -1.0_JPRB /), (/4,2/) )
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
  Real(Kind=jprb) :: emissstokes_ad(nfrequencies,4)
  Real(Kind=jprb) :: reflectstokes_ad(nfrequencies,4)
  Real(Kind=jprb) :: u19,phi,dfreq
  Real(Kind=jprb) :: tbfixed(4,4,3)! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed(4,4,3) ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated(4,3)! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e,a2e,a3e     ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb) :: zrough_v,zrough_h
  Real(Kind=jprb) :: zreflmod_v,zreflmod_h
  Real(Kind=jprb) :: delta,delta2
  Real(Kind=jprb) :: qdepol,emissfactor
  Real(Kind=jprb) :: emissfactor_v,emissfactor_h
  Real(Kind=jprb) :: zc(12),zx(9)
  Real(Kind=jprb) :: opdpsfc,freqr
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
  Type(profile_Type),  Pointer    :: prof_ad
  Type(geometry_Type), Pointer    :: geom


  Real(Kind=jprb) :: tcelsius_ad
  Real(Kind=jprb) :: tcelsius_sq_ad
  Real(Kind=jprb) :: tcelsius_cu_ad
  Real(Kind=jprb) :: f1_ad, f2_ad
  Real(Kind=jprb) :: del1_ad, del2_ad
  Real(Kind=jprb) :: einf_ad
  Real(Kind=jprb) :: fen_ad, fen_sq_ad
  Real(Kind=jprb) :: den1_ad, den2_ad
  Real(Kind=jprb) :: sigma_ad
  Real(Kind=jprb) :: perm_real1_ad, perm_real2_ad
  Real(Kind=jprb) :: perm_imag1_ad, perm_imag2_ad, perm_imag3_ad
  Real(Kind=jprb) :: perm_Real_ad, perm_imag_ad
  Real(Kind=jprb) :: perm_static_ad, perm_infinite_ad
  Real(Kind=jprb) :: fresnel_v_Real_ad, fresnel_v_imag_ad
  Real(Kind=jprb) :: fresnel_h_Real_ad, fresnel_h_imag_ad
  Real(Kind=jprb) :: fresnel_v_ad, fresnel_h_ad
  Real(Kind=jprb) :: small_rough_cor_ad, foam_cor_ad
  Real(Kind=jprb) :: large_rough_cor_ad(2)
  Real(Kind=jprb) :: small_rough_ad, large_rough_ad
  Real(Kind=jprb) :: variance_ad, varm_ad
  Real(Kind=jprb) :: wind10_ad
  Real(Kind=jprb) :: wind10_sq_ad, windsec_ad
  Real(Kind=jprb) :: wind10_direction_ad, windangle_ad, windratio_ad ! Note wind azimuth is in radians
  Real(Kind=jprb) :: azimuthal_emiss_ad, azimuthal_emiss,u19_ad,phi_ad
  Real(Kind=jprb) :: tbfixed_ad(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed_ad(4,4,3)   ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated_ad(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e_ad,a2e_ad,a3e_ad     ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb) :: opdpsfc_ad, freqr_ad
  Real(Kind=jprb) :: zrough_v_ad, zrough_h_ad
  Real(Kind=jprb) :: zreflmod_v_ad, zreflmod_h_ad
  Real(Kind=jprb) :: delta_ad, delta2_ad
  Real(Kind=jprb) :: qdepol_ad, emissfactor_ad
  Real(Kind=jprb) :: emissfactor_v_ad, emissfactor_h_ad
  Real(Kind=jprb) :: zx_ad(9)
  Complex(Kind=jprb) :: perm1_ad, perm2_ad
  Complex(Kind=jprb) :: rhth_ad, rvth_ad
  Complex(Kind=jprb) :: permittivity_ad
  Integer(Kind=jpim) :: wanted_fastem_ver  ! user fastem version request

  Real(Kind=jprb) :: test_variance

  !- End of header --------------------------------------------------------

  ! If the coefficent file contains FASTEM 2 it contains also FASTEM 1 but
  ! the version choosen is given by coef % fastem_ver value
  wanted_fastem_ver = coef % fastem_ver

  !If a TL value of emissivity is passed to the routine
  !Loop over channels

  phi_ad=0.0_JPRB
  efixed_ad(:,:,:)=0.0_JPRB

  Do i = 1, nfrequencies
     ichannel=polarisations(i,1)
     If ( .Not. calcemis(ichannel) ) Cycle
     chan        = channels(i)
     prof        => profiles( lprofiles(i) )
     prof_ad     => profiles_ad( lprofiles(i) )
     geom        => geometry( lprofiles(i) )

     !-------------------------------
     !0. Point to fastem coefficients
     !-------------------------------

     c => coef % fastem_coef

     pol_id = coef % fastem_polar(chan) + 1
     reflectstokes_ad(i,:) = 0.0_JPRB
     emissstokes_ad(i,:)   = 0.0_JPRB

     If (pol_id <= 3 .or. pol_id >= 6) then
        Do Ich=1, polarisations(i,3)
           reflectstokes_ad(i,ich) = reflectivity_ad(ichannel+ich-1)
           emissstokes_ad(i,ich)   = emissivity_ad(ichannel+ich-1)
        End Do
     End If

     If (pol_id == 4) then
        reflectstokes_ad(i,1) = reflectivity_ad(ichannel)
        emissstokes_ad(i,1)   = emissivity_ad(ichannel)
     End If

     If (pol_id == 5) then
        reflectstokes_ad(i,2) = reflectivity_ad(ichannel)
        emissstokes_ad(i,2)   = emissivity_ad(ichannel)
     End If

     wind10_ad = 0._JPRB
     wind10_direction_ad = 0.0_JPRB

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

        if (abs(prof % s2m % v) >= 0.0001_JPRB) then
           windratio=prof % s2m % u/prof % s2m % v
        else
           windratio=0.0
           if (abs(prof % s2m % u) > 0.0001_JPRB) then
              windratio=999999.0*prof % s2m % u
           endif
        endif

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
        fresnel(1)     = fresnel_v_Real * fresnel_v_Real + &
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
        emiss_save(:) = 1.0 - fresnel(:) * small_rough_cor + large_rough_cor(:)

        !Apply foam correction
        foam_cor(1)  = c(22) * ( wind10 ** c(23) )
        foam_cor(2)  = c(22) * ( wind10 ** c(23) )
        !Currently ignore foam effects on 3rd and 4th elements.
        foam_cor(3)  = 0.0_JPRB
        foam_cor(4)  = 0.0_JPRB

        emissstokes(i,:) = emiss_save(:) - foam_cor(:)*emiss_save(:) + foam_cor(:)
        emissstokes(i,3) = 0.0
        emissstokes(i,4) = 0.0
	
        If ((wanted_fastem_ver == 2 .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) .And. &
          & transmission % tau_surf(ichannel) < 0.9999_JPRB .And.  &
          & transmission % tau_surf(ichannel) > 0.00001_JPRB ) Then

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
           zreflmod_v = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v) / (1.0_JPRB-transmission % tau_surf(ichannel))
           zreflmod_h = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h) / (1.0_JPRB-transmission % tau_surf(ichannel))

        End If

           !.......end of forward part....................................
           !
           ! * Now run adjoint code of fastem
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
             Do istokes=1,4
                efixed(1,istokes,M)= tbfixed(istokes,4,M)  ! 7   GHz
                efixed(2,istokes,M)= tbfixed(istokes,3,M)  ! 10  GHz
                efixed(3,istokes,M)= tbfixed(istokes,2,M)  ! 19  GHz
                efixed(4,istokes,M)= tbfixed(istokes,1,M)  ! 37  GHz
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
	  
	   zreflmod_v_ad = reflectstokes_ad(i,1) * (1.0_JPRB-emissstokes(i,1))
           zreflmod_h_ad = reflectstokes_ad(i,2) * (1.0_JPRB-emissstokes(i,2))
           zreflmod_v_ad = zreflmod_v_ad - 0.5_JPRB * reflectstokes_ad(i,3) * emissstokes(i,3) &
	      & - 0.5_JPRB * reflectstokes_ad(i,4) * emissstokes(i,4)
           zreflmod_h_ad = zreflmod_h_ad - 0.5_JPRB * reflectstokes_ad(i,3) * emissstokes(i,3) &
	      & - 0.5_JPRB * reflectstokes_ad(i,4) * emissstokes(i,4)
	   
	   emissstokes_ad(i,4) = emissstokes_ad(i,4) - 0.5_JPRB * (zreflmod_v + zreflmod_h) * reflectstokes_ad(i,4)
           emissstokes_ad(i,3) = emissstokes_ad(i,3) - 0.5_JPRB * (zreflmod_v + zreflmod_h) * reflectstokes_ad(i,3)
           emissstokes_ad(i,2) = emissstokes_ad(i,2) - reflectstokes_ad(i,2) * zreflmod_h
           emissstokes_ad(i,1) = emissstokes_ad(i,1) - reflectstokes_ad(i,1) * zreflmod_v

           zrough_v_ad = -zreflmod_v_ad * &
             & ( transmission % tau_surf(ichannel)**zrough_v * Log(transmission % tau_surf(ichannel)) ) / &
             & (1.0_JPRB-transmission % tau_surf(ichannel))

           transmission_ad % tau_surf(ichannel) = transmission_ad % tau_surf(ichannel) + zreflmod_v_ad *&
             & (-zrough_v * transmission % tau_surf(ichannel)**(zrough_v-1.0_JPRB) * &
                     & (1.0_JPRB-transmission % tau_surf(ichannel)) +  &
             &    ( 1.0_JPRB-transmission % tau_surf(ichannel)**zrough_v)         ) &
             & / (1.0_JPRB-transmission % tau_surf(ichannel))**2

            zrough_h_ad = - zreflmod_h_ad * &
                 & ( transmission % tau_surf(ichannel)**zrough_h * Log(transmission % tau_surf(ichannel)) ) / &
                 & (1.0_JPRB-transmission % tau_surf(ichannel))

           transmission_ad % tau_surf(ichannel) = transmission_ad % tau_surf(ichannel) + zreflmod_h_ad *&
             & (-zrough_h * transmission %tau_surf(ichannel)**(zrough_h-1.0_JPRB) * &
                           & (1.0_JPRB-transmission % tau_surf(ichannel)) +  &
             &     ( 1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h)          ) &
             & / (1.0_JPRB-transmission % tau_surf(ichannel))**2

            zx_ad(:) = 0._JPRB
           Do jcof = 1,7
              jcofm1 = jcof-1
              !Switched h to v Deblonde SSMIS june 7, 2001

              zx_ad(9) = zx_ad(9) + zrough_v_ad * zx(jcof) * c(119+jcofm1*3)
              zx_ad(8) = zx_ad(8) + zrough_v_ad * zx(jcof) * c(118+jcofm1*3)
              zx_ad(jcof) = zrough_v_ad *&
                    & (          c(117+jcofm1*3)   &
                     & + zx(8) * c(118+jcofm1*3)   &
                     & + zx(9) * c(119+jcofm1*3) )

              zx_ad(9) = zx_ad(9) + zrough_h_ad * zx(jcof) * c(98+jcofm1*3)
              zx_ad(8) = zx_ad(8) + zrough_h_ad * zx(jcof) * c(97+jcofm1*3)
              zx_ad(jcof) = zx_ad(jcof) + zrough_h_ad *&
                    & (             c(96+jcofm1*3)   &
                     & + zx(8)  *   c(97+jcofm1*3)   &
                      & + zx(9)  *   c(98+jcofm1*3) )

           End Do
           zrough_v_ad = 0._JPRB
           zrough_h_ad = 0._JPRB

           !Define nine predictors for the effective angle calculation
           zx_ad(8) = zx_ad(8) + zx_ad(9) * 2 * zx(8)

           opdpsfc_ad = zx_ad(8) / opdpsfc

           zx_ad(2) = zx_ad(2) + zx_ad(7) * 2 * zx(2)

           zx_ad(4) = zx_ad(4) + zx_ad(6) * 2 * zx(4)

           zx_ad(3) = zx_ad(3) + zx_ad(5) * 2 * zx(3)

           zx_ad(2) = zx_ad(2) + zx_ad(3) * zx(4)

           zx_ad(4) = 0._JPRB

           variance_ad = zx_ad(2)

           zx_ad(1) = 0._JPRB

           !Compute surface to space optical depth
           transmission_ad % tau_surf(ichannel) = transmission_ad % tau_surf(ichannel) - opdpsfc_ad /&
                 & ( transmission % tau_surf(ichannel) * geom%seczen )

           If ( test_variance < varm ) Then
              varm_ad = variance_ad * ( c(139) * freq_ghz + c(140) )
           Else
              varm_ad = variance_ad
           Endif

           variance_ad = varm_ad * c(138)
           wind10_ad = wind10_ad + variance_ad * 0.00512_JPRB
        Else
           emissstokes_ad(i,:) =  emissstokes_ad(i,:) - reflectstokes_ad(i,:)
        End If

       If ( wanted_fastem_ver == 3) then
          azimuthal_emiss_ad = 0.0_JPRB
          phi_ad             = 0.0_JPRB
          Do istokes=1,4
             azimuthal_emiss_ad=emissstokes_ad(i,istokes)
             Do M=1,3
                If(istokes.le.2) Then
                   einterpolated_ad(istokes,M)=azimuthal_emiss_ad*cos(m*phi)*(1.0_JPRB-geom%coszen)/&
                  &(1.0_JPRB - 0.6018_JPRB)
                   phi_ad= phi_ad - azimuthal_emiss_ad*einterpolated(istokes,M)*m*sin(m*phi)*(1.0_JPRB-geom%coszen)/&
                  &(1.0_JPRB - 0.6018_JPRB)
                Else
                   einterpolated_ad(istokes,M)=azimuthal_emiss_ad*sin(m*phi)*(1.0_JPRB-geom%coszen)/(1.0_JPRB - 0.6018_JPRB)
                   phi_ad= phi_ad + azimuthal_emiss_ad*einterpolated(istokes,M)*m*cos(m*phi)*(1.0_JPRB-geom%coszen)/&
                  &(1.0_JPRB - 0.6018_JPRB)
                End If
             Enddo
          End Do

          efixed_ad(:,:,:) = 0.0_JPRB
          Do M=1,3
             If (freq_ghz.le.freqfixed(1)) Then
                efixed_ad(1,:,M)=efixed_ad(1,:,M)+einterpolated_ad(:,M)
             Else If(freq_ghz.ge.freqfixed(4)) then
                efixed_ad(4,:,M)=efixed_ad(4,:,M)+einterpolated_ad(:,M)
             Else
                If(freq_ghz.lt.freqfixed(2)) ifreq=2
                If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
                If(freq_ghz.ge.freqfixed(3)) ifreq=4
                dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
                efixed_ad(ifreq,:,M)=efixed_ad(ifreq,:,M)+einterpolated_ad(:,M)*dfreq
                efixed_ad(ifreq-1,:,M)=efixed_ad(ifreq-1,:,M)+einterpolated_ad(:,M)*(1.0-dfreq)
             End If

             Do istokes=1,4
                tbfixed_ad(istokes,4,M)= efixed_ad(1,istokes,M)  ! 7   GHz
                tbfixed_ad(istokes,3,M)= efixed_ad(2,istokes,M)  ! 10  GHz
                tbfixed_ad(istokes,2,M)= efixed_ad(3,istokes,M)  ! 19  GHz
                tbfixed_ad(istokes,1,M)= efixed_ad(4,istokes,M)  ! 37  GHz
             End Do
          End Do

          u19_ad = 0.0_JPRB
          Do ich = 0,15_JPRB
             i_freq = int(ich/4) + 1    ! 37, 19, 10, 7 GHz
             j_stokes = mod(ich,4) + 1
             a3e_ad = tbfixed_ad(j_stokes,i_freq,3)
             a2e_ad = tbfixed_ad(j_stokes,i_freq,2)
             a1e_ad = tbfixed_ad(j_stokes,i_freq,1)
             u19_ad = u19_ad + a3e_ad*(c(150+ich*12)+u19*(2.0*c(151+ich*12)+3.0*u19*c(152+ich*12)))
             u19_ad = u19_ad + a2e_ad*(c(146+ich*12)+u19*(2.0*c(147+ich*12)+3.0*u19*c(148+ich*12)))
             u19_ad = u19_ad + a1e_ad*(c(142+ich*12)+u19*(2.0*c(143+ich*12)+3.0*u19*c(144+ich*12)))
          End Do
          wind10_ad = wind10_ad + u19_ad
          wind10_direction_ad = -1.0_JPRB * phi_ad
        End If

        ! Be careful do TL first because the next 2 lines of the direct model
        ! have variables in input/output of the statement

        foam_cor_ad = 0.0_JPRB
        Do Ich=1,4
           foam_cor_ad   = foam_cor_ad + emissstokes_ad(i,ich) * (1.0_JPRB - emiss_save(ich))
           emissstokes_ad(i,Ich) = emissstokes_ad(i,ich) * (1.0_JPRB - foam_cor(ich))
        End Do

        !Apply foam correction
        wind10_ad = wind10_ad + foam_cor_ad *&
              & c(22) * c(23) * ( wind10 ** (c(23)-1.0_JPRB) )

        !1.3.3) Large scale geometric correction
        !------
        fresnel_v_ad          = -emissstokes_ad(i,1) * small_rough_cor
        small_rough_cor_ad    = -emissstokes_ad(i,1) * fresnel(1)
        large_rough_cor_ad(1) =  emissstokes_ad(i,1)

        fresnel_h_ad          = -emissstokes_ad(i,2) * small_rough_cor

        small_rough_cor_ad    =  small_rough_cor_ad - emissstokes_ad(i,2) * fresnel(2)
        large_rough_cor_ad(2) =  emissstokes_ad(i,2)

        windsec_ad   =             large_rough_cor_ad(2) * zc(12) / 100._JPRB
        wind10_sq_ad =             large_rough_cor_ad(2) * zc(11) / 100._JPRB
        wind10_ad    = wind10_ad + large_rough_cor_ad(2) * zc(10) / 100._JPRB


        windsec_ad   = windsec_ad   + large_rough_cor_ad(1) * zc(6) / 100._JPRB
        wind10_sq_ad = wind10_sq_ad + large_rough_cor_ad(1) * zc(5) / 100._JPRB
        wind10_ad    = wind10_ad    + large_rough_cor_ad(1) * zc(4) / 100._JPRB


        !1.3.2) Small scale correction to reflection coefficients
        !------

        If (freq_ghz >= 15.0) Then
           wind10_ad = wind10_ad + small_rough_cor_ad *&
                & small_rough_cor * c(21) * geom % coszen_sq / (freq_ghz_sq)
        End If

        !1.3.1) Fresnel reflection coefficients
        !------

        fresnel_h_real_ad = fresnel_h_ad * 2 * fresnel_h_real
        fresnel_h_imag_ad = fresnel_h_ad * 2 * fresnel_h_imag

        rhth_ad = CMPLX(fresnel_h_real_ad, -fresnel_h_imag_ad,jprb)

        fresnel_v_real_ad = fresnel_v_ad * 2 * fresnel_v_real
        fresnel_v_imag_ad = fresnel_v_ad * 2 * fresnel_v_imag

        rvth_ad = CMPLX(fresnel_v_real_ad, -fresnel_v_imag_ad,jprb)

        perm1_ad = - rvth_ad * 2 * perm2 / (perm2+perm1)**2
        perm2_ad =   rvth_ad * 2 * perm1 / (perm2+perm1)**2

        perm1_ad = perm1_ad - rhth_ad * 2 * geom%coszen / (geom%coszen+perm1)**2

        permittivity_ad = perm2_ad * geom%coszen

        permittivity_ad = permittivity_ad + perm1_ad * 0.5_JPRB / perm1

        !-----------------------------------------------------
        !1.2 calculate permittivity using double-debye formula
        !-----------------------------------------------------

        perm_Real_ad =  Real(  permittivity_ad )
        perm_imag_ad = -Aimag( permittivity_ad )

        perm_imag1_ad = perm_imag_ad
        perm_imag2_ad = perm_imag_ad
        perm_imag3_ad = perm_imag_ad

        einf_ad       = perm_real_ad
        perm_real1_ad = perm_real_ad
        perm_real2_ad = perm_real_ad

        sigma_ad = perm_imag3_ad / (2.0_JPRB * c(20) * perm_free * freq_ghz)
        tcelsius_ad = 0.09437_JPRB * sigma_ad

        del2_ad =  perm_imag2_ad * fen * den2 * f2  / (den2 * den2)
        den2_ad = -perm_imag2_ad * fen * del2 * f2  / (den2 * den2)
        f2_ad   =  perm_imag2_ad * fen * den2 * del2/ (den2 * den2)

        del1_ad =  perm_imag1_ad * fen * den1 * f1  / (den1 * den1)
        den1_ad = -perm_imag1_ad * fen * del1 * f1  / (den1 * den1)
        f1_ad   =  perm_imag1_ad * fen * den1 * del1/ (den1 * den1)


        del2_ad = del2_ad + perm_real2_ad * den2 / (den2 * den2)
        den2_ad = den2_ad - perm_real2_ad * del2 / (den2 * den2)

        del1_ad = del1_ad + perm_real1_ad * den1 / (den1 * den1)
        den1_ad = den1_ad - perm_real1_ad * del1 / (den1 * den1)


        f2_ad = f2_ad + den2_ad * 2 * fen_sq * f2
        f1_ad = f1_ad + den1_ad * 2 * fen_sq * f1

        !Static permittivity estatic = del1+del2+einf
        tcelsius_ad    = tcelsius_ad + c(19) * einf_ad
        tcelsius_ad    = tcelsius_ad + del2_ad * c(13)
        tcelsius_sq_ad = del2_ad * c(14)
        tcelsius_cu_ad = del2_ad * c(15)

        tcelsius_ad    = tcelsius_ad    + del1_ad * c(9)
        tcelsius_sq_ad = tcelsius_sq_ad + del1_ad * c(10)
        tcelsius_cu_ad = tcelsius_cu_ad + del1_ad * c(11)


        !Define two relaxation frequencies, f1 and f2
        tcelsius_ad    = tcelsius_ad    + f2_ad * c(5)
        tcelsius_sq_ad = tcelsius_sq_ad + f2_ad * c(6)
        tcelsius_cu_ad = tcelsius_cu_ad + f2_ad * c(7)

        tcelsius_ad    = tcelsius_ad    + f1_ad * c(2)
        tcelsius_sq_ad = tcelsius_sq_ad + f1_ad * c(3)


        !Set values for temperature polynomials (convert from kelvin to celsius)
        tcelsius_ad    = tcelsius_ad + tcelsius_cu_ad * 3 * tcelsius_sq

        tcelsius_ad    = tcelsius_ad + tcelsius_sq_ad * 2 * tcelsius
        prof_ad % skin % t = prof_ad % skin % t + tcelsius_ad

        wind10_ad = wind10_ad + windsec_ad * geom%seczen
        windangle_ad = wind10_direction_ad *quadcof(iquadrant,2)
        windratio_ad = 0.0_JPRB
        if (abs(prof % s2m % v) >= 0.0001_JPRB) windratio_ad = windangle_ad/(1.0_JPRB+windratio*windratio)

!        prof_ad % s2m % u=0.0_JPRB
!        prof_ad % s2m % v=0.0_JPRB

        If (abs(prof % s2m % v) >= 0.0001_JPRB) then
           prof_ad % s2m % u = prof_ad % s2m % u + windratio_ad*prof % s2m % v /&
              & (prof % s2m % v *prof % s2m % v)
           prof_ad % s2m % v = prof_ad % s2m % v - windratio_ad*prof % s2m % u /&
              & (prof % s2m % v *prof % s2m % v)
        Else
           If (abs(prof % s2m % u) > 0.0001_JPRB) then
              prof_ad % s2m % u=999999.0*windratio_ad
           Endif
        Endif
        wind10_ad = wind10_ad + wind10_sq_ad * 2 * wind10

        If( wind10 > 0._JPRB ) Then
           wind10_sq_ad  = 0.5_JPRB*wind10_ad/wind10
        Else
           wind10_sq_ad = 0.0_JPRB
        Endif

        prof_ad % s2m % u = prof_ad % s2m % u +&
              & 2 * wind10_sq_ad * prof % s2m % u
        prof_ad % s2m % v = prof_ad % s2m % v +&
              & 2 * wind10_sq_ad * prof % s2m % v
        prof_ad % skin % fastem(:) = 0._JPRB
	
     Else
        !--------------------
        !2. Land/ice surfaces
        !--------------------

        !Coherent surface scattering model coefficients (input with the profile)
        perm_static   = prof % skin % fastem(1)
        perm_infinite = prof % skin % fastem(2)
        freqr         = prof % skin % fastem(3)
        small_rough   = prof % skin % fastem(4)
        large_rough   = prof % skin % fastem(5)
        chan          = channels(i)
        freq_ghz      = coef % frequency_ghz(chan)

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
        fresnel(1)      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel(2)      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag

        !Small scale roughness correction
        delta           = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * small_rough
        delta2          = delta * delta
        small_rough_cor = Exp(-delta2*geom%coszen_sq)

        !Large scale roughness correction
        qdepol = 0.35_JPRB - 0.35_JPRB*Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)
        emissfactor_v = 1.0_JPRB - fresnel(1) * small_rough_cor
        emissfactor_h = 1.0_JPRB - fresnel(2) * small_rough_cor
        emissfactor   = emissfactor_h - emissfactor_v
        emissstokes(i,1)    = emissfactor_v + qdepol * emissfactor
        emissstokes(i,2)    = emissfactor_h - qdepol * emissfactor
        !reflect_v(i)  = 1.0_JPRB - emiss_v(i)
        !reflect_h(i)  = 1.0_JPRB - emiss_h(i)

        !.......end of forward part....................................
        !
        ! * Now run adjoint code of fastem
        !
        emissstokes_ad(i,2) = emissstokes_ad(i,2) - reflectstokes_ad(i,2)
        emissstokes_ad(i,1) = emissstokes_ad(i,1) - reflectstokes_ad(i,1)

        emissfactor_h_ad =  emissstokes_ad(i,2)
        qdepol_ad        = -emissstokes_ad(i,2) * emissfactor
        emissfactor_ad   = -emissstokes_ad(i,2) * qdepol

        emissfactor_v_ad = emissstokes_ad(i,1)
        qdepol_ad        = qdepol_ad      +emissstokes_ad(i,1) * emissfactor
        emissfactor_ad   = emissfactor_ad +emissstokes_ad(i,1) * qdepol

        emissfactor_v_ad = emissfactor_v_ad - emissfactor_ad
        emissfactor_h_ad = emissfactor_h_ad + emissfactor_ad

        fresnel_h_ad       = -emissfactor_h_ad * small_rough_cor
        small_rough_cor_ad = -emissfactor_h_ad * fresnel(2)

        fresnel_v_ad       =                    -emissfactor_v_ad * small_rough_cor
        small_rough_cor_ad = small_rough_cor_ad -emissfactor_v_ad * fresnel(1)

        !Large scale roughness correction
        large_rough_ad = qdepol_ad * 0.35_JPRB * 0.60_JPRB*freq_ghz*2*large_rough *&
              & Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        !Small scale roughness correction
        delta2_ad = -small_rough_cor_ad * geom%coszen_sq * small_rough_cor
        delta_ad  = delta2_ad* 2 * delta
        small_rough_ad = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * delta_ad

        !1.3.1) Fresnel reflection coefficients
        !Simple Debye + Fresnel model gives reflectivities
        !------
        fresnel_h_real_ad = fresnel_h_ad * 2 * fresnel_h_real
        fresnel_h_imag_ad = fresnel_h_ad * 2 * fresnel_h_imag

        rhth_ad = CMPLX(fresnel_h_real_ad, -fresnel_h_imag_ad,jprb)

        fresnel_v_real_ad = fresnel_v_ad * 2 * fresnel_v_real
        fresnel_v_imag_ad = fresnel_v_ad * 2 * fresnel_v_imag

        rvth_ad = CMPLX(fresnel_v_real_ad, -fresnel_v_imag_ad,jprb)

        perm1_ad = - rvth_ad * 2 * perm2 / (perm2+perm1)**2
        perm2_ad =   rvth_ad * 2 * perm1 / (perm2+perm1)**2

        perm1_ad = perm1_ad - rhth_ad * 2 * geom%coszen / (geom%coszen+perm1)**2

        permittivity_ad = perm2_ad * geom%coszen

        permittivity_ad = permittivity_ad + perm1_ad * 0.5_JPRB / perm1

        perm_Real_ad =  Real(  permittivity_ad )
        perm_imag_ad = -Aimag( permittivity_ad )

        fen_ad           =  perm_imag_ad *&
              & (perm_static - perm_infinite)/ den1
        perm_static_ad   =  perm_imag_ad *&
              & fen / den1
        perm_infinite_ad = -perm_imag_ad *&
               & fen / den1
        den1_ad          = -perm_imag_ad *&
              & fen * (perm_static - perm_infinite)/ (den1*den1)

        perm_static_ad   = perm_static_ad   + perm_real_ad / den1
        perm_infinite_ad = perm_infinite_ad + perm_real_ad *&
              & fen_sq / den1
        fen_sq_ad        =                    perm_real_ad *&
              & perm_infinite / den1
        den1_ad          = den1_ad          - perm_real_ad *&
               & (perm_static + perm_infinite * fen_sq) / (den1*den1)


        fen_sq_ad = fen_sq_ad + den1_ad

        fen_ad    = fen_ad + fen_sq_ad * 2* fen

        freqr_ad  =  -fen_ad * freq_ghz / freqr**2

        prof_ad % skin % fastem(1) = prof_ad % skin % fastem(1) +&
              & perm_static_ad

        prof_ad % skin % fastem(2) = prof_ad % skin % fastem(2) +&
              & perm_infinite_ad

        prof_ad % skin % fastem(3) = prof_ad % skin % fastem(3) +&
              & freqr_ad

        prof_ad % skin % fastem(4) = prof_ad % skin % fastem(4) +&
              & small_rough_ad

        prof_ad % skin % fastem(5) = prof_ad % skin % fastem(5) +&
              & large_rough_ad

     End If

  End Do


  emissivity_ad(:) = emissivity_ad(:)  - reflectivity_ad(:)


End Subroutine rttov_calcemis_mw_ad
