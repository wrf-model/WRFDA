!
Subroutine rttov_calcemis_mw ( &
     & profiles,         &! in
     & geometry,         &! in
     & coef,             &! in
     & nfrequencies,     &! in
     & nchannels,        &! in
     & nprofiles,        &! in
     & channels,         &! in
     & polarisations,    &! in
     & lprofiles,        &! in
     & transmission,     &! in
     & calcemis,         &! in
     & emissivity,       &! inout
     & reflectivity,     &! out
     & errorstatus   )    ! inout
  !
  ! Description:
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
  !  1.2       24/01/2003  error return code changed to array size (P Brunel)
  !                        No more test on negative values for  emissivity input
  !  1.3       26/09/2003  Added polarimetric code and Fastem-3 (S English)
  !  1.4       14/10/2005  Bug fix to wind10_direction calculation (J Cameron)
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
       & surftype_sea        ,&
       & errorstatus_fatal

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & transmission_Type  ,&
       & geometry_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Type(profile_Type),  Intent(in) ,Target   :: profiles(nprofiles)
  Type(geometry_Type), Intent(in) ,Target   :: geometry(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coef
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Type(transmission_Type), Intent(in):: transmission
  Logical,             Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),     Intent(inout) :: emissivity(nchannels)
  Real(Kind=jprb),     Intent(out)   :: reflectivity(nchannels)
  Integer(Kind=jpim),  Intent(inout) :: errorstatus(nprofiles)

  !local constants:
  Real(Kind=jprb), Parameter :: quadcof(4,2) = Reshape( &
       & (/ 0.0_JPRB, 1.0_JPRB, 1.0_JPRB, 2.0_JPRB, &
       & 1.0_JPRB, -1.0_JPRB, 1.0_JPRB, -1.0_JPRB  /), (/4,2/) )
  Real(Kind=jprb), Parameter :: freqfixed(4) = Reshape( &
       & (/ 7.0_JPRB, 10.0_JPRB, 19.0_JPRB, 37.0_JPRB /), (/4/) )

  !local variables:
  Character (len=80) :: errMessage
  Character (len=18) :: NameOfRoutine = 'rttov_calcemis_mw '
  Real(Kind=jprb) :: tcelsius
  Real(Kind=jprb) :: tcelsius_sq
  Real(Kind=jprb) :: tcelsius_cu
  Real(Kind=jprb) :: einf                  ! Debye parameter Epsilon infinity
  Real(Kind=jprb) :: fen,fen_sq            ! intermediate Debye variable
  Real(Kind=jprb) :: del1,del2             ! intermediate Debye variable
  Real(Kind=jprb) :: den1,den2             ! intermediate Debye variable
  Real(Kind=jprb) :: f1,f2                 ! intermediate Debye variable
  Real(Kind=jprb) :: perm_free             ! permittivity (space)
  Real(Kind=jprb) :: sigma                 ! saline water conductivity
  Real(Kind=jprb) :: perm_real1,perm_real2 ! permittivity (real part)
  Real(Kind=jprb) :: perm_imag1,perm_imag2,perm_imag3 !    .... imaginary part
  Real(Kind=jprb) :: perm_Real,perm_imag   ! permittivity (real, imaginary part)
  Real(Kind=jprb) :: perm_static           ! static land permittivity
  Real(Kind=jprb) :: perm_infinite         ! infinite frequency land permittivity
  Real(Kind=jprb) :: freq_ghz,freq_ghz_sq  ! frequency in GHz , and squared
  Real(Kind=jprb) :: fresnel_v_Real,fresnel_v_imag
  Real(Kind=jprb) :: fresnel_h_Real,fresnel_h_imag
  Real(Kind=jprb) :: fresnel_v,fresnel_h
  Real(Kind=jprb) :: small_rough_cor,foam_cor
  Real(Kind=jprb) :: large_rough_cor(2)
  Real(Kind=jprb) :: small_rough,large_rough ! small and large scale roughness
  Real(Kind=jprb) :: emissstokes(nfrequencies,4)
  Real(Kind=jprb) :: reflectstokes(nfrequencies,4)
  Real(Kind=jprb) :: variance,varm
  Real(Kind=jprb) :: wind10
  Real(Kind=jprb) :: wind10_sq,windsec, windratio
  Real(Kind=jprb) :: wind10_direction, windangle ! Note wind azimuth is in radians
  Real(Kind=jprb) :: opdpsfc,freqr
  Real(Kind=jprb) :: zrough_v,zrough_h
  Real(Kind=jprb) :: zreflmod_v,zreflmod_h
  Real(Kind=jprb) :: delta,delta2
  Real(Kind=jprb) :: qdepol,emissfactor
  Real(Kind=jprb) :: emissfactor_v,emissfactor_h
  Real(Kind=jprb) :: zc(12)    ! large scale correction
  Real(Kind=jprb) :: zx(9)     ! effective path coefficients
  Real(Kind=jprb) :: azimuthal_emiss,u19,phi,dfreq
  Real(Kind=jprb) :: tbfixed(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  Real(Kind=jprb) :: efixed(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  Real(Kind=jprb) :: einterpolated(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  Real(Kind=jprb) :: a1e,a2e,a3e    ! coefficients used in azimuthal emissivity model
  Real(Kind=jprb), Pointer :: c(:)   ! pointer to FASTEM coefs
  Complex(Kind=jprb) :: perm1,perm2  ! permittivity
  Complex(Kind=jprb) :: rhth,rvth    ! Fresnel reflectivity complex variables
  Complex(Kind=jprb) :: permittivity ! permittivity
  Integer(Kind=jpim) :: i,j,chan,istokes,ifreq,m
  Integer(Kind=jpim) :: iquadrant    ! Determines which quadrant (NE, SE, SW, NW) the wind is blowing to
  Integer(Kind=jpim) :: pol_id       ! polarisation indice
  Integer(Kind=jpim) :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  ! == pol_id +1
  !   1 average of vertical and horizontal
  !   2 nominal vertical at nadir, rotating
  !      with view angle
  !   3 nominal horizontal at nadir, rotating
  !      with view angle
  !   4 vertical
  !   5 horizontal
  !   6 vertical and horizontal
  !   7 full stokes vector
  Integer(Kind=jpim) :: jcof,jcofm1
  Type(profile_Type),  Pointer    :: prof
  Type(geometry_Type), Pointer    :: geom
  Integer(Kind=jpim) :: wanted_fastem_ver  ! user fastem version request
  !- End of header --------------------------------------------------------

  ! If the coefficent file contains FASTEM 2 it contains also FASTEM 1 but
  ! the version choosen is given by coef % fastem_ver value
  wanted_fastem_ver = coef % fastem_ver

  !Loop over channels

  Do i = 1, nfrequencies
     ichannel=polarisations(i,1)
     If ( .Not. calcemis(ichannel) ) Cycle
     chan        = channels(i)
     prof        => profiles( lprofiles(i) )
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
        wind10_sq        =   prof % s2m % u * prof % s2m % u +&
                              & prof % s2m % v * prof % s2m % v
        wind10           = Sqrt( wind10_sq )
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
        windsec          = wind10 * geom%seczen

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
        ! perm_free = 8.854E-3_JPRB not 8.854E-12 as multiplied by 1E9 for GHz
        perm_free    = 8.854E-3_JPRB
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
        fresnel_v_Real = Dble(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel_v      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        fresnel_h_Real = Dble(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel_h      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag


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
             & zc(1)                  + &
             & zc(2) * geom%seczen    + &
             & zc(3) * geom%seczen_sq + &
             & zc(4) * wind10         + &
             & zc(5) * wind10_sq      + &
             & zc(6) * windsec
        large_rough_cor(2) = &
             & zc(7)                   + &
             & zc(8)  * geom%seczen    + &
             & zc(9)  * geom%seczen_sq + &
             & zc(10) * wind10         + &
             & zc(11) * wind10_sq      + &
             & zc(12) * windsec
        large_rough_cor(:) = large_rough_cor(:) * 0.01_JPRB

        ! For Fastem-3 do not compute rough surface effects if theta > 60 degrees
        If (wanted_fastem_ver <= 2.0_JPRB .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) Then
           emissstokes(i,1) = 1.0_JPRB - fresnel_v * small_rough_cor + large_rough_cor(1)
           emissstokes(i,2) = 1.0_JPRB - fresnel_h * small_rough_cor + large_rough_cor(2)
        Else
           emissstokes(i,1) = 1.0_JPRB - fresnel_v
           emissstokes(i,2) = 1.0_JPRB - fresnel_h
        End If

        emissstokes(i,3) = 0.0_JPRB
        emissstokes(i,4) = 0.0_JPRB

        !Apply foam correction
        foam_cor  = c(22) * ( wind10 ** c(23) )
        emissstokes(i,1) = emissstokes(i,1) - foam_cor*emissstokes(i,1) + foam_cor
        emissstokes(i,2) = emissstokes(i,2) - foam_cor*emissstokes(i,2) + foam_cor

        If ( wanted_fastem_ver == 3) then
         ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)
         ! Angle between wind direction and satellite azimuthal view angle

         phi = pi-(wind10_direction-prof % azangle*pi/180.0_JPRB)
         ! Assume 19m wind = 10m wind for now (fix later).
         u19=wind10
         Do ich = 0,15
            a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))
            a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))
            a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))
            i_freq = int(ich/4) + 1   ! 37, 19, 10, 7 GHz
            j_stokes = mod(ich,4) + 1
            tbfixed(j_stokes,i_freq,1) = a1e !* prof % skin % t
            tbfixed(j_stokes,i_freq,2) = a2e !* prof % skin % t
            tbfixed(j_stokes,i_freq,3) = a3e !* prof % skin % t
         End Do

         Do M=1,3
            Do istokes=1,4
         efixed(1,istokes,M)= tbfixed(istokes,4,M) !/prof % skin % t  ! 7  GHz
         efixed(2,istokes,M)= tbfixed(istokes,3,M) !/prof % skin % t  ! 10  GHz
         efixed(3,istokes,M)= tbfixed(istokes,2,M) !/prof % skin % t  ! 19  GHz
         efixed(4,istokes,M)= tbfixed(istokes,1,M) !/prof % skin % t  ! 37  GHz
            End Do

         ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz

            If (freq_ghz.le.freqfixed(1)) Then
         Do istokes=1,4
            einterpolated(istokes,M)=efixed(1,istokes,M)
         End Do
            Else If(freq_ghz.ge.freqfixed(4)) then
         Do istokes=1,4
            einterpolated(istokes,M)=efixed(4,istokes,M)
         End Do
            Else
         If(freq_ghz.lt.freqfixed(2)) ifreq=2
         If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
         If(freq_ghz.ge.freqfixed(3)) ifreq=4
         dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
         Do istokes=1,4
            einterpolated(istokes,M)=efixed(ifreq-1,istokes,M)+dfreq*(efixed(ifreq,istokes,M)-efixed(ifreq-1,istokes,M))
         End Do
            End If
         End Do
         Do istokes = 1,4
            azimuthal_emiss=0.0_JPRB
            Do M=1,3
         If(istokes.le.2) Then
            azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*(1.0_JPRB-geom%coszen)&
                  & /(1.0_JPRB - 0.6018_JPRB)
         Else
            azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*(1.0_JPRB-geom%coszen)&
                   & /(1.0_JPRB - 0.6018_JPRB)
         End If
            End Do
            emissstokes(i,istokes)=emissstokes(i,istokes)+azimuthal_emiss
         End Do
        End If

        ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((wanted_fastem_ver == 2 .or. (wanted_fastem_ver == 3 .And. geom%seczen <= 2.0_JPRB)) .And. &
             & transmission % tau_surf(ichannel) < 0.9999_JPRB .And. &
             & transmission % tau_surf(ichannel) > 0.00001_JPRB ) Then

           !Convert windspeed to slope variance using the Cox and Munk model
           variance = 0.00512_JPRB * wind10 + 0.0030_JPRB
           varm     = variance * c(138)
           variance = varm * ( c(139) * freq_ghz + c(140) )
           If ( variance > varm ) variance = varm
           If ( variance < 0.0_JPRB  ) variance = 0.0_JPRB

           !Compute surface to space optical depth
           opdpsfc = -log(transmission % tau_surf(ichannel)) / geom%seczen

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

           zreflmod_v = (1.0_JPRB-transmission %tau_surf(ichannel)**zrough_v)&
              & / (1.0_JPRB-transmission % tau_surf(ichannel))
           zreflmod_h = (1.0_JPRB-transmission % tau_surf(ichannel)**zrough_h)&
              & / (1.0_JPRB-transmission % tau_surf(ichannel))
           reflectstokes(i,1)  = zreflmod_v * (1.0_JPRB-emissstokes(i,1))
           reflectstokes(i,2)  = zreflmod_h * (1.0_JPRB-emissstokes(i,2))
           reflectstokes(i,3)  = -0.5_JPRB * (zreflmod_v + zreflmod_h) * emissstokes(i,3)
           reflectstokes(i,4)  = -0.5_JPRB * (zreflmod_v + zreflmod_h) * emissstokes(i,4)
        Else
           reflectstokes(i,1) = 1.0_JPRB - emissstokes(i,1)
           reflectstokes(i,2) = 1.0_JPRB - emissstokes(i,2)
           reflectstokes(i,3) = 0.0_JPRB
           reflectstokes(i,4) = 0.0_JPRB
        End If

        !--------------------
        !2. Land/ice surfaces
        !--------------------

     Else

!        If ( Any( c == 0.0_JPRB ) ) Then
!           Write( errMessage, '( "some fastem coefs are 0.0 " )' )
!           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
!           errorstatus(:) = errorstatus_fatal
!           Return
!        End If

        ! Test input FASTEM land coefficients
        ! only coefs 1-3 are checked
        If ( Any( prof % skin % fastem(1:3) == 0.0_JPRB ) ) Then
           Write( errMessage, '( "some profile fastem(1:3) values are 0.0 " )' )
           Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
           errorstatus(:) = errorstatus_fatal
           Return
        End If

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
        fresnel_v_Real = Dble(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel_v      = fresnel_v_Real * fresnel_v_Real + &
                        & fresnel_v_imag * fresnel_v_imag
        fresnel_h_Real = Dble(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel_h      = fresnel_h_Real * fresnel_h_Real + &
                        & fresnel_h_imag * fresnel_h_imag

        !Small scale roughness correction
        delta           = 4.0_JPRB * pi * coef % ff_cwn(chan) * 0.1_JPRB * small_rough
        delta2          = delta * delta
        small_rough_cor = Exp(-delta2*geom%coszen_sq)

        !Large scale roughness correction
        qdepol = 0.35_JPRB - 0.35_JPRB*Exp(-0.60_JPRB*freq_ghz*large_rough*large_rough)

        emissfactor_v = 1.0_JPRB - fresnel_v * small_rough_cor
        emissfactor_h = 1.0_JPRB - fresnel_h * small_rough_cor
        emissfactor   = emissfactor_h - emissfactor_v
        emissstokes(i,1) = emissfactor_v + qdepol * emissfactor
        emissstokes(i,2) = emissfactor_h - qdepol * emissfactor
        emissstokes(i,3) = 0.0_JPRB
        emissstokes(i,4) = 0.0_JPRB

        reflectstokes(i,:)  = 1.0_JPRB - emissstokes(i,:)
        ! End of if sea else land if else endif loop
     End If
     ! Now return only required polarisations - either the calculated vector (V, H, or full Stokes)
     If (pol_id <= 3 .or. pol_id >= 6) then
        Do ich=1,polarisations(i,3)
           emissivity(ichannel+ich-1)=emissstokes(i,ich)
           reflectivity(ichannel+ich-1)=reflectstokes(i,ich)
        End Do
     End If
     ! Or V-pol only
     If (pol_id == 4) then
        emissivity(ichannel)=emissstokes(i,1)
        reflectivity(ichannel)=reflectstokes(i,1)
     End If
     ! Or H-pol only
     If (pol_id == 5) then
        emissivity(ichannel)=emissstokes(i,2)
        reflectivity(ichannel)=reflectstokes(i,2)
     End If
    ! End loop over channels
  End Do
End Subroutine rttov_calcemis_mw
