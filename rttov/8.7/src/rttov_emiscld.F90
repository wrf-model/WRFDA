!
Subroutine rttov_emiscld(  &
      & errorstatus,       &! out
      & nfrequencies,      &! in
      & nchannels,         &! in
      & nprofiles,         &! in
      & nlevels ,          &! in
      & channels,          &! in
      & polarisations,     &! in
      & lprofiles,         &! in
      & profiles,          &! in  (surftype and zenangle)
      & coef,              &! in  (frequencies mw/ir/hi)
      & cld_profiles,      &! in
      & cld_radiance)       ! inout  (only cldemis calculated)
  ! Description:
  ! To compute cloud emissivity in the micro-wave (0-200 GHz)
  !                         and in the infrared   (50-2860 cm-1)
  !                                               spectral ranges
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
  ! See references in the comments
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  ! 1         03/2000     Original code (F. Chevallier and P. Bauer)
  ! 2         03/2001     Fortran 90    (F. Chevallier)
  ! 2.1       01/12/2002  New F90 code with structures (P Brunel A Smith)
  ! 3         18/9/2003   New absorption properties for water and ice clouds added (P. Francis)
  ! 4         24/2/04     Polarimetry options added
  ! 5         06/10/04    Added errorstatus to arguments.
  !                       Changed stop statements to returns. (J Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :    &
       & pi                   ,&
       & gravity              ,&
       & surftype_land        ,&
       & nvalhusta            ,&
       & zhustaom             , &
       & zhustaa1             , &
       & zhustaa2             , &
       & zhustaa3             , &
       & zhustab1             , &
       & zhustab2             , &
       & zhustab3             , &
       & zhustac1             , &
       & zhustac2             , &
       & zhustac3             , &
       & zhustad1             , &
       & zhustad2             , &
       & zhustad3             , &
       & zhustae1             , &
       & zhustae2             , &
       & zhustae3             , &
       & zhustaf1             , &
       & zhustaf2             , &
       & zhustaf3             , &
       & low_re               , &
       & upp_re               , &
       & nvalice              , &
       & ziceom               , &
       & ziceclmna            , ziceclmnb            , &
       & ziceclmnc            , ziceclmnd            , &
       & ziceaggra            , ziceaggrb            , &
       & ziceaggrc            , ziceaggrd            , &
       & sensor_id_ir         ,&
       & sensor_id_mw         ,&
       & sensor_id_hi         ,&
       & errorstatus_success  ,&
       & errorstatus_fatal

  ! Imported Type Definitions:
  Use rttov_types, Only :    &
       & rttov_coef           ,&
       & profile_Type         ,&
       & profile_cloud_Type   ,&
       & radiance_cloud_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),        Intent(in)    :: nfrequencies       ! Number of chans*profs
  Integer(Kind=jpim),        Intent(in)    :: nchannels          ! Number of output radiances
                                                       !  (= channels used * profiles)
  Integer(Kind=jpim),        Intent(in)    :: nprofiles           ! Number of profiles
  Integer(Kind=jpim),        Intent(in)    :: nlevels              ! Number of levels
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)  ! Error return code
  Integer(Kind=jpim),        Intent(in)    :: polarisations(nchannels,3)  ! Channel indices
  Integer(Kind=jpim),        Intent(in)    :: channels(nfrequencies) ! Channel indices
  Integer(Kind=jpim),        Intent(in)    :: lprofiles(nfrequencies)! Profiles indices
  Type(profile_Type),        Intent(in)    :: profiles(nprofiles) ! Profiles on RTTOV levels
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles)! Cloud profiles on NWP levels
  Type(rttov_coef),          Intent(in)    :: coef          ! Coefficients
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance  ! radiances (mw/cm-1/ster/sq.m)


  ! Local parameters:
  !
  Real(Kind=jprb), Parameter               :: rtt   = 273.15_JPRB
  Real(Kind=jprb), Parameter               :: rgp   = 8.314510_JPRB, rm = 0.0289644_JPRB
  Real(Kind=jprb), Parameter               :: repsc = 1.e-12_JPRB
  Real(Kind=jprb), Parameter               :: hundred = 100.0_JPRB
  Real(Kind=jprb), Parameter               :: rtou_upp=-20._JPRB
  Real(Kind=jprb), Parameter               :: rtou_low=-60._JPRB


  !local variables:
  Integer(Kind=jpim)                       :: jc, jk, jl, jo, ido, idp, idq, idh
  Integer(Kind=jpim)                       :: freq
  Real(Kind=jprb)                          :: zomega, zfreq
  Real(Kind=jprb)                          :: zlwgkg, ziwgkg, ztempc, zdp, p1, p2
  Real(Kind=jprb)                          :: zmsaiu, zdom, zmin, zmultl, zmsalu
  Real(Kind=jprb)                          :: zdz, zodw, zodi, zhelp, zbeta
  Real(Kind=jprb)                          :: fac1, fac2, fac3, fac4, fac5, fac6
  Real(Kind=jprb)                          :: eps0, eps1, eps2, fp, fs, theta
  Real(Kind=jprb)                          :: a, b, tk
  Real(Kind=jprb)                          :: zeps_r, zeps_i
  Real(Kind=jprb)                          :: co_ssa_1, kext_hu_1, kabs_hu_1
  Real(Kind=jprb)                          :: co_ssa_2, kext_hu_2, kabs_hu_2
  Real(Kind=jprb)                          :: kabs_ice_1, kabs_ice_2
  Real(Kind=jprb)                          :: zradipou_upp, zradipou_low
  Real(Kind=jprb)                          :: bwyser, nft
  Real(Kind=jprb)                          :: amcfarq, bmcfarq, cmcfarq, zmcfarq

  Character (len=80) :: errMessage
  Character (len=13) :: NameOfRoutine = 'rttov_emiscld'
  !

  ! Local arrays:
  !
  Integer(Kind=jpim), Dimension(nchannels) :: indh
  Integer(Kind=jpim), Dimension(nchannels) :: indi
  Real(Kind=jprb), Dimension(nprofiles)    :: zradlp, zradip
  Real(Kind=jprb), Dimension(nprofiles)    :: zflwp, zfiwp, zlwc, ziwc
  Real(Kind=jprb), Dimension(nprofiles)    :: zview
  Real(Kind=jprb)                          :: zdzst(nprofiles, nlevels)

  !- End of header --------------------------------------------------------
  !All input NWP profiles have the same number of levels

  errorstatus(:) = errorstatus_success


  ! All channels are IR or MW according to the coefficient
  ! structure information

  !
  ! Calculate upper and lower limits for Ou-Liou effective size
  !
  zradipou_upp=326.3_JPRB + rtou_upp*(12.42_JPRB + rtou_upp*(0.197_JPRB + rtou_upp*0.0012_JPRB))
  zradipou_low=326.3_JPRB + rtou_low*(12.42_JPRB + rtou_low*(0.197_JPRB + rtou_low*0.0012_JPRB))
  !
  ! and convert these to the "generalized" effective size used here (using McFarquhar et al 2003 equation),
  ! not forgetting the factor of 2 to convert from McFarquhar's radius to a diameter
  !
  zradipou_upp=-1.56_JPRB + zradipou_upp*(0.388_JPRB + zradipou_upp*0.00051_JPRB)
  zradipou_upp=2.0_JPRB*zradipou_upp
  zradipou_low=-1.56_JPRB + zradipou_low*(0.388_JPRB + zradipou_low*0.00051_JPRB)
  zradipou_low=2.0_JPRB*zradipou_low

  !
  ! nearest Hu & Stamnes and ice cloud coefficients
  ! For IR and Hires only
  If(  coef % id_sensor == sensor_id_ir  .Or. &
        & coef % id_sensor == sensor_id_hi ) Then
     Do jc = 1, nfrequencies
        ido = channels(jc)
        zomega = coef % ff_cwn(ido)
        zmin = 1.e+06_JPRB
        Do jo = 1, nvalhusta
           zdom = Abs( zomega - zhustaom(jo) )
           If (zdom < zmin) Then
              indh(jc) = jo
              zmin = zdom
           End If
        End Do
        zmin = 1.e+06_JPRB
        Do jo = 1, nvalice
           zdom = Abs( zomega - ziceom(jo) )
           If (zdom < zmin) Then
              indi(jc) = jo
              zmin = zdom
           End If
        End Do
     End Do
  End If

  ! secant of zenith angle
  !
  zview(:) = 1._JPRB / Cos( profiles(:)%zenangle /180._JPRB * pi )

  ! pressure layering (Pa)
  !
  Do jl = 1, nprofiles
     Do jk = 1, nlevels
        p1 = Max( cld_profiles(jl)%ph(jk)  *hundred, repsc )
        p2 = Max( cld_profiles(jl)%ph(jk+1)*hundred, repsc )
        zdzst(jl,jk) = -1._JPRB *rgp /gravity /rm *Log(p1/p2)
     End Do
  End Do


  Do jk = 1, nlevels
     Do jl = 1, nprofiles

        zlwgkg = Max(cld_profiles(jl)%clw(jk)*1000._JPRB,0._JPRB)
        ziwgkg = Max(cld_profiles(jl)%ciw(jk)*1000._JPRB,0._JPRB)
        If (cld_profiles(jl)%cc(jk) > (2._JPRB*repsc)) Then
           zlwgkg = zlwgkg / cld_profiles(jl)%cc(jk)
           ziwgkg = ziwgkg / cld_profiles(jl)%cc(jk)
        Else
           zlwgkg=0._JPRB
           ziwgkg=0._JPRB
        End If

        ! Liquid and ice water paths (g.m-2)
        zdp = cld_profiles(jl)%ph(jk+1) - cld_profiles(jl)%ph(jk)
        zdp = zdp * 100._JPRB
        zflwp(jl) = zlwgkg*zdp / gravity
        zfiwp(jl) = ziwgkg*zdp / gravity

        ! Liquid and ice water contents (kg.m-3)
        zlwc(jl) = (zlwgkg/1000._JPRB) * (cld_profiles(jl)%p(jk)*100._JPRB * rm) /&
              & (rgp *cld_profiles(jl)%t(jk))
        ziwc(jl) = (ziwgkg/1000._JPRB) * (cld_profiles(jl)%p(jk)*100._JPRB * rm) /&
              & (rgp *cld_profiles(jl)%t(jk))

        zradip(jl) = 1._JPRB
        zradlp(jl) = 1._JPRB
        zmultl     = 0._JPRB

        If (zflwp(jl) > 0._JPRB) Then

           ! Liquid particle radius (micro-meters)

           If ( profiles(jl)%skin%surftype == surftype_land ) Then
              zradlp(jl)=10._JPRB
           Else
              zradlp(jl)=13._JPRB
           End If

           !
           ! Extra check that droplet r_e goes neither below 2.5 microns nor above 60 microns (Hu & Stamnes limits)
           !
           zradlp(jl)=Max(zradlp(jl),low_re(1))
           zradlp(jl)=Min(zradlp(jl),upp_re(3))
        Endif

        If (zfiwp(jl) > 0._JPRB) Then
           ! Ice particle radius (micro-meters)
           !
           If (cld_profiles(jl)%kradip == 0) Then ! Ou-Liou
             !
             ! Ou and Liou, 1995, Atmos. Res., 35, 127-138.
             !
             ztempc = cld_profiles(jl)%t(jk) - rtt
             zradip(jl)=326.3_JPRB+ &
                       & ztempc*(12.42_JPRB + ztempc*(0.197_JPRB + ztempc*0.0012_JPRB))
             ! and convert this to the "generalized" effective diameter (see McFarquhar et al 2003)
             zradip(jl)=-1.56_JPRB + zradip(jl)*(0.388_JPRB + zradip(jl)*0.00051_JPRB)
             zradip(jl)=2.0_JPRB*zradip(jl)
             !
             ! Take Ou-Liou scheme as being valid only between -20C and -60C
             !
             zradip(jl)=max(zradip(jl),zradipou_low)
             zradip(jl)=min(zradip(jl),zradipou_upp)
             !
           Elseif (cld_profiles(jl)%kradip == 1) Then ! Wyser
             !
             ! Wyser et al., reference details here..., McFarquhar et al. (2003)
             ! Note two typos in McFarquhar paper:
             ! (a) reference to "r" should be "4" in final equation
             ! (b) T_k should be (273-T_k) (see original Wyser paper)
             !
             bwyser=-2.0_JPRB
             ! Wyser's IWC is in g.m-3
             if (cld_profiles(jl)%t(jk) < 273._JPRB) bwyser = bwyser &
               & +(0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**1.5_JPRB)*Log10(1000._JPRB*ziwc(jl)/50._JPRB))
             zradip(jl)=377.4_JPRB + bwyser*(203.3_JPRB + bwyser*(37.91_JPRB + bwyser*2.3696_JPRB)) ! Wyser definition
             nft=(sqrt(3._JPRB)+4._JPRB)/(3._JPRB*sqrt(3._JPRB))
             zradip(jl)=zradip(jl)/nft ! convert to intermediate definition (see Table 1 of McFarquhar et al.)
             zradip(jl)=2._JPRB*4._JPRB*zradip(jl)*sqrt(3._JPRB)/9._JPRB ! includes factor of 2 to convert McFarquhar"s r_ge to D_ge
            !
           Elseif (cld_profiles(jl)%kradip == 2) Then ! Boudala et al.
             !
             ! Boudala et al., 2002, Int. J. Climatol., 22, 1267-1284.
             !
             ztempc=cld_profiles(jl)%t(jk)-rtt
             zradip(jl)=53.005_JPRB*((ziwc(jl)*1000._JPRB)**0.06_JPRB)*exp(0.013_JPRB*ztempc)
             !
           Elseif (cld_profiles(jl)%kradip == 3) Then ! McFarquhar
             !
             ! McFarquhar et al. (2003)
             !
             amcfarq=1.78449_JPRB
             bmcfarq=0.281301_JPRB
             cmcfarq=0.0177166_JPRB
             zmcfarq=1000.0_JPRB*ziwc(jl) ! Put IWC in g.m-3
             zradip(jl)=10.0_JPRB**(amcfarq+(bmcfarq*Log10(zmcfarq))+&
                                    & (cmcfarq*Log10(zmcfarq)*Log10(zmcfarq)))
             zradip(jl)=2.0_JPRB*zradip(jl) ! Includes factor of 2 to convert McFarquhar's r_ge to D_ge
             !
!           Else
!             errMessage = 'Wrong kradip'
!             errorstatus(:) = errorstatus_fatal
!             Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
!             Return
           End If
           !
        End If
     End Do  ! profiles

     Do jc = 1, nchannels
        freq = polarisations(jc,2)
        ido = channels(freq)
        idp = lprofiles(freq)

        ! Micro Waves
        If(  coef % id_sensor == sensor_id_mw ) Then
           zdz    = zdzst(idp,jk) * cld_profiles(idp)%t(jk)
           zfreq  = coef%frequency_ghz (ido)
           ! Water
           ! Liebe et al., 1989, IEEE Trans. Antennas Propag., 37, 1617-1623.
           theta = 300.0_JPRB / cld_profiles(idp)%t(jk)
           fac1  = theta - 1.0_JPRB
           fac2  = fac1 * fac1

           eps0 = 77.66_JPRB + 103.3_JPRB * fac1
           eps1 = 5.48_JPRB
           eps2 = 3.51_JPRB

           fp   = 20.09_JPRB -  142.0_JPRB * fac1 + 294.0_JPRB * fac2
           fs   = 590.0_JPRB - 1500.0_JPRB * fac1

           fac3 = zfreq / fp
           fac4 = 1.0_JPRB + fac3 * fac3
           fac5 = zfreq / fs
           fac6 = 1.0_JPRB + fac5 * fac5

           zeps_r = (eps0 - eps1) / fac4 + (eps1 - eps2) / fac6 + eps2
           zeps_i = (eps0 - eps1) * fac3 / fac4 + (eps1 - eps2) * fac5 / fac6

           zhelp = zeps_i / ( (zeps_r + 2.0_JPRB)**2 + zeps_i**2 )
           zbeta = 0.18851441_JPRB * zfreq * zlwc(idp) * zhelp
           zodw = zbeta * zview(idp) * zdz

           ! Ice
           ! Hufford, 1991, Int. J. Infrared Millimeter Waves, 12, 677-681.
           zeps_r = 3.15_JPRB
           tk = cld_profiles(idp)%t(jk)

           If (tk > 273.16_JPRB) tk = 273.16_JPRB
           theta = 300.0_JPRB / tk
           a     = 1.0e-04_JPRB * (50.4_JPRB + 62.0_JPRB * (theta - 1.0_JPRB)) &
                & * Exp (-22.1_JPRB * (theta - 1.0_JPRB))
           b     = 1.0e-04_JPRB * (0.633_JPRB / theta - 0.131_JPRB) &
                & + (7.36e-04_JPRB * theta / (theta - 0.9927_JPRB)) ** 2
           zeps_i = a / zfreq + b * zfreq

           zhelp = zeps_i / ( (zeps_r + 2.0_JPRB)**2 + zeps_i**2 )
           zbeta = 0.18851441_JPRB * zfreq * ziwc(idp) * zhelp
           zodi = zbeta * zview(idp) * zdz

           cld_radiance % cldemis(jk,jc) = 1._JPRB - Exp( - zodw - zodi )

           ! Infra Red
        Elseif(  coef % id_sensor == sensor_id_ir  .Or. &
              & coef % id_sensor == sensor_id_hi ) Then

           If (zflwp(idp) > 0._JPRB) Then

           !
           ! Water cloud coefficients
           ! from Hu and Stamnes, 1993, J. Climate, Vol. 6, pp. 728-742
           !
           idh = indh(jc)
           zomega = coef % ff_cwn(ido)
           zdom = zomega - zhustaom(idh)
           !
           If (zdom >= 0.0_JPRB) Then                ! Better to interpolate the single-scattering properties
            If (zradlp(idp) < upp_re(1)) Then   ! themselves rather than the coefficients
             kext_hu_1=(zhustaa1(idh)*(zradlp(idp)**zhustab1(idh)))+zhustac1(idh)
             co_ssa_1=(zhustad1(idh)*(zradlp(idp)**zhustae1(idh)))+zhustaf1(idh)
             kext_hu_2=(zhustaa1(idh-1)*(zradlp(idp)**zhustab1(idh-1)))+zhustac1(idh-1)
             co_ssa_2=(zhustad1(idh-1)*(zradlp(idp)**zhustae1(idh-1)))+zhustaf1(idh-1)
            Elseif (zradlp(idp) >= low_re(2) .AND. zradlp(idp) < upp_re(2)) Then
             kext_hu_1=(zhustaa2(idh)*(zradlp(idp)**zhustab2(idh)))+zhustac2(idh)
             co_ssa_1=(zhustad2(idh)*(zradlp(idp)**zhustae2(idh)))+zhustaf2(idh)
             kext_hu_2=(zhustaa2(idh-1)*(zradlp(idp)**zhustab2(idh-1)))+zhustac2(idh-1)
             co_ssa_2=(zhustad2(idh-1)*(zradlp(idp)**zhustae2(idh-1)))+zhustaf2(idh-1)
            Elseif (zradlp(idp) >= low_re(3)) Then
             kext_hu_1=(zhustaa3(idh)*(zradlp(idp)**zhustab3(idh)))+zhustac3(idh)
             co_ssa_1=(zhustad3(idh)*(zradlp(idp)**zhustae3(idh)))+zhustaf3(idh)
             kext_hu_2=(zhustaa3(idh-1)*(zradlp(idp)**zhustab3(idh-1)))+zhustac3(idh-1)
             co_ssa_2=(zhustad3(idh-1)*(zradlp(idp)**zhustae3(idh-1)))+zhustaf3(idh-1)
            End If
            kabs_hu_1=kext_hu_1*co_ssa_1
            kabs_hu_2=kext_hu_2*co_ssa_2
            !
            ! Do the interpolation
            zmsalu = ((zdom)*(kabs_hu_2-kabs_hu_1)/(zhustaom(idh-1)-zhustaom(idh))) + kabs_hu_1
            !
           Elseif (zdom < 0.0_JPRB) Then
            If (zradlp(idp) < upp_re(1)) Then
             kext_hu_1=(zhustaa1(idh)*(zradlp(idp)**zhustab1(idh)))+zhustac1(idh)
             co_ssa_1=(zhustad1(idh)*(zradlp(idp)**zhustae1(idh)))+zhustaf1(idh)
             kext_hu_2=(zhustaa1(idh+1)*(zradlp(idp)**zhustab1(idh+1)))+zhustac1(idh+1)
             co_ssa_2=(zhustad1(idh+1)*(zradlp(idp)**zhustae1(idh+1)))+zhustaf1(idh+1)
            Elseif (zradlp(idp) >= low_re(2) .AND. zradlp(idp) < upp_re(2)) Then
             kext_hu_1=(zhustaa2(idh)*(zradlp(idp)**zhustab2(idh)))+zhustac2(idh)
             co_ssa_1=(zhustad2(idh)*(zradlp(idp)**zhustae2(idh)))+zhustaf2(idh)
             kext_hu_2=(zhustaa2(idh+1)*(zradlp(idp)**zhustab2(idh+1)))+zhustac2(idh+1)
             co_ssa_2=(zhustad2(idh+1)*(zradlp(idp)**zhustae2(idh+1)))+zhustaf2(idh+1)
            Elseif (zradlp(idp) >= low_re(3)) Then
             kext_hu_1=(zhustaa3(idh)*(zradlp(idp)**zhustab3(idh)))+zhustac3(idh)
             co_ssa_1=(zhustad3(idh)*(zradlp(idp)**zhustae3(idh)))+zhustaf3(idh)
             kext_hu_2=(zhustaa3(idh+1)*(zradlp(idp)**zhustab3(idh+1)))+zhustac3(idh+1)
             co_ssa_2=(zhustad3(idh+1)*(zradlp(idp)**zhustae3(idh+1)))+zhustaf3(idh+1)
            End If
            kabs_hu_1=kext_hu_1*co_ssa_1
            kabs_hu_2=kext_hu_2*co_ssa_2
            !
            ! Do the interpolation
            zmsalu = ((zdom)*(kabs_hu_2-kabs_hu_1)/(zhustaom(idh+1)-zhustaom(idh))) + kabs_hu_1
           End If
           !
           zmsalu = zview(idp) * (0.001_JPRB*zmsalu) ! Convert Hu/Stamnes to m^2g^-1 and put in viewing angle dependence

           Else

           zmsalu = 0._JPRB

           End If

           !
           ! Ice cloud emissivity
           ! See Baran et al. JAS, 417-427, 2003; Baran et al. JQSRT, 549-567, 2003; Baran & Francis QJRMS, 2004.

           If (zfiwp(idp) > 0._JPRB) Then

           idq = indi(jc)
           zomega = coef % ff_cwn(ido)
           zdom = zomega - ziceom(idq)
           If (zdom >= 0.0_JPRB) Then
            If (cld_profiles(idp)%kice == 0) Then ! Hexagonal columns
             kabs_ice_1=ziceclmna(idq)+(ziceclmnb(idq)/zradip(idp))+ &
              & (ziceclmnc(idq)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq)*zradip(idp))
             kabs_ice_2=ziceclmna(idq-1)+(ziceclmnb(idq-1)/zradip(idp))+ &
              & (ziceclmnc(idq-1)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq-1)*zradip(idp))
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             kabs_ice_1=ziceaggra(idq)+(ziceaggrb(idq)/zradip(idp))+ &
              & (ziceaggrc(idq)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq)*zradip(idp))
             kabs_ice_2=ziceaggra(idq-1)+(ziceaggrb(idq-1)/zradip(idp))+ &
              & (ziceaggrc(idq-1)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq-1)*zradip(idp))
!            Else
!             errMessage = 'Wrong kice'
!             errorstatus(:) = errorstatus_fatal
!             Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
!             Return
            End If
            !
            ! Do the interpolation
            zmsaiu = ((zdom)*(kabs_ice_2-kabs_ice_1)/(ziceom(idq-1)-ziceom(idq))) + kabs_ice_1
            !
           Elseif (zdom < 0.0_JPRB) Then
            If (cld_profiles(idp)%kice == 0) Then ! Hexagonal columns
             kabs_ice_1=ziceclmna(idq)+(ziceclmnb(idq)/zradip(idp))+ &
              & (ziceclmnc(idq)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq)*zradip(idp))
             kabs_ice_2=ziceclmna(idq+1)+(ziceclmnb(idq+1)/zradip(idp))+ &
              & (ziceclmnc(idq+1)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq+1)*zradip(idp))
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             kabs_ice_1=ziceaggra(idq)+(ziceaggrb(idq)/zradip(idp))+ &
              & (ziceaggrc(idq)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq)*zradip(idp))
             kabs_ice_2=ziceaggra(idq+1)+(ziceaggrb(idq+1)/zradip(idp))+ &
              & (ziceaggrc(idq+1)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq+1)*zradip(idp))
!            Else
!             errMessage = 'Wrong kice'
!             errorstatus(:) = errorstatus_fatal
!             Call Rttov_ErrorReport (errorstatus_fatal, errMessage, NameOfRoutine)
!             Return
            End If
            !
            ! Do the interpolation
            zmsaiu = ((zdom)*(kabs_ice_2-kabs_ice_1)/(ziceom(idq+1)-ziceom(idq))) + kabs_ice_1
           End If
           zmsaiu = zview(idp) * zmsaiu ! Add in viewing angle dependence

           Else

            zmsaiu = 0._JPRB

           End If

           !
           ! Cloud layer emissivity

 !          if(zflwp(idp) > 1200._JPRB)zflwp(idp)=1200._JPRB
 !          if(zflwp(idp) < 0._JPRB)zflwp(idp)=0._JPRB
 !          if(zfiwp(idp) > 1200._JPRB)zfiwp(idp)=1200._JPRB
 !          if(zfiwp(idp) < 0._JPRB)zfiwp(idp)=0._JPRB
 !          if(zmsalu    < 0._JPRB )zmsalu = 0._JPRB
 !          if(zmsalu    > 10._JPRB )zmsalu = 10._JPRB
 !          if(zmsaiu    < 0._JPRB )zmsaiu = 0._JPRB
 !          if(zmsaiu    > 10._JPRB )zmsaiu = 10._JPRB

            cld_radiance % cldemis(jk,jc)  =&
                 & 1._JPRB - Exp( -zmsalu*zflwp(idp) -zmsaiu*zfiwp(idp) )

 !           if(cld_radiance % cldemis(jk,jc)  > 1._JPRB )cld_radiance % cldemis(jk,jc)=1._JPRB
 !           if(cld_radiance % cldemis(jk,jc)  < 0._JPRB )cld_radiance % cldemis(jk,jc)=0._JPRB

        Else
           cld_radiance % cldemis(jk,jc) = 0._JPRB

        End If

     End Do ! channels
     !
  End Do ! Levels

End Subroutine rttov_emiscld
