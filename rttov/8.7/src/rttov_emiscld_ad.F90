Subroutine rttov_emiscld_ad(  &
      & errorstatus,       &! out
      & nfrequencies,      &! in
      & nchannels,         &! in
      & nprofiles,         &! in
      & nlevels,           &! in
      & channels,          &! in
      & polarisations,     &! in
      & lprofiles,         &! in
      & profiles,          &! in  (surftype and zenangle)
      & coef,              &! in  (frequencies mw/ir/hi)
      & cld_profiles,      &! in
      & cld_profiles_ad,   &! inout
      & cld_radiance,      &! inout  (cldemis part only)
      & cld_radiance_ad)    ! inout  (cldemis part only)
  !
  ! Description:
  ! To compute adjoint of cloud emissivity in the micro-wave (0-200 GHz)
  !                                    and in the infrared   (50-2860 cm-1)
  !                                                          spectral ranges
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
  !  ?+0.1   06/10/04  Added errorstatus to the arguments.
  !                    Changed stop statements to returns.
  !                    Added description/method/history header. (J Cameron)
  !  1.2   29/03/2005  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  ! A user guide and technical documentation is available at
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/index.html
  !
  ! Declarations:
  ! Modules used:

  Use rttov_const, Only :    &
       & pi                   ,&
       & gravity              ,&
       & surftype_land        ,&
       & nvalhusta            ,&
       & zhustaom             ,&
       & zhustaa1             ,&
       & zhustaa2             ,&
       & zhustaa3             ,&
       & zhustab1             ,&
       & zhustab2             ,&
       & zhustab3             ,&
       & zhustac1             ,&
       & zhustac2             ,&
       & zhustac3             ,&
       & zhustad1             ,&
       & zhustad2             ,&
       & zhustad3             ,&
       & zhustae1             ,&
       & zhustae2             ,&
       & zhustae3             ,&
       & zhustaf1             ,&
       & zhustaf2             ,&
       & zhustaf3             ,&
       & low_re               ,&
       & upp_re               ,&
       & nvalice              ,&
       & ziceom               ,&
       & ziceclmna            , ziceclmnb            ,&
       & ziceclmnc            , ziceclmnd            ,&
       & ziceaggra            , ziceaggrb            ,&
       & ziceaggrc            , ziceaggrd            ,&
       & sensor_id_ir         ,&
       & sensor_id_mw         ,&
       & sensor_id_hi         ,&
       & errorstatus_success  ,&
       & errorstatus_fatal

  Use rttov_types, Only :    &
       & rttov_coef           ,&
       & profile_Type         ,&
       & profile_cloud_Type   ,&
       & radiance_cloud_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  !subroutine arguments:
  Integer(Kind=jpim),        Intent(in)    :: nfrequencies
  Integer(Kind=jpim),        Intent(in)    :: nchannels
  Integer(Kind=jpim),        Intent(in)    :: nprofiles
  Integer(Kind=jpim),        Intent(in)    :: nlevels
  Integer(Kind=jpim),        Intent(out)   :: errorstatus(nprofiles)
  Integer(Kind=jpim),        Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),        Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),        Intent(in)    :: lprofiles(nfrequencies)
  Type(profile_Type),        Intent(in)    :: profiles(nprofiles)
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles)
  Type(rttov_coef),          Intent(in)    :: coef
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance

  Type(profile_cloud_Type),  Intent(inout) :: cld_profiles_ad(nprofiles)
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance_ad



  ! Local parameters:
  !
  Real(Kind=jprb), Parameter               :: rtt=273.15_JPRB
  Real(Kind=jprb), Parameter               :: rgp = 8.314510_JPRB, rm = 0.0289644_JPRB
  Real(Kind=jprb), Parameter               :: repsc=1.e-12_JPRB
  Real(Kind=jprb), Parameter               :: rtice=rtt-23._JPRB
  Real(Kind=jprb), Parameter               :: hundred=100.0_JPRB
  Real(Kind=jprb), Parameter               :: rtou_upp=-20._JPRB
  Real(Kind=jprb), Parameter               :: rtou_low=-60



  !local variables:
  Integer(Kind=jpim)                       :: jc, jk, jl, jo, ido, idp, idq, idh, freq
  Real(Kind=jprb)                          :: zomega, zfreq
  Real(Kind=jprb)                          :: p1, p2
  Real(Kind=jprb)                          :: zmsaiu, zdom, zmin, zmultl, zmsalu
  Real(Kind=jprb)                          :: zdz, zodw, zodi
  Real(Kind=jprb)                          :: fac1, fac2, fac3, fac4, fac5, fac6
  Real(Kind=jprb)                          :: eps0, eps1, eps2, fp, fs, theta
  Real(Kind=jprb)                          :: a, b, tk

  Real(Kind=jprb)                          :: zlwgkg_ad, ziwgkg_ad, ztempc_ad, zdp_ad, p1_ad, p2_ad
  Real(Kind=jprb)                          :: zmsaiu_ad
  Real(Kind=jprb)                          :: zdz_ad, zodw_ad, zodi_ad, zhelp_ad, zbeta_ad
  Real(Kind=jprb)                          :: fac1_ad, fac2_ad, fac3_ad, fac4_ad, fac5_ad, fac6_ad
  Real(Kind=jprb)                          :: eps0_ad, fp_ad, fs_ad, theta_ad
  Real(Kind=jprb)                          :: a_ad, b_ad, tk_ad
  Real(Kind=jprb)                          :: zeps_r_ad, zeps_i_ad
  Real(Kind=jprb)                          :: kabs_ice_1_ad, kabs_ice_2_ad, bwyser_ad, zmcfarq_ad

  Real(Kind=jprb)                          :: znum_ad, zden_ad
  Real(Kind=jprb)                          :: value, value1, value2
  Real(Kind=jprb)                          :: co_ssa_1, kext_hu_1, kabs_hu_1
  Real(Kind=jprb)                          :: co_ssa_2, kext_hu_2, kabs_hu_2
  Real(Kind=jprb)                          :: kabs_ice_1, kabs_ice_2
  Real(Kind=jprb)                          :: zradipou_upp, zradipou_low
  Real(Kind=jprb)                          :: nft
  Real(Kind=jprb)                          :: amcfarq, bmcfarq, cmcfarq

  Real(Kind=jprb) :: zeps_r_1, zeps_i_1
  Real(Kind=jprb) :: zeps_r_2, zeps_i_2
  Real(Kind=jprb) :: znum_3, zden_3
  Real(Kind=jprb) :: znum_4, zden_4
  Real(Kind=jprb) :: zhelp_1, zbeta_1
  Real(Kind=jprb) :: zhelp_2, zbeta_2

  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_emiscld_ad'
  !


  ! Local arrays:
  !
  Integer(Kind=jpim), DIMENSION(nchannels) :: indh
  Integer(Kind=jpim), DIMENSION(nchannels) :: indi
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zradlp, zradip
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zlwgkg, ziwgkg, ztempc, zdp
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zflwp, zfiwp, zlwc, ziwc
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zview, bwyser, zmcfarq
  Real(Kind=jprb)                          :: zdzst(nprofiles, nlevels)
  Real(Kind=jprb), DIMENSION(nprofiles)    :: znum_1, zden_1, znum_2, zden_2

  Real(Kind=jprb), DIMENSION(nprofiles)    :: zradip_ad
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zflwp_ad, zfiwp_ad, zlwc_ad, ziwc_ad
  Real(Kind=jprb), DIMENSION(nprofiles,nlevels) :: zdzst_ad
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zradip0, zradip1

  !- End of header --------------------------------------------------------

  !---------------------------------------------------------------------------
  !All input NWP profiles have the same number of levels

  errorstatus(:) = errorstatus_success


  ! All channels are IR or MW according to the coefficient
  ! structure information

  ! repeat direct code

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
     Do jc = 1, nchannels
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
        ! For TL coding replace Min/Max functions by tests
        !p1 = Max( cld_profiles(jl)%ph(jk)  *hundred, repsc )
        !p2 = Max( cld_profiles(jl)%ph(jk+1)*hundred, repsc )
        value = cld_profiles(jl)%ph(jk)  *hundred
        If ( value < repsc ) Then
           p1    = repsc
        Else
           p1    = value
        End if

        value = cld_profiles(jl)%ph(jk+1)  *hundred
        If ( value < repsc ) Then
           p2    = repsc
        Else
           p2    = value
        End if

        zdzst(jl,jk)    = -1._JPRB *rgp /gravity /rm *Log(p1/p2)
     End Do
  End Do

  ! Loop on levels is external to direct and AD codes
  Do jk = 1, nlevels

     !
     ! -- Direct computation for jk
     !
     Do jl = 1, nprofiles

        value = cld_profiles(jl)%clw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           zlwgkg(jl) = 0._JPRB
        Else
           zlwgkg(jl) = value
        End If

        value = cld_profiles(jl)%ciw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           ziwgkg(jl) = 0._JPRB
        Else
           ziwgkg(jl) = value
        End If

        If (cld_profiles(jl)%cc(jk) > (2._JPRB*repsc)) Then
           zlwgkg(jl) = zlwgkg(jl) / cld_profiles(jl)%cc(jk)
           ziwgkg(jl) = ziwgkg(jl) / cld_profiles(jl)%cc(jk)
        Else
           zlwgkg(jl) = 0._JPRB
           ziwgkg(jl) = 0._JPRB
        End If

        ! Liquid and ice water paths (g.m-2)
        zdp(jl) = cld_profiles(jl)%ph(jk+1)    - cld_profiles(jl)%ph(jk)

        zdp(jl) = zdp(jl) * 100._JPRB

        zflwp(jl) = zlwgkg(jl)*zdp(jl) / gravity

        zfiwp(jl) = ziwgkg(jl)*zdp(jl) / gravity

        ! Liquid and ice water contents (kg.m-3)
        znum_1(jl) =  zlwgkg(jl)/1000._JPRB    * cld_profiles(jl)%p(jk)*100._JPRB * rm
        zden_1(jl) =  rgp *cld_profiles(jl)%t(jk)
        zlwc(jl)   =  znum_1(jl) / zden_1(jl)

        znum_2(jl) =  ziwgkg(jl)/1000._JPRB    * cld_profiles(jl)%p(jk)*100._JPRB * rm
        zden_2(jl) =  rgp *cld_profiles(jl)%t(jk)
        ziwc(jl)   =  znum_2(jl) / zden_2(jl)

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
             ztempc(jl) = cld_profiles(jl)%t(jk) - rtt
             zradip(jl)=326.3_JPRB+ &
                       & ztempc(jl)*(12.42_JPRB + ztempc(jl)*(0.197_JPRB + ztempc(jl)*0.0012_JPRB))
             ! and convert this to the "generalized" effective diameter (see McFarquhar et al 2003)
             zradip0(jl) = zradip(jl)
             zradip(jl)=-1.56_JPRB + zradip(jl)*(0.388_JPRB + zradip(jl)*0.00051_JPRB)
             zradip(jl)=2.0_JPRB*zradip(jl)
             !
             ! Take Ou-Liou scheme as being valid only between -20C and -60C
             !
             zradip1(jl) = zradip(jl)
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
             bwyser(jl)=-2.0_JPRB
             ! Wyser's IWC is in g.m-3
             if (cld_profiles(jl)%t(jk) < 273._JPRB) bwyser(jl) = bwyser(jl) &
               & +(0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**1.5_JPRB)*Log10(1000._JPRB*ziwc(jl)/50._JPRB))
             zradip(jl)=377.4_JPRB + bwyser(jl)*(203.3_JPRB + bwyser(jl)*(37.91_JPRB + bwyser(jl)*2.3696_JPRB)) ! Wyser definition
             nft=(sqrt(3._JPRB)+4._JPRB)/(3._JPRB*sqrt(3._JPRB))
             zradip(jl)=zradip(jl)/nft ! convert to intermediate definition (see Table 1 of McFarquhar et al.)
             zradip(jl)=2._JPRB*4._JPRB*zradip(jl)*sqrt(3._JPRB)/9._JPRB ! includes factor of 2 to convert McFarquhar"s r_ge to D_ge
            !
           Elseif (cld_profiles(jl)%kradip == 2) Then ! Boudala et al.
             !
             ! Boudala et al., 2002, Int. J. Climatol., 22, 1267-1284.
             !
             ztempc(jl)=cld_profiles(jl)%t(jk)-rtt
             zradip(jl)=53.005_JPRB*((ziwc(jl)*1000._JPRB)**0.06_JPRB)*exp(0.013_JPRB*ztempc(jl))
             !
           Elseif (cld_profiles(jl)%kradip == 3) Then ! McFarquhar
             !
             ! McFarquhar et al. (2003)
             !
             amcfarq=1.78449_JPRB
             bmcfarq=0.281301_JPRB
             cmcfarq=0.0177166_JPRB
             zmcfarq(jl) =1000.0_JPRB*ziwc(jl) ! Put IWC in g.m-3
             zradip(jl)=10.0_JPRB**(amcfarq+(bmcfarq*Log10(zmcfarq(jl) ))+&
                                    & (cmcfarq*Log10(zmcfarq(jl) )*Log10(zmcfarq(jl) )))
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
     End Do ! profiles

     zradip_ad(:)   = 0._JPRB
     zflwp_ad(:)    = 0._JPRB
     zfiwp_ad(:)    = 0._JPRB
     ziwc_ad(:)     = 0._JPRB
     zlwc_ad(:)     = 0._JPRB
     zdzst_ad(:,jk) = 0._JPRB

     !
     ! -- Direct computation for jk and jc
     !
     Do jc = 1, nchannels
        freq = polarisations(jc,2)
        ido = channels(freq)
        idp = lprofiles(freq)

        ! Micro Waves
        If(  coef % id_sensor == sensor_id_mw ) Then

           zdz    = zdzst(idp,jk)    * cld_profiles(idp)%t(jk)
           zfreq  = coef%frequency_ghz (ido)

           ! Water
           ! Liebe et al., 1989, IEEE Trans. Antennas Propag., 37, 1617-1623.
           theta = 300.0_JPRB / cld_profiles(idp)%t(jk)
           fac1  = theta - 1.0_JPRB
           fac2  = fac1 * fac1

           eps0  = 77.66_JPRB + 103.3_JPRB * fac1
           eps1  = 5.48_JPRB
           eps2  = 3.51_JPRB

           fp    = 20.09_JPRB -  142.0_JPRB * fac1 + 294.0_JPRB * fac2
           fs    = 590.0_JPRB - 1500.0_JPRB * fac1

           fac3    =  zfreq / fp
           fac4    = 1.0_JPRB + fac3 * fac3
           fac5    =  zfreq / fs
           fac6    = 1.0_JPRB + fac5 * fac5

           zeps_r_1 = (eps0 - eps1) / fac4 + (eps1 - eps2) / fac6 + eps2
           zeps_i_1 = (eps0 - eps1) * fac3 / fac4 +&
                 & (eps1 - eps2) * fac5 / fac6

           znum_3  = zeps_i_1
           zden_3  = (zeps_r_1 + 2.0_JPRB)**2 + zeps_i_1**2

           zhelp_1 = znum_3 / zden_3
           zbeta_1 = 0.18851441_JPRB * zfreq * zlwc(idp) * zhelp_1
           zodw    = zbeta_1 * zview(idp) * zdz

           ! Ice
           ! Hufford, 1991, Int. J. Infrared Millimeter Waves, 12, 677-681.
           zeps_r_2 = 3.15_JPRB
           tk       = cld_profiles(idp)%t(jk)

           If (tk > 273.16_JPRB) Then
              tk    = 273.16_JPRB
           End if

           theta    =  300.0_JPRB / tk
           a     = 1.0e-04_JPRB * (50.4_JPRB + 62.0_JPRB * (theta - 1.0_JPRB)) &
                & * Exp (-22.1_JPRB * (theta - 1.0_JPRB))
           b     = 1.0e-04_JPRB * (0.633_JPRB / theta - 0.131_JPRB) &
                & + (7.36e-04_JPRB * theta / (theta - 0.9927_JPRB)) ** 2
           zeps_i_2  = a / zfreq    + b * zfreq

           znum_4  = zeps_i_2
           zden_4  = (zeps_r_2 + 2.0_JPRB)**2 + zeps_i_2**2
           zhelp_2 = znum_4 / zden_4
           zbeta_2 = 0.18851441_JPRB * zfreq * ziwc(idp) * zhelp_2
           zodi    = zbeta_2 * zview(idp) * zdz

           cld_radiance %    cldemis(jk,jc) = 1._JPRB - Exp( - zodw - zodi )

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
           ! See Baran et al. JAS, 417-427, 2003; Baran et al. JQSRT, 549-567, 2003.

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
           cld_radiance % cldemis(jk,jc)  =&
                 & 1._JPRB - Exp( -zmsalu*zflwp(idp) -zmsaiu*zfiwp(idp) )
           !
        Else
           cld_radiance % cldemis(jk,jc) = 0._JPRB

        End If

        !
        ! -- Adjoint computation for jk and jc
        !
        ! Micro Waves
        If(  coef % id_sensor == sensor_id_mw ) Then

           ! Ice
           ! Hufford, 1991, Int. J. Infrared Millimeter Waves, 12, 677-681.
           zodw_ad = cld_radiance_ad % cldemis(jk,jc) * Exp( - zodw - zodi )
           zodi_ad = cld_radiance_ad % cldemis(jk,jc) * Exp( - zodw - zodi )
           cld_radiance_ad % cldemis(jk,jc) = 0._JPRB

           zbeta_ad = zodi_ad * zview(idp) * zdz
           zdz_ad   = zodi_ad * zbeta_2    * zview(idp)
           zodi_ad  = 0._JPRB

           ziwc_ad(idp) = ziwc_ad(idp) + zbeta_ad * 0.18851441_JPRB * zfreq * zhelp_2
           zhelp_ad     = zbeta_ad * 0.18851441_JPRB * zfreq * ziwc(idp)


           znum_ad =  zhelp_ad / zden_4
           zden_ad = -zhelp_ad * znum_4 / zden_4**2
           zhelp_ad = 0._JPRB

           zeps_i_ad = zden_ad * 2._JPRB * zeps_i_2
           zden_ad   = 0._JPRB

           zeps_i_ad = zeps_i_ad + znum_ad
           znum_ad   = 0._JPRB


           a_ad      = zeps_i_ad / zfreq
           b_ad      = zeps_i_ad * zfreq
           zeps_i_ad = 0._JPRB

           theta_ad = &
                 & - 1.0e-04_JPRB * 0.633_JPRB / (theta**2) * b_ad &
                 & - 2*(7.36e-04_JPRB * theta / (theta - 0.9927_JPRB)) &
                 & * 7.36e-04_JPRB * 0.9927_JPRB / ((theta - 0.9927_JPRB)**2) * b_ad
           b_ad     = 0._JPRB

           theta_ad = theta_ad &
                 & + 1.0e-04_JPRB * 62.0_JPRB * exp (-22.1_JPRB * (theta - 1.0_JPRB)) * a_ad &
                 & - 22.1_JPRB * 1.0e-04_JPRB * (50.4_JPRB + 62.0_JPRB * (theta - 1.0_JPRB)) &
                 & * exp (-22.1_JPRB * (theta - 1.0_JPRB)) * a_ad
           a_ad     = 0._JPRB

           If (cld_profiles(idp)%t(jk) > 273.16_JPRB) Then
              tk_ad = 0._JPRB
           Else
              tk    = cld_profiles(idp)%t(jk)
              tk_ad = -300.0_JPRB * theta_ad / tk**2
           End if
           theta_ad = 0._JPRB

           cld_profiles_ad(idp)%t(jk) = cld_profiles_ad(idp)%t(jk) + tk_ad

           ! Water
           ! Liebe et al., 1989, IEEE Trans. Antennas Propag., 37, 1617-1623.



           zbeta_ad = zodw_ad * zview(idp) * zdz
           zdz_ad   = zdz_ad + zodw_ad * zview(idp) * zbeta_1
           zodw_ad  = 0._JPRB


           zlwc_ad(idp) = zlwc_ad(idp) + zbeta_ad * 0.18851441_JPRB * zfreq * zhelp_1
           zhelp_ad     = zbeta_ad *  0.18851441_JPRB * zfreq * zlwc(idp)

           znum_ad  =  zhelp_ad / zden_3
           zden_ad  = -zhelp_ad * znum_3/ zden_3**2
           zhelp_ad = 0._JPRB


           zeps_r_ad = zden_ad * 2._JPRB * (zeps_r_1 + 2.0_JPRB)
           zeps_i_ad = zden_ad * 2._JPRB * zeps_i_1
           zden_ad   = 0._JPRB

           zeps_i_ad = zeps_i_ad + znum_ad
           znum_ad   = 0._JPRB

           eps0_ad   = zeps_i_ad * fac3 / fac4
           fac3_ad   = zeps_i_ad * (eps0 - eps1) / fac4
           fac4_ad   =-zeps_i_ad * (eps0 - eps1) * fac3 / fac4**2
           fac5_ad   = zeps_i_ad * (eps1 - eps2) / fac6
           fac6_ad   =-zeps_i_ad * (eps1 - eps2) * fac5 / fac6**2
           zeps_i_ad = 0._JPRB

           eps0_ad   = eps0_ad + zeps_r_ad / fac4
           fac4_ad   = fac4_ad - zeps_r_ad * (eps0 - eps1) / fac4**2
           fac6_ad   = fac6_ad - zeps_r_ad * (eps1 - eps2) / fac6**2
           zeps_r_ad = 0._JPRB


           fac5_ad = fac5_ad +  2._JPRB * fac6_ad * fac5
           fac6_ad = 0._JPRB

           fs_ad   = -zfreq * fac5_ad / fs**2
           fac5_ad = 0._JPRB


           fac3_ad = fac3_ad + 2._JPRB * fac4_ad * fac3
           fac4_ad = 0._JPRB

           fp_ad   = -zfreq * fac3_ad / fp**2
           fac3_ad = 0._JPRB


           fac1_ad = - 1500.0_JPRB * fs_ad
           fs_ad   = 0._JPRB

           fac1_ad = fac1_ad - 142.0_JPRB * fp_ad
           fac2_ad =           294.0_JPRB * fp_ad
           fp_ad   = 0._JPRB

           fac1_ad = fac1_ad + 103.3_JPRB * eps0_ad
           eps0_ad = 0._JPRB

           fac1_ad = fac1_ad + 2._JPRB * fac2_ad * fac1
           fac2_ad = 0._JPRB

           theta_ad = fac1_ad

           cld_profiles_ad(idp)%t(jk) = cld_profiles_ad(idp)%t(jk)  -&
                 & 300.0_JPRB * theta_ad / cld_profiles(idp)%t(jk)**2
           theta_ad = 0._JPRB


           cld_profiles_ad(idp)%t(jk) = cld_profiles_ad(idp)%t(jk) +&
                 & zdz_ad * zdzst(idp,jk)
           zdzst_ad(idp,jk) = zdzst_ad(idp,jk) + zdz_ad * cld_profiles(idp)%t(jk)


        Elseif(  coef % id_sensor == sensor_id_ir  .Or. &
              & coef % id_sensor == sensor_id_hi ) Then
              !
              ! Cloud layer emissivity
              !

           zmsaiu_ad     = cld_radiance_ad % cldemis(jk,jc) * zfiwp(idp) * &
                 & (1 - cld_radiance % cldemis(jk,jc))
           zflwp_ad(idp) = zflwp_ad(idp) + &
                 & cld_radiance_ad % cldemis(jk,jc) * zmsalu * &
                 & (1 - cld_radiance % cldemis(jk,jc))
           zfiwp_ad(idp) = zfiwp_ad(idp) + &
                 & cld_radiance_ad % cldemis(jk,jc) * zmsaiu * &
                 & (1 - cld_radiance % cldemis(jk,jc))
           cld_radiance_ad % cldemis(jk,jc)  = 0._JPRB

           !
           ! Ice cloud emissivity
           ! See Baran et al. JAS, 417-427, 2003; Baran et al. JQSRT, 549-567, 2003.

           If (zfiwp(idp) > 0._JPRB) Then

           idq = indi(jc)
           zdom = zomega - ziceom(idq)
           zmsaiu_ad = zview(idp) * zmsaiu_ad ! Add in viewing angle dependence
           If (zdom >= 0.0_JPRB) Then
            !
            ! Do the interpolation
            kabs_ice_2_ad = zdom/(ziceom(idq-1)-ziceom(idq))*zmsaiu_ad
            kabs_ice_1_ad = (1-zdom/(ziceom(idq-1)-ziceom(idq)))*zmsaiu_ad
            If (cld_profiles(idp)%kice == 0) Then ! Hexagonal columns
             zradip_ad(idp) = zradip_ad(idp) + &
              & (  ziceclmnd(idq) &
               & - ziceclmnb(idq)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceclmnc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_1_ad + &
              & (  ziceclmnd(idq-1) &
               & - ziceclmnb(idq-1)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceclmnc(idq-1)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_2_ad
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             zradip_ad(idp) = zradip_ad(idp) + &
              & (  ziceaggrd(idq) &
               & - ziceaggrb(idq)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceaggrc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_1_ad + &
              & (  ziceaggrd(idq-1) &
               & - ziceaggrb(idq-1)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceaggrc(idq-1)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_2_ad
            End If
            !
           Elseif (zdom < 0.0_JPRB) Then
            !
            ! Do the interpolation
            kabs_ice_2_ad = zdom/(ziceom(idq+1)-ziceom(idq))*zmsaiu_ad
            kabs_ice_1_ad = (1-zdom/(ziceom(idq+1)-ziceom(idq)))*zmsaiu_ad
            If (cld_profiles(idp)%kice == 0) Then ! Hexagonal columns
             zradip_ad(idp) = zradip_ad(idp) + &
              & (  ziceclmnd(idq) &
               & - ziceclmnb(idq)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceclmnc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_1_ad + &
              & (  ziceclmnd(idq+1) &
               & - ziceclmnb(idq+1)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceclmnc(idq+1)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_2_ad
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             zradip_ad(idp) = zradip_ad(idp) + &
              & (  ziceaggrd(idq) &
               & - ziceaggrb(idq)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceaggrc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_1_ad + &
              & (  ziceaggrd(idq+1) &
               & - ziceaggrb(idq+1)/(zradip(idp)*zradip(idp)) &
               & - 2*ziceaggrc(idq+1)/(zradip(idp)*zradip(idp)*zradip(idp)) ) * kabs_ice_2_ad
            End If

           End If

           !
           ! zradlp_ad = 0. since zradlp is constant for each geotype
           !
           End If
        Else
           cld_radiance_ad % cldemis(jk,jc)  = 0._JPRB
        End If

     End Do ! channels

     ! -- Adjoint computation for jk (remaining)
     !
     Do jl = 1, nprofiles
        If (zfiwp(jl) > 0._JPRB) Then
           ! Ice particle radius (micro-meters)
           !
           If (cld_profiles(jl)%kradip == 0) Then ! Ou-Liou
             !
             ! Ou and Liou, 1995, Atmos. Res., 35, 127-138.
             !

             If (zradip1(jl) > zradipou_upp .or. zradip1(jl) < zradipou_low) zradip_ad(jl) = 0._JPRB
             ! and convert this to the "generalized" effective diameter (see McFarquhar et al 2003)
             zradip_ad(jl)=2.0_JPRB*zradip_ad(jl)
             zradip_ad(jl)=0.388_JPRB*zradip_ad(jl)+2._JPRB*0.00051_JPRB*zradip0(jl)*zradip_ad(jl)

             ztempc_ad = ( 12.42_JPRB + 2._JPRB*0.197_JPRB*ztempc(jl) + 3._JPRB*0.0012_JPRB*ztempc(jl)*ztempc(jl) )*zradip_ad(jl)
             cld_profiles_ad(jl)%t(jk) = cld_profiles_ad(jl)%t(jk) + ztempc_ad
           Elseif (cld_profiles(jl)%kradip == 1) Then ! Wyser
             !
             ! Wyser et al., reference details here..., McFarquhar et al. (2003)
             ! Note two typos in McFarquhar paper:
             ! (a) reference to "r" should be "4" in final equation
             ! (b) T_k should be (273-T_k) (see original Wyser paper)
             !
             zradip_ad(jl) = 2._JPRB*4._JPRB*zradip_ad(jl)*sqrt(3._JPRB)/9._JPRB
             zradip_ad(jl)=zradip_ad(jl)/nft
             bwyser_ad = ( 203.3_JPRB + 2*bwyser(jl)*37.91_JPRB + 3*bwyser(jl)*bwyser(jl)*2.3696_JPRB ) * zradip_ad(jl)
             If (cld_profiles(jl)%t(jk) < 273._JPRB) Then
               cld_profiles_ad(jl)%t(jk) = cld_profiles_ad(jl)%t(jk) &
                 & - 1.5_JPRB * 0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**0.5_JPRB)*&
               & Log10(1000._JPRB*ziwc(jl)/50._JPRB) * bwyser_ad
               ziwc_ad(jl) = ziwc_ad(jl) &
                 & + 0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**1.5_JPRB) / ziwc(jl) / log(10._JPRB) &
                      & * bwyser_ad
             Endif
           Elseif (cld_profiles(jl)%kradip == 2) Then ! Boudala et al.
             !
             ! Boudala et al., 2002, Int. J. Climatol., 22, 1267-1284.
             !
             ziwc_ad(jl) = ziwc_ad(jl) &
                & + 53.005_JPRB*(1000._JPRB**0.06_JPRB)*(ziwc(jl)**(0.06_JPRB-1._JPRB))*exp(0.013_JPRB*ztempc(jl))*zradip_ad(jl)
             ztempc_ad = 53.005_JPRB*((ziwc(jl)*1000._JPRB)**0.06_JPRB)*exp(0.013_JPRB*ztempc(jl))*0.013_JPRB*zradip_ad(jl)
             cld_profiles_ad(jl)%t(jk) = cld_profiles_ad(jl)%t(jk) + ztempc_ad
             !
           Elseif (cld_profiles(jl)%kradip == 3) Then ! McFarquhar
             !
             ! McFarquhar et al. (2003)
             !
             zradip_ad(jl)=2.0_JPRB*zradip_ad(jl)
             zmcfarq_ad = 10.0_JPRB**(amcfarq+(bmcfarq*Log10(zmcfarq(jl) ))+ cmcfarq*Log10(zmcfarq(jl))*Log10(zmcfarq(jl)) ) &
                          & * (bmcfarq + 2._JPRB*cmcfarq*Log10(zmcfarq(jl))) / zmcfarq(jl) *zradip_ad(jl)
             ziwc_ad(jl) = ziwc_ad(jl) + 1000._JPRB*zmcfarq_ad
           End If
        End If

        ! Liquid and ice water contents (kg.m-3)
        znum_ad =  ziwc_ad(jl) / zden_2(jl)
        zden_ad = -ziwc_ad(jl) * znum_2(jl) / zden_2(jl)**2
        ziwc_ad(jl) =  0._JPRB

        cld_profiles_ad(jl)%t(jk) = cld_profiles_ad(jl)%t(jk) + zden_ad * rgp
        zden_ad = 0._JPRB

        ziwgkg_ad = znum_ad/1000._JPRB * cld_profiles(jl)%p(jk)*100._JPRB * rm
        cld_profiles_ad(jl)%p(jk) = cld_profiles_ad(jl)%p(jk) + znum_ad *&
              & ziwgkg(jl)/1000._JPRB * 100._JPRB * rm
        znum_ad     =  0._JPRB

        znum_ad =  zlwc_ad(jl) / zden_1(jl)
        zden_ad = -zlwc_ad(jl) * znum_1(jl) / zden_1(jl)**2
        zlwc_ad(jl) =  0._JPRB

        cld_profiles_ad(jl)%t(jk) = cld_profiles_ad(jl)%t(jk) + zden_ad * rgp
        zden_ad = 0._JPRB

        zlwgkg_ad = znum_ad/1000._JPRB * cld_profiles(jl)%p(jk)*100._JPRB * rm
        cld_profiles_ad(jl)%p(jk) = cld_profiles_ad(jl)%p(jk) +&
              & znum_ad * zlwgkg(jl)/1000._JPRB *100._JPRB * rm
        znum_ad =  0._JPRB

        ! Liquid and ice water paths (g.m-2)
        zdp_ad    = 0._JPRB
        zdp_ad    = zdp_ad    + zfiwp_ad(jl) * ziwgkg(jl) / gravity
        ziwgkg_ad = ziwgkg_ad + zfiwp_ad(jl) * zdp(jl) / gravity
        zfiwp_ad(jl) = 0._JPRB

        zdp_ad    = zdp_ad    + zflwp_ad(jl) * zlwgkg(jl) / gravity
        zlwgkg_ad = zlwgkg_ad + zflwp_ad(jl) * zdp(jl) / gravity
        zflwp_ad(jl) = 0._JPRB

        zdp_ad    = zdp_ad  * 100._JPRB

        cld_profiles_ad(jl)%ph(jk+1) = cld_profiles_ad(jl)%ph(jk+1) + zdp_ad
        cld_profiles_ad(jl)%ph(jk)   = cld_profiles_ad(jl)%ph(jk)   - zdp_ad
        zdp_ad = 0._JPRB

        value = cld_profiles(jl)%clw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           zlwgkg(jl) = 0._JPRB
        Else
           zlwgkg(jl) = value
        End If

        value = cld_profiles(jl)%ciw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           ziwgkg(jl) = 0._JPRB
        Else
           ziwgkg(jl) = value
        End If

        If (cld_profiles(jl)%cc(jk) > (2._JPRB*repsc)) Then

           cld_profiles_ad(jl)%cc(jk) = cld_profiles_ad(jl)%cc(jk) -&
                 & zlwgkg_ad * zlwgkg(jl) / cld_profiles(jl)%cc(jk)**2
           zlwgkg_ad = zlwgkg_ad / cld_profiles(jl)%cc(jk)

           cld_profiles_ad(jl)%cc(jk) = cld_profiles_ad(jl)%cc(jk) -&
                 & ziwgkg_ad * ziwgkg(jl) / cld_profiles(jl)%cc(jk)**2
           ziwgkg_ad = ziwgkg_ad / cld_profiles(jl)%cc(jk)

        Else
           zlwgkg_ad = 0._JPRB
           ziwgkg_ad = 0._JPRB
        End If

        cld_profiles_ad(jl)%ciw(jk) = cld_profiles_ad(jl)%ciw(jk) + ziwgkg_ad * 1000._JPRB

        cld_profiles_ad(jl)%clw(jk) = cld_profiles_ad(jl)%clw(jk) + zlwgkg_ad * 1000._JPRB

     End Do  ! profiles

  End Do ! Levels


  ! pressure layering (Pa)
  !
  Do jl = 1, nprofiles
     Do jk = nlevels, 1, -1

        value1 = cld_profiles(jl)%ph(jk)  *hundred
        If ( value1 < repsc ) Then
           p1    = repsc
        Else
           p1    = value1
        End if

        value2 = cld_profiles(jl)%ph(jk+1)  *hundred
        If ( value2 < repsc ) Then
           p2    = repsc
        Else
           p2    = value2
        End if

        p1_ad = -zdzst_ad(jl,jk) * rgp /gravity /rm /p1
        p2_ad =  zdzst_ad(jl,jk) * rgp /gravity /rm /p2
        zdzst_ad(jl,jk) = 0._JPRB

        If ( value2 >= repsc ) Then
           cld_profiles_ad(jl)%ph(jk+1) = cld_profiles_ad(jl)%ph(jk+1) +&
                 & p2_ad * hundred
        End if

        If ( value1 >= repsc ) Then
           cld_profiles_ad(jl)%ph(jk) = cld_profiles_ad(jl)%ph(jk) +&
                 & p1_ad * hundred
        End if

     End Do
  End Do


End Subroutine rttov_emiscld_ad
