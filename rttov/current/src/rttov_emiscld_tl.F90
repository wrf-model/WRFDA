Subroutine rttov_emiscld_tl(  &
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
        & cld_profiles_tl,   &! in
        & cld_radiance,      &! inout  (cldemis part only)
        & cld_radiance_tl)    ! inout  (cldemis part only)
  !
  ! Description:
  ! To compute tangent linear of cloud emissivity
  !                          in the micro-wave (0-200 GHz)
  !                      and in the infrared   (50-2860 cm-1)
  !                                            spectral ranges
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
  Integer(Kind=jpim),             Intent(in)    :: nfrequencies
  Integer(Kind=jpim),             Intent(in)    :: nchannels
  Integer(Kind=jpim),             Intent(in)    :: nprofiles
  Integer(Kind=jpim),             Intent(in)    :: nlevels
  Integer(Kind=jpim),             Intent(out)   :: errorstatus(nprofiles)
  Integer(Kind=jpim),             Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),             Intent(in)    :: lprofiles(nfrequencies)
  Integer(Kind=jpim),             Intent(in)    :: polarisations(nchannels,3)  ! Channel indices
  Type(profile_Type),             Intent(in)    :: profiles(nprofiles)
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles(nprofiles)
  Type(rttov_coef),          Intent(in)    :: coef
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance
  Type(profile_cloud_Type),  Intent(in)    :: cld_profiles_tl(nprofiles)
  Type(radiance_cloud_Type), Intent(inout) :: cld_radiance_tl


  ! Local parameters:
  !
  Real(Kind=jprb), Parameter               :: rtt=273.15_JPRB
  Real(Kind=jprb), Parameter               :: rgp = 8.314510_JPRB, rm = 0.0289644_JPRB
  Real(Kind=jprb), Parameter               :: repsc=1.e-12_JPRB
  Real(Kind=jprb), Parameter               :: rtice=rtt-23._JPRB
  Real(Kind=jprb), Parameter               :: hundred=100.0_JPRB
  Real(Kind=jprb), Parameter               :: rtou_upp=-20._JPRB
  Real(Kind=jprb), Parameter               :: rtou_low=-60._JPRB



  !local variables:
  Integer(Kind=jpim)                       :: jc, jk, jl, jo, ido, idp, idq, idh
  Integer(Kind=jpim)                       :: freq
  Real(Kind=jprb)                          :: zomega, zfreq
  Real(Kind=jprb)                          :: zlwgkg, ziwgkg, ztempc, zdp, p1, p2
  Real(Kind=jprb)                          :: zmsaiu, zdom, zmin, zmultl, zmsalu, zmsalu_tl
  Real(Kind=jprb)                          :: zdz, zodw, zodi, zhelp, zbeta
  Real(Kind=jprb)                          :: fac1, fac2, fac3, fac4, fac5, fac6
  Real(Kind=jprb)                          :: eps0, eps1, eps2, fp, fs, theta
  Real(Kind=jprb)                          :: a, b, tk
  Real(Kind=jprb)                          :: zeps_r, zeps_i
  Real(Kind=jprb)                          :: co_ssa_1, kext_hu_1, kabs_hu_1
  Real(Kind=jprb)                          :: co_ssa_2, kext_hu_2, kabs_hu_2
  Real(Kind=jprb)                          :: kabs_ice_1, kabs_ice_2
  Real(Kind=jprb)                          :: kabs_ice_1_tl, kabs_ice_2_tl
  Real(Kind=jprb)                          :: zradipou_upp, zradipou_low
  Real(Kind=jprb)                          :: bwyser, bwyser_tl, nft
  Real(Kind=jprb)                          :: amcfarq, bmcfarq, cmcfarq, zmcfarq, zmcfarq_tl

  Real(Kind=jprb)                          :: zlwgkg_tl, ziwgkg_tl, ztempc_tl, zdp_tl, p1_tl, p2_tl
  Real(Kind=jprb)                          :: zmsaiu_tl
  Real(Kind=jprb)                          :: zdz_tl, zodw_tl, zodi_tl, zhelp_tl, zbeta_tl
  Real(Kind=jprb)                          :: fac1_tl, fac2_tl, fac3_tl, fac4_tl, fac5_tl, fac6_tl
  Real(Kind=jprb)                          :: eps0_tl, fp_tl, fs_tl, theta_tl
  Real(Kind=jprb)                          :: a_tl, b_tl, tk_tl
  Real(Kind=jprb)                          :: zeps_r_tl, zeps_i_tl

  Real(Kind=jprb)                          :: znum,    zden
  Real(Kind=jprb)                          :: znum_tl, zden_tl
  Real(Kind=jprb)                          :: value

  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_emiscld_tl'
  !

  ! Local arrays:
  !
  Integer(Kind=jpim), Dimension(nchannels) :: indh
  Integer(Kind=jpim), Dimension(nchannels) :: indi
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zradlp, zradip
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zflwp, zfiwp, zlwc, ziwc
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zview
  Real(Kind=jprb)                          :: zdzst(nprofiles, nlevels)

  Real(Kind=jprb), DIMENSION(nprofiles)    :: zradip_tl
  Real(Kind=jprb), DIMENSION(nprofiles)    :: zflwp_tl, zfiwp_tl, zlwc_tl, ziwc_tl
  Real(Kind=jprb)                          :: zdzst_tl(nprofiles, nlevels)

  !- End of header --------------------------------------------------------

  !---------------------------------------------------------------------------
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

  !
  ! secant of zenith angle
  !
  zview(:) = 1._JPRB / Cos( profiles(:)%zenangle /180._JPRB * pi )

  ! pressure layering (Pa)
  !
  Do jl = 1, nprofiles
     Do jk = 1, nlevels
        value = cld_profiles(jl)%ph(jk)  *hundred
        If ( value < repsc ) Then
           p1_tl = 0._JPRB
           p1    = repsc
        Else
           p1_tl = cld_profiles_tl(jl)%ph(jk)  *hundred
           p1    = value
        End if

        value = cld_profiles(jl)%ph(jk+1)  *hundred
        If ( value < repsc ) Then
           p2_tl = 0._JPRB
           p2    = repsc
        Else
           p2_tl = cld_profiles_tl(jl)%ph(jk+1)  *hundred
           p2    = value
        End if


        zdzst_tl(jl,jk) = -1._JPRB *rgp /gravity /rm * ( p1_tl/p1 - p2_tl/p2 )
        zdzst(jl,jk)    = -1._JPRB *rgp /gravity /rm *Log(p1/p2)
     End Do
  End Do

  Do jk = 1, nlevels
     Do jl = 1, nprofiles

        value = cld_profiles(jl)%clw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           zlwgkg_tl = 0._JPRB
           zlwgkg    = 0._JPRB
        Else
           zlwgkg_tl = cld_profiles_tl(jl)%clw(jk)*1000._JPRB
           zlwgkg    = value
        End If

        value = cld_profiles(jl)%ciw(jk)*1000._JPRB
        If( value < 0._JPRB ) Then
           ziwgkg_tl = 0._JPRB
           ziwgkg    = 0._JPRB
        Else
           ziwgkg_tl = cld_profiles_tl(jl)%ciw(jk)*1000._JPRB
           ziwgkg    = value
        End If

        If (cld_profiles(jl)%cc(jk) > (2._JPRB*repsc)) Then

           zlwgkg_tl = (zlwgkg_tl * cld_profiles(jl)%cc(jk)  -&
                       & zlwgkg    * cld_profiles_tl(jl)%cc(jk)  ) /&
                      & cld_profiles(jl)%cc(jk)**2
           ziwgkg_tl = (ziwgkg_tl * cld_profiles(jl)%cc(jk)  -&
                       & ziwgkg    * cld_profiles_tl(jl)%cc(jk)  ) /&
                      & cld_profiles(jl)%cc(jk)**2

           zlwgkg = zlwgkg / cld_profiles(jl)%cc(jk)
           ziwgkg = ziwgkg / cld_profiles(jl)%cc(jk)
        Else
           zlwgkg_tl = 0._JPRB
           ziwgkg_tl = 0._JPRB
           zlwgkg    = 0._JPRB
           ziwgkg    = 0._JPRB
        End If

        ! Liquid and ice water paths (g.m-2)
        zdp_tl = cld_profiles_tl(jl)%ph(jk+1) - cld_profiles_tl(jl)%ph(jk)
        zdp    = cld_profiles(jl)%ph(jk+1)    - cld_profiles(jl)%ph(jk)

        zdp_tl = zdp_tl * 100._JPRB
        zdp    = zdp * 100._JPRB

        zflwp_tl(jl) = ( zlwgkg_tl*zdp + zlwgkg*zdp_tl ) / gravity
        zflwp(jl)    =   zlwgkg*zdp / gravity

        zfiwp_tl(jl) = ( ziwgkg_tl*zdp + ziwgkg*zdp_tl ) / gravity
        zfiwp(jl)    = ziwgkg*zdp / gravity

        ! Liquid and ice water contents (kg.m-3)
        znum_tl =  zlwgkg_tl/1000._JPRB * cld_profiles(jl)%p(jk)*100._JPRB * rm + &
                  & zlwgkg/1000._JPRB    * cld_profiles_tl(jl)%p(jk)*100._JPRB * rm
        znum    =  zlwgkg/1000._JPRB    * cld_profiles(jl)%p(jk)*100._JPRB * rm
        zden_tl =  rgp *cld_profiles_tl(jl)%t(jk)
        zden    =  rgp *cld_profiles(jl)%t(jk)

        zlwc_tl(jl) =  (znum_tl * zden -  znum * zden_tl) / zden**2
        zlwc(jl)    =  znum / zden

        znum_tl     =  ziwgkg_tl/1000._JPRB * cld_profiles(jl)%p(jk)*100._JPRB * rm + &
                  & ziwgkg/1000._JPRB    * cld_profiles_tl(jl)%p(jk)*100._JPRB * rm
        znum        =  ziwgkg/1000._JPRB    * cld_profiles(jl)%p(jk)*100._JPRB * rm
        ziwc_tl(jl) =  (znum_tl * zden -  znum * zden_tl) / zden**2
        ziwc(jl)    =  znum / zden

        zradip(jl) = 1._JPRB
        zradlp(jl) = 1._JPRB
        zmultl     = 0._JPRB
        zmsalu     = 0._JPRB

        zradip_tl(jl) = 0._JPRB

        If (zflwp(jl) > 0._JPRB) Then
           ! Liquid particle radius (micro-meters)

           If ( profiles(jl)%skin%surftype == surftype_land ) Then
              zradlp(jl)=10._JPRB
           Else
              zradlp(jl)=13._JPRB
           End If
        End If


        If (zfiwp(jl) > 0._JPRB) Then
           ! Ice particle radius (micro-meters)
           !
           If (cld_profiles(jl)%kradip == 0) Then ! Ou-Liou
             !
             ! Ou and Liou, 1995, Atmos. Res., 35, 127-138.
             !
             ztempc_tl = cld_profiles_tl(jl)%t(jk)
             ztempc = cld_profiles(jl)%t(jk) - rtt
             zradip_tl(jl)= ( 12.42_JPRB + 2._JPRB*0.197_JPRB*ztempc + 3._JPRB*0.0012_JPRB*ztempc*ztempc )*ztempc_tl
             zradip(jl)=326.3_JPRB+ &
                       & ztempc*(12.42_JPRB + ztempc*(0.197_JPRB + ztempc*0.0012_JPRB))
             ! and convert this to the "generalized" effective diameter (see McFarquhar et al 2003)
             zradip_tl(jl)=0.388_JPRB*zradip_tl(jl) + 0.00051_JPRB*2*zradip(jl)*zradip_tl(jl)
             zradip(jl)=-1.56_JPRB + zradip(jl)*(0.388_JPRB + zradip(jl)*0.00051_JPRB)
             zradip_tl(jl)=2.0_JPRB*zradip_tl(jl)
             zradip(jl)=2.0_JPRB*zradip(jl)
             !
             ! Take Ou-Liou scheme as being valid only between 20C and 60C
             !
             If (zradip(jl) < zradipou_low .or. zradip(jl) > zradipou_upp) zradip_tl(jl) = 0._JPRB
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
             bwyser_tl = 0._JPRB
             bwyser=-2.0_JPRB
             If (cld_profiles(jl)%t(jk) < 273._JPRB) Then
               bwyser_tl = - 0.001_JPRB*1.5_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**0.5_JPRB)*cld_profiles_tl(jl)%t(jk) &
                                    & *Log10(1000._JPRB*ziwc(jl)/50._JPRB) &
                             & + 0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**1.5_JPRB)*ziwc_tl(jl) / ziwc(jl) / log(10._JPRB)
               ! Wyser's IWC is in g.m-3
               bwyser=bwyser+(0.001_JPRB*((273._JPRB-cld_profiles(jl)%t(jk))**1.5_JPRB)*Log10(1000._JPRB*ziwc(jl)/50._JPRB))
             Endif
             zradip_tl(jl)=( 203.3_JPRB + 2*bwyser*37.91_JPRB + 3*bwyser*bwyser*2.3696_JPRB ) *bwyser_tl
             zradip(jl)=377.4_JPRB + bwyser*(203.3_JPRB + bwyser*(37.91_JPRB + bwyser*2.3696_JPRB)) ! Wyser definition
             nft=(sqrt(3._JPRB)+4._JPRB)/(3._JPRB*sqrt(3._JPRB))
             zradip_tl(jl)=zradip_tl(jl)/nft
             zradip(jl)=zradip(jl)/nft ! convert to intermediate definition (see Table 1 of McFarquhar et al.)
             zradip_tl(jl)=2._JPRB*4._JPRB*zradip_tl(jl)*sqrt(3._JPRB)/9._JPRB
             zradip(jl)=2._JPRB*4._JPRB*zradip(jl)*sqrt(3._JPRB)/9._JPRB ! includes factor of 2 to convert McFarquhar"s r_ge to D_ge
            !
           Elseif (cld_profiles(jl)%kradip == 2) Then ! Boudala et al.
             !
             ! Boudala et al., 2002, Int. J. Climatol., 22, 1267-1284.
             !
             ztempc_tl=cld_profiles_tl(jl)%t(jk)
             ztempc=cld_profiles(jl)%t(jk)-rtt
             zradip_tl(jl)=53.005_JPRB*(1000._JPRB**0.06_JPRB)*(ziwc(jl)**(0.06_JPRB-1))*ziwc_tl(jl)*exp(0.013_JPRB*ztempc)&
                          & +53.005_JPRB*((ziwc(jl)*1000._JPRB)**0.06_JPRB)*0.013_JPRB*ztempc_tl*exp(0.013_JPRB*ztempc)
             zradip(jl)=53.005_JPRB*((ziwc(jl)*1000._JPRB)**0.06_JPRB)*exp(0.013_JPRB*ztempc)
             !
           Elseif (cld_profiles(jl)%kradip == 3) Then ! McFarquhar
             !
             ! McFarquhar et al. (2003)
             !
             amcfarq=1.78449_JPRB
             bmcfarq=0.281301_JPRB
             cmcfarq=0.0177166_JPRB
             zmcfarq_tl=1000.0_JPRB*ziwc_tl(jl)
             zmcfarq=1000.0_JPRB*ziwc(jl) ! Put IWC in g.m-3
             zradip_tl(jl)= 10.0_JPRB**(amcfarq+bmcfarq*Log10(zmcfarq)+cmcfarq*Log10(zmcfarq)*Log10(zmcfarq) ) &
                            & * ( bmcfarq + 2._JPRB*cmcfarq*Log10(zmcfarq) ) / zmcfarq * zmcfarq_tl
             zradip(jl)=10.0_JPRB**(amcfarq+(bmcfarq*Log10(zmcfarq))+&
                                    & (cmcfarq*Log10(zmcfarq)*Log10(zmcfarq)))
             zradip_tl(jl)=2.0_JPRB*zradip_tl(jl)
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

           zdz    = zdzst(idp,jk)    * cld_profiles(idp)%t(jk)
           zdz_tl = zdzst_tl(idp,jk) * cld_profiles(idp)%t(jk) +&
                   & zdzst(idp,jk)    * cld_profiles_tl(idp)%t(jk)

           zfreq  = coef%frequency_ghz (ido)

           ! Water
           ! Liebe et al., 1989, IEEE Trans. Antennas Propag., 37, 1617-1623.
           theta    = 300.0_JPRB / cld_profiles(idp)%t(jk)
           theta_tl =-300.0_JPRB * cld_profiles_tl(idp)%t(jk) /&
                          & cld_profiles(idp)%t(jk)**2

           fac1     = theta - 1.0_JPRB
           fac1_tl  = theta_tl

           fac2     = fac1 * fac1
           fac2_tl  = 2._JPRB * fac1_tl * fac1

           eps0    = 77.66_JPRB + 103.3_JPRB * fac1
           eps0_tl = 103.3_JPRB * fac1_tl

           eps1 = 5.48_JPRB
           eps2 = 3.51_JPRB

           fp    = 20.09_JPRB -  142.0_JPRB * fac1 + 294.0_JPRB * fac2
           fp_tl = -142.0_JPRB * fac1_tl + 294.0_JPRB * fac2_tl

           fs    = 590.0_JPRB - 1500.0_JPRB * fac1
           fs_tl = - 1500.0_JPRB * fac1_tl

           fac3    =  zfreq / fp
           fac3_tl = -zfreq * fp_tl / fp**2

           fac4    = 1.0_JPRB + fac3 * fac3
           fac4_tl = 2._JPRB * fac3_tl * fac3

           fac5    =  zfreq / fs
           fac5_tl = -zfreq * fs_tl / fs**2

           fac6    = 1.0_JPRB + fac5 * fac5
           fac6_tl = 2._JPRB * fac5_tl * fac5

           zeps_r    = (eps0 - eps1) / fac4 + (eps1 - eps2) / fac6 + eps2
           zeps_r_tl = (eps0_tl * fac4 - (eps0 - eps1) * fac4_tl) / fac4**2 - &
                        & (eps1 - eps2) * fac6_tl / fac6**2

           zeps_i    = (eps0 - eps1) * fac3 / fac4 +&
                      & (eps1 - eps2) * fac5 / fac6
           zeps_i_tl = ((eps0_tl*fac3 + (eps0 - eps1)*fac3_tl)* fac4 - &
                       & (eps0 - eps1) * fac3 * fac4_tl )  / fac4**2       &
                                  & +                                      &
                       & (eps1 - eps2) * (fac5_tl * fac6 -  fac5 * fac6_tl ) /&
                        & fac6**2

           znum    = zeps_i
           znum_tl = zeps_i_tl
           zden    = (zeps_r + 2.0_JPRB)**2 + zeps_i**2
           zden_tl = 2._JPRB * zeps_r_tl * (zeps_r + 2.0_JPRB) + &
                    & 2._JPRB * zeps_i_tl * zeps_i

           zhelp    = znum / zden
           zhelp_tl = (znum_tl * zden -  znum * zden_tl) / zden**2


           zbeta    = 0.18851441_JPRB * zfreq * zlwc(idp) * zhelp
           zbeta_tl = 0.18851441_JPRB * zfreq *&
                      & (zlwc_tl(idp) * zhelp + zlwc(idp) * zhelp_tl)

           zodw     = zbeta * zview(idp) * zdz
           zodw_tl  = zbeta_tl * zview(idp) * zdz   +&
                     & zbeta    * zview(idp) * zdz_tl

           ! Ice
           ! Hufford, 1991, Int. J. Infrared Millimeter Waves, 12, 677-681.
           zeps_r    = 3.15_JPRB
           zeps_r_tl =0._JPRB

           tk    = cld_profiles(idp)%t(jk)
           tk_tl = cld_profiles_tl(idp)%t(jk)

           If (tk > 273.16_JPRB) Then
              tk    = 273.16_JPRB
              tk_tl = 0._JPRB
           End if

           theta    =  300.0_JPRB / tk
           theta_tl = -300.0_JPRB * tk_tl / tk**2

           a     = 1.0e-04_JPRB * (50.4_JPRB + 62.0_JPRB * (theta - 1.0_JPRB)) &
                & * Exp (-22.1_JPRB * (theta - 1.0_JPRB))
           a_tl  = 1.0e-04_JPRB * Exp (-22.1_JPRB * (theta - 1.0_JPRB)) * &
                  & ( 62.0_JPRB * theta_tl +&
                     & (50.4_JPRB + 62.0_JPRB * (theta - 1.0_JPRB)) *  (-22.1_JPRB * theta_tl) )

           b     = 1.0e-04_JPRB * (0.633_JPRB / theta - 0.131_JPRB) &
                & + (7.36e-04_JPRB * theta / (theta - 0.9927_JPRB)) ** 2
           b_tl  = - 1.0e-04_JPRB * 0.633_JPRB * theta_tl / (theta **2)  &
                 & - 7.36e-04_JPRB**2 * 2 * theta / (theta - 0.9927_JPRB) &
                 & * 0.9927_JPRB / ((theta - 0.9927_JPRB)**2) * theta_tl

           zeps_i    = a / zfreq    + b * zfreq
           zeps_i_tl = a_tl / zfreq + b_tl * zfreq

           znum    = zeps_i
           znum_tl = zeps_i_tl
           zden    = (zeps_r + 2.0_JPRB)**2 + zeps_i**2
           ! note that zeps_r_tl is 0.
           zden_tl = 2._JPRB * zeps_r_tl * (zeps_r + 2.0_JPRB) +&
                    & 2._JPRB * zeps_i_tl * zeps_i

           zhelp    = znum / zden
           zhelp_tl = (znum_tl * zden -  znum * zden_tl) / zden**2


           zbeta    = 0.18851441_JPRB * zfreq * ziwc(idp) * zhelp
           zbeta_tl = 0.18851441_JPRB * zfreq *&
                 & (ziwc_tl(idp) * zhelp + ziwc(idp) * zhelp_tl)

           zodi    = zbeta * zview(idp) * zdz
           zodi_tl = zbeta_tl * zview(idp) * zdz + &
                    & zbeta    * zview(idp) * zdz_tl

           cld_radiance %    cldemis(jk,jc) = 1._JPRB - Exp( - zodw - zodi )
           cld_radiance_tl % cldemis(jk,jc) = ( zodw_tl + zodi_tl ) *&
                                              & Exp( - zodw - zodi )

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
           zmsalu_tl = 0._JPRB    ! since no liquid particle radius perturbations
           zmsalu = zview(idp) * (0.001_JPRB*zmsalu) ! Convert Hu/Stamnes to m^2g^-1 and put in viewing angle dependence

           Else

           zmsalu_tl = 0._JPRB
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
             kabs_ice_1_tl= ( ziceclmnd(idq) &
                          & - ziceclmnb(idq)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceclmnc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_1=ziceclmna(idq)+(ziceclmnb(idq)/zradip(idp))+ &
              & (ziceclmnc(idq)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq)*zradip(idp))
             kabs_ice_2_tl= ( ziceclmnd(idq-1) &
                          & - ziceclmnb(idq-1)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceclmnc(idq-1)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_2=ziceclmna(idq-1)+(ziceclmnb(idq-1)/zradip(idp))+ &
              & (ziceclmnc(idq-1)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq-1)*zradip(idp))
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             kabs_ice_1_tl= ( ziceaggrd(idq) &
                          & - ziceaggrb(idq)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceaggrc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_1=ziceaggra(idq)+(ziceaggrb(idq)/zradip(idp))+ &
              & (ziceaggrc(idq)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq)*zradip(idp))
             kabs_ice_2_tl= ( ziceaggrd(idq-1) &
                          & - ziceaggrb(idq-1)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceaggrc(idq-1)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
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
            zmsaiu_tl = ((zdom)*(kabs_ice_2_tl-kabs_ice_1_tl)/(ziceom(idq-1)-ziceom(idq))) + kabs_ice_1_tl
            zmsaiu = ((zdom)*(kabs_ice_2-kabs_ice_1)/(ziceom(idq-1)-ziceom(idq))) + kabs_ice_1
            !
           Elseif (zdom < 0.0_JPRB) Then
            If (cld_profiles(idp)%kice == 0) Then ! Hexagonal columns
             kabs_ice_1_tl= ( ziceclmnd(idq) &
                          & - ziceclmnb(idq)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceclmnc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_1=ziceclmna(idq)+(ziceclmnb(idq)/zradip(idp))+ &
              & (ziceclmnc(idq)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq)*zradip(idp))
             kabs_ice_2_tl= ( ziceclmnd(idq+1) &
                          & - ziceclmnb(idq+1)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceclmnc(idq+1)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_2=ziceclmna(idq+1)+(ziceclmnb(idq+1)/zradip(idp))+ &
              & (ziceclmnc(idq+1)/(zradip(idp)*zradip(idp)))+(ziceclmnd(idq+1)*zradip(idp))
            Elseif (cld_profiles(idp)%kice == 1) Then ! Aggregates
             kabs_ice_1_tl= ( ziceaggrd(idq) &
                          & - ziceaggrb(idq)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceaggrc(idq)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
             kabs_ice_1=ziceaggra(idq)+(ziceaggrb(idq)/zradip(idp))+ &
              & (ziceaggrc(idq)/(zradip(idp)*zradip(idp)))+(ziceaggrd(idq)*zradip(idp))
             kabs_ice_2_tl= ( ziceaggrd(idq+1) &
                          & - ziceaggrb(idq+1)/(zradip(idp)*zradip(idp)) &
                          & - 2*ziceaggrc(idq+1)/(zradip(idp)*zradip(idp)*zradip(idp)) )*zradip_tl(idp)
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
            zmsaiu_tl = ((zdom)*(kabs_ice_2_tl-kabs_ice_1_tl)/(ziceom(idq+1)-ziceom(idq))) + kabs_ice_1_tl
            zmsaiu = ((zdom)*(kabs_ice_2-kabs_ice_1)/(ziceom(idq+1)-ziceom(idq))) + kabs_ice_1
           End If
           zmsaiu_tl = zview(idp) * zmsaiu_tl
           zmsaiu = zview(idp) * zmsaiu ! Add in viewing angle dependence

           Else

            zmsaiu_tl = 0._JPRB
            zmsaiu = 0._JPRB

           End If

           !
           ! Cloud layer emissivity
           cld_radiance    % cldemis(jk,jc)  =&
                 & 1._JPRB - Exp( -zmsalu*zflwp(idp) -zmsaiu*zfiwp(idp) )
           cld_radiance_tl % cldemis(jk,jc)  =&
                  & ( zmsalu * zflwp_tl(idp) +&
                    & zmsaiu_tl * zfiwp(idp) + zmsaiu * zfiwp_tl(idp) ) *&
                 & Exp( -zmsalu*zflwp(idp) -zmsaiu*zfiwp(idp) )
           !
        Else
           cld_radiance    % cldemis(jk,jc) = 0._JPRB
           cld_radiance_tl % cldemis(jk,jc) = 0._JPRB

        End If

     End Do ! channels
     !
  End Do ! Levels


End Subroutine rttov_emiscld_tl
