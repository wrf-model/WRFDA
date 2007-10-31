Subroutine rttov_aitosu_tl( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nlevels,           &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & overlap_scheme,    &! in
        & profiles,          &! in  (cloud cover)
        & profiles_tl,       &! in  (cloud cover)
        & radiance ,         &! inout (cldemis input and
        & radiance_tl  )      ! inout  cs_wtao, cs_wsurf, wtao, wsurf in output)
  ! Description:
  ! AD of routine to compute the weights of the black-body-derived radiances
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
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  ! 1      07/10/2004  Added history
  ! 1.1    29/03/2005  Add end of header comment (J. Cameron)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_types, Only :    &
       & profile_cloud_Type   ,&
       & radiance_cloud_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit none

  !subroutine arguments:
  Integer(Kind=jpim),             Intent(in)    :: nfrequencies
  Integer(Kind=jpim),             Intent(in)    :: nchannels
  Integer(Kind=jpim),             Intent(in)    :: nprofiles
  Integer(Kind=jpim),             Intent(in)    :: nlevels
  Integer(Kind=jpim),             Intent(in)    :: lprofiles(nfrequencies)
  Integer(Kind=jpim),             Intent(in)    :: polarisations(nchannels,3)      ! Channel indices
  Integer(Kind=jpim),             Intent(in)    :: overlap_scheme
  Type(profile_cloud_Type),  Intent(in)    :: profiles(nprofiles)
  Type(radiance_cloud_Type), Intent(inout) :: radiance

  Type(profile_cloud_Type),  Intent(in)    :: profiles_tl(nprofiles)
  Type(radiance_cloud_Type), Intent(inout) :: radiance_tl



  ! Local parameters:
  !
  Real(Kind=jprb)   , Parameter :: repclc = 1.e-12_JPRB
  Real(Kind=jprb)   , Parameter :: unity = 1.0_JPRB
  !


  ! Local scalars:
  !
  Integer(Kind=jpim) :: jk, jk1, jl, idp, freq
  Real(Kind=jprb)    :: zcadj,    ztr1,    ztr2
  Real(Kind=jprb)    :: zcadj_tl, ztr1_tl, ztr2_tl
  Real(Kind=jprb)    :: znum,    zden
  Real(Kind=jprb)    :: znum_tl, zden_tl
  !


  ! Local arrays:
  !
  Real(Kind=jprb), Dimension(nchannels) :: zclear, zcloud, zcont, zsum
  Real(Kind=jprb), Dimension(nchannels) :: zclear_tl, zcloud_tl, zcont_tl, zsum_tl

  ! beware array shape zclm and zxxx is nchannels, nlevelsm+1
  Real(Kind=jprb)          :: zclm  (nlevels+1, nchannels)
  Real(Kind=jprb)          :: zcldfr(nlevels, nchannels)
  Real(Kind=jprb)          :: zcldem(nlevels, nchannels)
  Real(Kind=jprb)          :: zeffem(nlevels, nchannels)

  Real(Kind=jprb)          :: zclm_tl  (nlevels+1, nchannels)
  Real(Kind=jprb)          :: zcldfr_tl(nlevels, nchannels)
  Real(Kind=jprb)          :: zcldem_tl(nlevels, nchannels)
  Real(Kind=jprb)          :: zeffem_tl(nlevels, nchannels)

  Real(Kind=jprb)   :: value

  !- End of header --------------------------------------------------------

  !All input NWP profiles have the same number of levels

  radiance % cs_wtoa(:)  = 0._JPRB
  radiance % cs_wsurf(:) = 0._JPRB
  radiance % wtoa(:,:)   = 0._JPRB
  radiance % wsurf(:,:)  = 0._JPRB
  radiance_tl % cs_wtoa(:)  = 0._JPRB
  radiance_tl % cs_wsurf(:) = 0._JPRB
  radiance_tl % wtoa(:,:)   = 0._JPRB
  radiance_tl % wsurf(:,:)  = 0._JPRB


  Do jk = 1, nlevels
     Do jl = 1, nchannels
        freq = polarisations(jl,2)
        idp = lprofiles(freq)

        value = radiance % cldemis(jk,jl)
        If( value > (unity-repclc) ) Then
           zcldem(jk,jl)    = unity - repclc
           zcldem_tl(jk,jl) = 0._JPRB
        Else If( value < repclc ) Then
           zcldem(jk,jl)    = repclc
           zcldem_tl(jk,jl) = 0._JPRB
        Else
           zcldem(jk,jl)    = value
           zcldem_tl(jk,jl) = radiance_tl % cldemis(jk,jl)
        Endif


        value = profiles(idp)%cc(jk)
        If( value > (unity-repclc) ) Then
           zcldfr(jk,jl)    = unity - repclc
           zcldfr_tl(jk,jl) = 0._JPRB
        Else If( value < repclc ) Then
           zcldfr(jk,jl)    = repclc
           zcldfr_tl(jk,jl) = 0._JPRB
        Else
           zcldfr(jk,jl)    = value
           zcldfr_tl(jk,jl) = profiles_tl(idp)%cc(jk)
        Endif


        value = radiance % cldemis(jk,jl) * profiles(idp)%cc(jk)
        If( value > (unity-repclc) ) Then
           zeffem(jk,jl)    = unity - repclc
           zeffem_tl(jk,jl) = 0._JPRB
        Else If( value < repclc ) Then
           zeffem(jk,jl)    = repclc
           zeffem_tl(jk,jl) = 0._JPRB
        Else
           zeffem(jk,jl)    = value
           zeffem_tl(jk,jl) = radiance_tl % cldemis(jk,jl) * profiles(idp)%cc(jk)   +&
                             & radiance % cldemis(jk,jl)    * profiles_tl(idp)%cc(jk)
        Endif

     End Do
  End Do

  !----------------------------------------
  !
  ! Effect of cloudiness on toa radiances
  !
  ! Cloud cover matrix
  !
  ! zclm(jk2) is the obscuration factor by cloud layers between
  ! half-levels jk1 and jk2 as seen from jk1
  ! jk1 is the top of the atmosphere (toa)
  !

  jk1 = 1 ! level of top of the atmosphere

  zclm(:,:) = 0._JPRB
  zclear(:) = 1._JPRB
  zcloud(:) = 0._JPRB
  zcont(:)  = zeffem(jk1,:)

  zclm_tl(:,:) = 0._JPRB
  zclear_tl(:) = 0._JPRB
  zcloud_tl(:) = 0._JPRB
  zcont_tl(:)  = zeffem_tl(jk1,:)

  zsum(:)   = zcont(:)
  zclm(jk1+1,:) = zsum(:)

  zsum_tl(:)       = zcont_tl(:)
  zclm_tl(jk1+1,:) = zsum_tl(:)

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = jk1 , nlevels       ! layer loop
        Do jl = 1,nchannels
           If( zeffem(jk,jl) > zcloud(jl) ) Then
              znum    = zclear(jl) * (unity-zeffem(jk,jl))
              znum_tl = zclear_tl(jl) * (unity-zeffem(jk,jl)) -&
                       & zclear(jl)    * zeffem_tl(jk,jl)
           Else
              znum    = zclear(jl) * (unity-zcloud(jl))
              znum_tl = zclear_tl(jl) * (unity-zcloud(jl)) -&
                       & zclear(jl)    * zcloud_tl(jl)
           End If
           If ( zcloud(jl) > unity-repclc ) Then
              zden    = repclc
              zden_tl = 0._JPRB
           Else
              zden    = unity - zcloud(jl)
              zden_tl = -zcloud_tl(jl)
           Endif

           zclear(jl)    = znum / zden
           zclear_tl(jl) = (znum_tl * zden -  znum * zden_tl) / zden**2

           zclm(jk+1,jl)    = 1._JPRB - zclear(jl)
           zclm_tl(jk+1,jl) = -zclear_tl(jl)

           zcloud(jl)    = zeffem(jk,jl)
           zcloud_tl(jl) = zeffem_tl(jk,jl)
        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = jk1 + 1 , nlevels  ! layer loop
        Do jl = 1,nchannels
           znum    = zcont(jl)
           znum_tl = zcont_tl(jl)
           zden    = zeffem(jk-1,jl)
           zden_tl = zeffem_tl(jk-1,jl)
           ztr1    = znum / zden
           ztr1_tl = (znum_tl * zden -  znum * zden_tl) / zden**2

           znum    = 1._JPRB-(zsum(jl)-zcont(jl))-ztr1*zcldfr(jk-1,jl)
           znum_tl = -zsum_tl(jl) + zcont_tl(jl) - &
                     & ztr1_tl*zcldfr(jk-1,jl) - ztr1*zcldfr_tl(jk-1,jl)
           zden    = (1._JPRB-zcldfr(jk-1,jl))
           zden_tl = -zcldfr_tl(jk-1,jl)
           ztr2    = znum / zden
           ztr2_tl = (znum_tl * zden -  znum * zden_tl) / zden**2

           If( zcldfr(jk,jl) > zcldfr(jk-1,jl) ) Then
              zcadj    = zcldfr(jk-1,jl)
              zcadj_tl = zcldfr_tl(jk-1,jl)
           Else
              zcadj    = zcldfr(jk,jl)
              zcadj_tl = zcldfr_tl(jk,jl)
           End If

           zcont(jl) = zcldem(jk,jl)*(zcadj*(1._JPRB-zcldem(jk-1,jl))*ztr1  &
                & +(zcldfr(jk,jl)-zcadj)*ztr2)
           zcont_tl(jl) =&
                 & zcldem_tl(jk,jl)*(zcadj*(1._JPRB-zcldem(jk-1,jl))*ztr1  + &
                          & (zcldfr(jk,jl)-zcadj)*ztr2)               + &
                  & zcldem(jk,jl)*&
                   & (   zcadj_tl*(1._JPRB-zcldem(jk-1,jl))*ztr1    -  &
                       & zcadj   * zcldem_tl(jk-1,jl) *ztr1    +  &
                       & zcadj   *(1._JPRB-zcldem(jk-1,jl))*ztr1_tl +  &
                      & (zcldfr_tl(jk,jl)-zcadj_tl)*ztr2       +  &
                      & (zcldfr(jk,jl)   -zcadj   )*ztr2_tl    )

           zsum(jl)    = zsum(jl)    + zcont(jl)
           zsum_tl(jl) = zsum_tl(jl) + zcont_tl(jl)

           zclm(jk+1,jl)    = zsum(jl)
           zclm_tl(jk+1,jl) = zsum_tl(jl)
        Enddo
     Enddo
  Endif

  ! Weight computation
  !

  ! Contribution from clear-sky fraction
  radiance    % cs_wtoa(:) = 1._JPRB - zclm(nlevels+1,:)
  radiance_tl % cs_wtoa(:) =    - zclm_tl(nlevels+1,:)

!!$  Do jl = 1,nchannels
!!$     radiance % cs_wtoa(jl) = 1. - zclm(nlevels+1,jl)
!!$  Enddo

  ! Contribution from cloudy layers
  Do jk = jk1, nlevels  ! layer loop
     radiance    % wtoa( jk, : ) = zclm(jk+1,:)    - zclm(jk,:)
     radiance_tl % wtoa( jk, : ) = zclm_tl(jk+1,:) - zclm_tl(jk,:)
!!$     Do jl = 1,nchannels
!!$        radiance % wtoa( jk, jl ) = zclm(jk+1,jl) - zclm(jk,jl)
!!$     End Do
  End Do

  !----------------------------------------
  !
  ! Effect of cloudiness on surface radiances
  !

  !
  ! Cloud cover matrix
  !
  ! zclm(jk) is now the obscuration factor by cloud layers between
  ! surface and jk as seen from the surface
  !

  zclm(:,:) = 0._JPRB
  zcloud(:) = 0._JPRB
  zclear(:) = 1._JPRB
  zcont(:)  = zeffem(nlevels,:)
  zsum(:)   = zcont(:)
  zclm(nlevels,:) = zsum(:)

  zclm_tl(:,:) = 0._JPRB
  zcloud_tl(:) = 0._JPRB
  zclear_tl(:) = 0._JPRB
  zcont_tl(:)  = zeffem_tl(nlevels,:)
  zsum_tl(:)   = zcont_tl(:)
  zclm_tl(nlevels,:) = zsum_tl(:)

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = nlevels, 1, -1       ! layer loop
        Do jl = 1,nchannels
           If( zeffem(jk,jl) > zcloud(jl) ) Then
              znum    = zclear(jl)*(1._JPRB-zeffem(jk,jl))
              znum_tl = zclear_tl(jl) -zeffem_tl(jk,jl)
           Else
              znum    = zclear(jl)*(1._JPRB-zcloud(jl))
              znum_tl = zclear_tl(jl) - zcloud(jl)
           End If

           If ( zcloud(jl) > unity-repclc ) Then
              zden    = repclc
              zden_tl = 0._JPRB
           Else
              zden    = 1._JPRB - zcloud(jl)
              zden_tl = - zcloud_tl(jl)
           End If
           zclear(jl)    = znum / zden
           zclear_tl(jl) = (znum_tl * zden -  znum * zden_tl) / zden**2

           zclm(jk,jl)    = 1._JPRB - zclear(jl)
           zclm_tl(jk,jl) = - zclear_tl(jl)

           zcloud(jl)    = zeffem(jk,jl)
           zcloud_tl(jl) = zeffem_tl(jk,jl)
        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = nlevels - 1, 1, -1  ! layer loop
        Do jl = 1,nchannels
           znum    = zcont(jl)
           znum_tl = zcont_tl(jl)
           zden    = zeffem(jk+1,jl)
           zden_tl = zeffem_tl(jk+1,jl)
           ztr1    = znum / zden
           ztr1_tl = (znum_tl * zden -  znum * zden_tl) / zden**2



           znum    = 1._JPRB-(zsum(jl)-zcont(jl))-ztr1*zcldfr(jk+1,jl)
           znum_tl = -zsum_tl(jl) + zcont_tl(jl) - &
                     & ztr1_tl*zcldfr(jk+1,jl) - ztr1*zcldfr_tl(jk+1,jl)
           zden    = (1._JPRB-zcldfr(jk+1,jl))
           zden_tl = -zcldfr_tl(jk+1,jl)
           ztr2    = znum / zden
           ztr2_tl = (znum_tl * zden -  znum * zden_tl) / zden**2

           If( zcldfr(jk,jl) > zcldfr(jk+1,jl) ) Then
              zcadj    = zcldfr(jk+1,jl)
              zcadj_tl = zcldfr_tl(jk+1,jl)
           Else
              zcadj    = zcldfr(jk,jl)
              zcadj_tl = zcldfr_tl(jk,jl)
           End If

           zcont(jl) = zcldem(jk,jl)*(zcadj*(1._JPRB-zcldem(jk+1,jl))*ztr1  &
                & +(zcldfr(jk,jl)-zcadj)*ztr2)
           zcont_tl(jl) =&
                 & zcldem_tl(jk,jl)*(zcadj*(1._JPRB-zcldem(jk+1,jl))*ztr1  +&
                          & (zcldfr(jk,jl)-zcadj)*ztr2)               +&
                  & zcldem(jk,jl)*&
                   & (   zcadj_tl*(1._JPRB-zcldem(jk+1,jl))*ztr1    -  &
                       & zcadj   * zcldem_tl(jk+1,jl) *ztr1    +  &
                       & zcadj   *(1._JPRB-zcldem(jk+1,jl))*ztr1_tl +  &
                      & (zcldfr_tl(jk,jl)-zcadj_tl)*ztr2       +  &
                      & (zcldfr(jk,jl)   -zcadj   )*ztr2_tl    )


           zsum(jl)    = zsum(jl)    + zcont(jl)
           zsum_tl(jl) = zsum_tl(jl) + zcont_tl(jl)

           zclm(jk,jl)    = zsum(jl)
           zclm_tl(jk,jl) = zsum_tl(jl)

        End Do
     End Do
  End If

  ! Weight computation
  !

  ! Contribution from clear-sky fraction

  radiance    % cs_wsurf(:) = 1._JPRB - zclm(1,:)
  radiance_tl % cs_wsurf(:) = - zclm_tl(1,:)

  ! Contribution from cloudy layers

  Do jk = 1, nlevels  ! layer loop
     radiance    % wsurf( jk, : ) = zclm(jk,:)    - zclm(jk+1,:)
     radiance_tl % wsurf( jk, : ) = zclm_tl(jk,:) - zclm_tl(jk+1,:)
  Enddo

End Subroutine Rttov_aitosu_tl
