Subroutine rttov_aitosu_ad( &
        & nfrequencies,      &! in
        & nchannels,         &! in
        & nprofiles,         &! in
        & nlevels,           &! in
        & polarisations,     &! in
        & lprofiles,         &! in
        & overlap_scheme,    &! in
        & profiles,          &! in
        & profiles_ad,       &! inout
        & radiance ,         &! inout
        & radiance_ad  )      ! inout
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
  Implicit None

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

  Type(profile_cloud_Type),  Intent(inout) :: profiles_ad(nprofiles)
  Type(radiance_cloud_Type), Intent(inout) :: radiance_ad



  ! Local parameters:
  !
  Real(Kind=jprb)   , Parameter :: repclc = 1.e-12_JPRB
  Real(Kind=jprb)   , Parameter :: unity = 1.0_JPRB
  !


  ! Local scalars:
  !
  Integer(Kind=jpim) :: jk, jk1, jl, idp
  Integer(Kind=jpim) :: freq
  Real(Kind=jprb)    :: zcadj_ad, ztr1_ad, ztr2_ad
  Real(Kind=jprb)    :: znum_ad, zden_ad
  !


  ! Local arrays:
  !
  Real(Kind=jprb), Dimension(nchannels) :: zclear_ad, zcloud_ad, zcont_ad, zsum_ad

  ! beware array shape zclm and zxxx is nchannels, nlevelsm+1
  Real(Kind=jprb)          :: zclm  (nlevels+1, nchannels)
  Real(Kind=jprb)          :: zcldfr(nlevels, nchannels)
  Real(Kind=jprb)          :: zcldem(nlevels, nchannels)
  Real(Kind=jprb)          :: zeffem(nlevels, nchannels)

  Real(Kind=jprb)          :: zclm_ad  (nlevels+1, nchannels)
  Real(Kind=jprb)          :: zcldfr_ad(nlevels, nchannels)
  Real(Kind=jprb)          :: zcldem_ad(nlevels, nchannels)
  Real(Kind=jprb)          :: zeffem_ad(nlevels, nchannels)

  ! arrays for tracing forward model
  Real(Kind=jprb)          :: zcadj(nlevels+1, nchannels)
  Real(Kind=jprb)          :: ztr1(nlevels+1, nchannels)
  Real(Kind=jprb)          :: ztr2(nlevels+1, nchannels)
  Real(Kind=jprb)          :: zden(nlevels+1, nchannels)
  Real(Kind=jprb)          :: znum(nlevels+1, nchannels)
  Real(Kind=jprb) :: zclear(0:nlevels+1, nchannels)
  Real(Kind=jprb) :: zcloud(0:nlevels+1, nchannels)
  Real(Kind=jprb) :: zcont(nlevels, nchannels)
  Real(Kind=jprb) :: zsum (nlevels, nchannels)


  Real(Kind=jprb) :: test_1(nlevels, nchannels)
  Real(Kind=jprb) :: test_2(nlevels, nchannels)
  Real(Kind=jprb) :: test_3(nlevels, nchannels)

  Real(Kind=jprb)   :: value

  !- End of header --------------------------------------------------------

  !All input NWP profiles have the same number of levels

  radiance % cs_wtoa(:)  = 0._JPRB
  radiance % cs_wsurf(:) = 0._JPRB
  radiance % wtoa(:,:)   = 0._JPRB
  radiance % wsurf(:,:)  = 0._JPRB

  ! Init AD variables
  zcldfr_ad(:,:) = 0._JPRB
  zcldem_ad(:,:) = 0._JPRB
  zeffem_ad(:,:) = 0._JPRB

  Do jk = 1, nlevels
     Do jl = 1, nchannels
        freq = polarisations(jl,2)
        idp = lprofiles(freq)

        value = radiance % cldemis(jk,jl)
        test_1(jk,jl) = value
        If( value > (unity-repclc) ) Then
           zcldem(jk,jl)    = unity - repclc
        Else If( value < repclc ) Then
           zcldem(jk,jl)    = repclc
        Else
           zcldem(jk,jl)    = value
        Endif


        value = profiles(idp)%cc(jk)
        test_2(jk,jl) = value
        If( value > (unity-repclc) ) Then
           zcldfr(jk,jl)    = unity - repclc
        Else If( value < repclc ) Then
           zcldfr(jk,jl)    = repclc
        Else
           zcldfr(jk,jl)    = value
        Endif


        value = radiance % cldemis(jk,jl) * profiles(idp)%cc(jk)
        test_3(jk,jl) = value
        If( value > (unity-repclc) ) Then
           zeffem(jk,jl)    = unity - repclc
        Else If( value < repclc ) Then
           zeffem(jk,jl)    = repclc
        Else
           zeffem(jk,jl)    = value
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
  zclear(1,:) = 1._JPRB
  zcloud(:,:) = 0._JPRB
  zcont(1,:)  = zeffem(jk1,:)

  zsum(1,:)   = zcont(1,:)
  zclm(jk1+1,:) = zsum(1,:)

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = jk1 , nlevels       ! layer loop
        Do jl = 1,nchannels
           If( zeffem(jk,jl) > zcloud(jk,jl) ) Then
              znum(jk,jl) = zclear(jk,jl) * (unity-zeffem(jk,jl))
           Else
              znum(jk,jl) = zclear(jk,jl) * (unity-zcloud(jk,jl))
           End If
           If ( zcloud(jk,jl) > unity-repclc ) Then
              zden(jk,jl) = repclc
           Else
              zden(jk,jl) = unity - zcloud(jk,jl)
           Endif

           zclear(jk+1,jl)  = znum(jk,jl) / zden(jk,jl)
           zclm(jk+1,jl)    = 1._JPRB - zclear(jk+1,jl)
           zcloud(jk+1,jl)  = zeffem(jk,jl)
        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = jk1 + 1 , nlevels  ! layer loop
        Do jl = 1,nchannels
           zsum(jk,jl)  = zsum(jk-1,jl)
           zcont(jk,jl) = zcont(jk-1,jl)
           ztr1(jk,jl)  = zcont(jk,jl) / zeffem(jk-1,jl)

           znum(jk,jl)    = 1._JPRB - (zsum(jk,jl)-zcont(jk,jl)) -&
                  & ztr1(jk,jl)*zcldfr(jk-1,jl)
           zden(jk,jl)    = (1._JPRB-zcldfr(jk-1,jl))
           ztr2(jk,jl)    = znum(jk,jl) / zden(jk,jl)

           If( zcldfr(jk,jl) > zcldfr(jk-1,jl) ) Then
              zcadj(jk,jl) = zcldfr(jk-1,jl)
           Else
              zcadj(jk,jl) = zcldfr(jk,jl)
           End If

           zcont(jk,jl) = zcldem(jk,jl) * (zcadj(jk,jl) *&
                 & (1._JPRB-zcldem(jk-1,jl))*ztr1(jk,jl)  +&
                 & (zcldfr(jk,jl)-zcadj(jk,jl)) * ztr2(jk,jl))
           zsum(jk,jl)   = zsum(jk,jl)    + zcont(jk,jl)
           zclm(jk+1,jl) = zsum(jk,jl)
        Enddo
     Enddo
  Endif

  ! Weight computation
  !

  ! Contribution from clear-sky fraction
  radiance    % cs_wtoa(:) = 1._JPRB - zclm(nlevels+1,:)


  ! Contribution from cloudy layers
  Do jk = jk1, nlevels  ! layer loop
     radiance    % wtoa( jk, : ) = zclm(jk+1,:)    - zclm(jk,:)
  End Do




  !----------------------------------------
  !  --- Adjoint computation for top of the atmosphere
  !----------------------------------------

  zclm_ad(:,:) = 0._JPRB
  zclear_ad(:) = 0._JPRB
  zcloud_ad(:) = 0._JPRB
  zcont_ad(:)  = 0._JPRB
  zsum_ad(:)   = 0

  ! Weight computation
  !
  ! Contribution from cloudy layers
  Do jk = nlevels, jk1, -1  ! layer loop
     zclm_ad(jk+1,:) = zclm_ad(jk+1,:) + radiance_ad % wtoa( jk,:)
     zclm_ad(jk,:)   = zclm_ad(jk,:)   - radiance_ad % wtoa( jk,:)
     radiance_ad % wtoa( jk, : ) = 0._JPRB
  End Do

  ! Contribution from clear-sky fraction
  zclm_ad(nlevels+1,:) = zclm_ad(nlevels+1,:) - radiance_ad % cs_wtoa(:)
  radiance_ad % cs_wtoa(:) =   0._JPRB


  !----------------------------------------
  !
  ! Effect of cloudiness on toa radiances
  !
  ! Cloud cover matrix
  !
  ! zclm(jk2) is the obscuration factor by cloud layers between
  ! half-levels jk1 and jk2 as seen from jk1
  ! jk1 is the top of the atmosphere (toa)

  jk1 = 1 ! level of top of the atmosphere

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = nlevels, jk1, -1      ! layer loop
        Do jl = 1,nchannels
           zeffem_ad(jk,jl) = zeffem_ad(jk,jl) + zcloud_ad(jl)
           zcloud_ad(jl)    = 0._JPRB

           zclear_ad(jl)    = zclear_ad(jl) - zclm_ad(jk+1,jl)
           zclm_ad(jk+1,jl) = 0._JPRB

           znum_ad = zclear_ad(jl) / zden(jk,jl)
           zden_ad =-zclear_ad(jl) * znum(jk,jl) / (zden(jk,jl)**2)

           If ( zcloud(jk,jl) > unity-repclc ) Then
              zden_ad = 0._JPRB
           Else
              zcloud_ad(jl) = zcloud_ad(jl) - zden_ad
           Endif

           If( zeffem(jk,jl) > zcloud(jk,jl) ) Then
              zclear_ad(jl) = zclear_ad(jl) + znum_ad *&
                    & (unity-zeffem(jk,jl))
              zeffem_ad(jk,jl) = zeffem_ad(jk,jl) - znum_ad *&
                    & zclear(jk,jl)
           Else
              zclear_ad(jl) = zclear_ad(jl) + znum_ad *&
                    & (unity-zcloud(jk,jl))
              zcloud_ad(jl) = zcloud_ad(jl) + znum_ad *&
                    & zclear(jk,jl)
           End If
        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = nlevels, jk1+1, -1  ! layer loop
        Do jl = 1,nchannels
           znum_ad = 0._JPRB
           zden_ad = 0._JPRB
           ztr1_ad = 0._JPRB
           ztr2_ad = 0._JPRB
           zcadj_ad = 0._JPRB

           zsum_ad(jl)      = zsum_ad(jl) +  zclm_ad(jk+1,jl)
           zclm_ad(jk+1,jl) = 0._JPRB

           zcont_ad(jl)      = zcont_ad(jl) +  zsum_ad(jl)


           zcldem_ad(jk,jl) = zcldem_ad(jk,jl) + zcont_ad(jl) *&
                 & (zcadj(jk,jl)*(1._JPRB-zcldem(jk-1,jl))*ztr1(jk,jl)  + &
                          & (zcldfr(jk,jl)-zcadj(jk,jl))*ztr2(jk,jl))
           zcadj_ad = zcadj_ad + zcont_ad(jl) *&
                 & zcldem(jk,jl) * ( (1._JPRB-zcldem(jk-1,jl))*ztr1(jk,jl) - ztr2(jk,jl)  )
           zcldem_ad(jk-1,jl) = zcldem_ad(jk-1,jl) - zcont_ad(jl) *&
                  & zcldem(jk,jl) * zcadj(jk,jl) * ztr1(jk,jl)

           ztr1_ad = ztr1_ad +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * zcadj(jk,jl)  * (1._JPRB-zcldem(jk-1,jl))
           zcldfr_ad(jk,jl) = zcldfr_ad(jk,jl)  +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * ztr2(jk,jl)
           ztr2_ad = ztr2_ad +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * (zcldfr(jk,jl) -zcadj(jk,jl) )
           zcont_ad(jl) = 0._JPRB

           If( zcldfr(jk,jl) < zcldfr(jk-1,jl) ) Then
              zcldfr_ad(jk,jl)   = zcldfr_ad(jk,jl)   + zcadj_ad
           Else
              zcldfr_ad(jk-1,jl) = zcldfr_ad(jk-1,jl) + zcadj_ad
           End If

           znum_ad = znum_ad + ztr2_ad / zden(jk,jl)
           zden_ad = zden_ad - ztr2_ad * znum(jk,jl) / zden(jk,jl)**2

           zcldfr_ad(jk-1,jl) = zcldfr_ad(jk-1,jl) - zden_ad

           zsum_ad(jl)        = zsum_ad(jl)        - znum_ad
           zcont_ad(jl)       = zcont_ad(jl)       + znum_ad
           ztr1_ad            = ztr1_ad            - znum_ad * zcldfr(jk-1,jl)
           zcldfr_ad(jk-1,jl) = zcldfr_ad(jk-1,jl) - znum_ad * ztr1(jk,jl)


           zcont_ad(jl)       = zcont_ad(jl)       + ztr1_ad / zeffem(jk-1,jl)
           zeffem_ad(jk-1,jl) = zeffem_ad(jk-1,jl) - ztr1_ad * &
                 & zcont(jk-1,jl) / (zeffem(jk-1,jl)**2)

        Enddo
     Enddo
  Endif

  ! Cloud cover matrix
  !
  zsum_ad(:) = zsum_ad(:) + zclm_ad(jk1+1,:)
  zclm_ad(jk1+1,:) = 0._JPRB
  zcont_ad(:) = zcont_ad(:) + zsum_ad(:)
  zsum_ad(:)   = 0._JPRB
  zeffem_ad(jk1,:) = zeffem_ad(jk1,:) + zcont_ad(:)
  zcont_ad(:)  = 0._JPRB

!!  zclm_ad(:,:) = 0.
!!  zclear_ad(:) = 0.
!!  zcloud_ad(:) = 0.

  !----------------------------------------
  !  --- End of Adjoint computation for top of the atmosphere
  !----------------------------------------



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
  jk1 = nlevels   ! surface level

  zclm(:,:)   = 0._JPRB
  zcloud(:,:) = 0._JPRB
  zclear(:,:) = 1._JPRB
  zcont(nlevels,:) = zeffem(nlevels,:)
  zsum(nlevels,:)  = zcont(nlevels,:)
  zclm(nlevels,:)  = zsum(nlevels,:)

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = nlevels, 1, -1       ! layer loop
        Do jl = 1,nchannels
           If( zeffem(jk,jl) > zcloud(jk,jl) ) Then
              znum(jk,jl) = zclear(jk,jl)*(1._JPRB-zeffem(jk,jl))
           Else
              znum(jk,jl) = zclear(jk,jl)*(1._JPRB-zcloud(jk,jl))
           End If

           If ( zcloud(jk,jl) > unity-repclc ) Then
              zden(jk,jl) = repclc
           Else
              zden(jk,jl) = 1._JPRB - zcloud(jk,jl)
           End If
           zclear(jk-1,jl) = znum(jk,jl) / zden(jk,jl)
           zclm(jk,jl)     = 1._JPRB - zclear(jk-1,jl)
           zcloud(jk-1,jl) = zeffem(jk,jl)
        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = nlevels - 1, 1, -1  ! layer loop
        Do jl = 1,nchannels
           zcont(jk,jl) = zcont(jk+1,jl)
           zsum(jk,jl)  = zsum(jk+1,jl)
           ztr1(jk,jl)  = zcont(jk,jl) / zeffem(jk+1,jl)

           znum(jk,jl)  = 1._JPRB-(zsum(jk,jl)-zcont(jk,jl))-ztr1(jk,jl)*zcldfr(jk+1,jl)
           zden(jk,jl)  = (1._JPRB-zcldfr(jk+1,jl))
           ztr2(jk,jl)  = znum(jk,jl) / zden(jk,jl)

           If( zcldfr(jk,jl) > zcldfr(jk+1,jl) ) Then
              zcadj(jk,jl) = zcldfr(jk+1,jl)
           Else
              zcadj(jk,jl) = zcldfr(jk,jl)
           End If

           zcont(jk,jl) = zcldem(jk,jl)*(zcadj(jk,jl)*(1._JPRB-zcldem(jk+1,jl))*ztr1(jk,jl)  &
                & +(zcldfr(jk,jl)-zcadj(jk,jl))*ztr2(jk,jl))
           zsum(jk,jl) = zsum(jk,jl) + zcont(jk,jl)
           zclm(jk,jl) = zsum(jk,jl)

        End Do
     End Do
  End If

  ! Weight computation
  !
  ! Contribution from clear-sky fraction

  radiance    % cs_wsurf(:) = 1._JPRB - zclm(1,:)

  ! Contribution from cloudy layers

  Do jk = 1, nlevels  ! layer loop
     radiance    % wsurf( jk, : ) = zclm(jk,:)    - zclm(jk+1,:)
  Enddo


!-----------------------------------------------
!  --- Adjoint computation for surface radiances
!-----------------------------------------------

  zclm_ad(:,:) = 0._JPRB
  zclear_ad(:) = 0._JPRB
  zcloud_ad(:) = 0._JPRB
  zcont_ad(:)  = 0._JPRB
  zsum_ad(:)   = 0._JPRB

  ! Weight computation
  !
  ! Contribution from cloudy layers
  Do jk = nlevels, 1, -1  ! layer loop
     zclm_ad(jk,:)   = zclm_ad(jk,:)   + radiance_ad % wsurf( jk, : )
     zclm_ad(jk+1,:) = zclm_ad(jk+1,:) - radiance_ad % wsurf( jk, : )
     radiance_ad % wsurf( jk, : ) = 0._JPRB
  Enddo

  ! Contribution from clear-sky fraction
  zclm_ad(1,:) = zclm_ad(1,:) - radiance_ad % cs_wsurf(:)
  radiance_ad % cs_wsurf(:) = 0._JPRB


  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = 1, nlevels       ! layer loop
        Do jl = 1,nchannels
           znum_ad = 0._JPRB
           zden_ad = 0._JPRB

           zeffem_ad(jk,jl) = zeffem_ad(jk,jl) + zcloud_ad(jl)
           zcloud_ad(jl)    = 0._JPRB

           zclear_ad(jl)  = zclear_ad(jl) - zclm_ad(jk,jl)
           zclm_ad(jk,jl) = 0._JPRB

           znum_ad = znum_ad + zclear_ad(jl) * zclear(jk,jl)/zden(jk,jl)
           zden_ad = zden_ad - zclear_ad(jl) *&
                 & zclear(jk,jl) * znum(jk,jl) / (zden(jk,jl)**2)
           zclear_ad(jl) = zclear_ad(jl) * znum(jk,jl) / zden(jk,jl)

           If ( zcloud(jk,jl) < unity-repclc ) Then
              zcloud_ad(jl) = zcloud_ad(jl) - zden_ad
           End If

           If( zeffem(jk,jl) > zcloud(jk,jl) ) Then
              zeffem_ad(jk,jl) = zeffem_ad(jk,jl) - znum_ad
           Else
              zcloud_ad(jl)    = zcloud_ad(jl)    - znum_ad
           End If

        End Do
     End Do

  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = 1, nlevels - 1  ! layer loop
        Do jl = 1,nchannels

           znum_ad = 0._JPRB
           zden_ad = 0._JPRB
           ztr1_ad = 0._JPRB
           ztr2_ad = 0._JPRB
           zcadj_ad = 0._JPRB

           zsum_ad(jl)    = zsum_ad(jl) +  zclm_ad(jk,jl)
           zclm_ad(jk,jl) = 0._JPRB

           zcont_ad(jl)    = zcont_ad(jl) +  zsum_ad(jl)

           zcldem_ad(jk,jl) = zcldem_ad(jk,jl) + zcont_ad(jl) *&
                 & (zcadj(jk,jl)*(1._JPRB-zcldem(jk+1,jl))*ztr1(jk,jl)  + &
                          & (zcldfr(jk,jl)-zcadj(jk,jl))*ztr2(jk,jl))
           zcadj_ad = zcadj_ad + zcont_ad(jl) *&
                 & zcldem(jk,jl) * ( (1._JPRB-zcldem(jk+1,jl))*ztr1(jk,jl) - ztr2(jk,jl)  )
           zcldem_ad(jk+1,jl) = zcldem_ad(jk+1,jl) - zcont_ad(jl) *&
                  & zcldem(jk,jl) * zcadj(jk,jl) * ztr1(jk,jl)
           ztr1_ad = ztr1_ad +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * zcadj(jk,jl)  * (1._JPRB-zcldem(jk+1,jl))
           zcldfr_ad(jk,jl) = zcldfr_ad(jk,jl)  +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * ztr2(jk,jl)
           ztr2_ad = ztr2_ad +  zcont_ad(jl) *&
                 & zcldem(jk,jl) * (zcldfr(jk,jl) -zcadj(jk,jl) )
           zcont_ad(jl) = 0._JPRB

           If( zcldfr(jk,jl) < zcldfr(jk+1,jl) ) Then
              zcldfr_ad(jk,jl)   = zcldfr_ad(jk,jl)   + zcadj_ad
           Else
              zcldfr_ad(jk+1,jl) = zcldfr_ad(jk+1,jl) + zcadj_ad
           End If

           znum_ad = znum_ad + ztr2_ad / zden(jk,jl)
           zden_ad = zden_ad - ztr2_ad * znum(jk,jl) / zden(jk,jl)**2

           zcldfr_ad(jk+1,jl) = zcldfr_ad(jk+1,jl) - zden_ad

           zsum_ad(jl)        = zsum_ad(jl)        - znum_ad
           zcont_ad(jl)       = zcont_ad(jl)       + znum_ad
           ztr1_ad            = ztr1_ad            - znum_ad * zcldfr(jk+1,jl)
           zcldfr_ad(jk+1,jl) = zcldfr_ad(jk+1,jl) - znum_ad * ztr1(jk,jl)


           zcont_ad(jl)       = zcont_ad(jl)       + ztr1_ad / zeffem(jk+1,jl)
           zeffem_ad(jk+1,jl) = zeffem_ad(jk+1,jl) - ztr1_ad * &
                 & zcont(jk+1,jl) / (zeffem(jk+1,jl)**2)

       End Do
     End Do
  End If


! Cloud cover matrix
!
  zsum_ad(:) = zsum_ad(:) + zclm_ad(jk1,:)

  zcont_ad(:) = zcont_ad(:) + zsum_ad(:)

  zeffem_ad(jk1,:) = zeffem_ad(jk1,:) + zcont_ad(:)

  zclm_ad(jk1,:) = 0._JPRB
  zclear_ad(:) = 0._JPRB
  zcloud_ad(:) = 0._JPRB
  zcont_ad(:)  = 0._JPRB
  zsum_ad(:)   = 0._JPRB

!-----------------------------------------------
!  --- End of Adjoint computation for surface radiances
!-----------------------------------------------


!  --- Adjoint computation for initialisations

  Do jk = nlevels, 1, -1
     Do jl = 1, nchannels
        freq = polarisations(jl,2)
        idp = lprofiles(freq)

        value = test_3(jk,jl)
        If( value > (unity-repclc) ) Then
           zeffem_ad(jk,jl)    = 0._JPRB
        Else If( value < repclc ) Then
           zeffem_ad(jk,jl)    = 0._JPRB
        Else
           radiance_ad % cldemis(jk,jl) = radiance_ad % cldemis(jk,jl) +&
                 & zeffem_ad(jk,jl) * profiles(idp)%cc(jk)
           profiles_ad(idp)%cc(jk)      = profiles_ad(idp)%cc(jk)      +&
                 & zeffem_ad(jk,jl) * radiance % cldemis(jk,jl)
           zeffem_ad(jk,jl)    = 0._JPRB
        Endif

        value = test_2(jk,jl)
        If( value > (unity-repclc) ) Then
           zcldfr_ad(jk,jl)    = 0._JPRB
        Else If( value < repclc ) Then
           zcldfr_ad(jk,jl)    = 0._JPRB
        Else
           profiles_ad(idp)%cc(jk) = profiles_ad(idp)%cc(jk) + zcldfr_ad(jk,jl)
           zcldfr_ad(jk,jl)        = 0._JPRB
        Endif

        value = test_1(jk,jl)
        If( value > (unity-repclc) ) Then
           zcldem_ad(jk,jl)    = 0._JPRB
        Else If( value < repclc ) Then
           zcldem_ad(jk,jl)    = 0._JPRB
        Else
           radiance_ad % cldemis(jk,jl) = radiance_ad % cldemis(jk,jl) +&
                 & zcldem_ad(jk,jl)
           zcldem_ad(jk,jl)    = 0._JPRB
        Endif

     End Do
  End Do

End Subroutine Rttov_aitosu_ad
