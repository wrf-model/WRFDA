!
Subroutine rttov_aitosu (  &
      & nfrequencies,      &! in
      & nchannels,         &! in
      & nprofiles,         &! in
      & nlevels,           &! in
      & polarisations,     &! in
      & lprofiles,         &! in
      & overlap_scheme,    &! in
      & profiles,          &! in  (cloud cover)
      & radiance)           ! inout  (cldemis input and
                           ! cs_wtao, cs_wsurf, wtao, wsurf in output)
  ! Description:
  ! To compute the weights of the black-body-derived radiances
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
  ! 1         03/2000     Original code (F. Chevallier)
  ! 2         03/2001     Fortran 90    (F. Chevallier)
  ! 2.1       12/2002   New F90 code with structures (P Brunel A  Smith)
  ! 3         24/2/04   Added polarimetry option
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



  ! Local parameters:
  !
  Real(Kind=jprb)   , Parameter :: repclc = 1.e-12_JPRB
  Real(Kind=jprb)   , Parameter :: unity = 1.0_JPRB
  !


  ! Local scalars:
  !
  Integer(Kind=jpim) :: jk, jk1, jl, idp
  Integer(Kind=jpim) :: freq
  Real(Kind=jprb)    :: zcadj, ztr1, ztr2
  !


  ! Local arrays:
  !
  Real(Kind=jprb), Dimension(nchannels) :: zclear, zcloud, zcont, zsum

  ! beware array shape zclm and zxxx is nchannels, nlevelsm+1
  Real(Kind=jprb)          :: zclm  (nlevels+1, nchannels)
  Real(Kind=jprb)          :: zcldfr(nlevels, nchannels)
  Real(Kind=jprb)          :: zcldem(nlevels, nchannels)
  Real(Kind=jprb)          :: zeffem(nlevels, nchannels)

  !- End of header --------------------------------------------------------

  !All input NWP profiles have the same number of levels

  radiance % cs_wtoa(:)  = 0._JPRB
  radiance % cs_wsurf(:) = 0._JPRB
  radiance % wtoa(:,:)   = 0._JPRB
  radiance % wsurf(:,:)  = 0._JPRB


  Do jk = 1, nlevels
     Do jl = 1, nchannels
        freq = polarisations(jl,2)
        idp = lprofiles(freq)
        zcldem(jk,jl)= Min( Max( radiance % cldemis(jk,jl),repclc ) ,unity-repclc )
        zcldfr(jk,jl)= Min( Max( profiles(idp)%cc(jk),repclc ) ,unity-repclc )
        zeffem(jk,jl)= radiance % cldemis(jk,jl) * profiles(idp)%cc(jk)
        zeffem(jk,jl)= Min( Max( zeffem(jk,jl),repclc) ,unity-repclc )
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
  zsum(:)   = zcont(:)
  zclm(jk1+1,:) = zsum(:)

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = jk1 , nlevels       ! layer loop
        Do jl = 1,nchannels
           zclear(jl) = zclear(jl)*(unity-Max(zeffem(jk,jl),zcloud(jl))) &
                & /(unity-Min(zcloud(jl),unity-repclc))
           zclm(jk+1,jl) = 1._JPRB - zclear(jl)
           zcloud(jl) = zeffem(jk,jl)
        End Do
     End Do
  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = jk1 + 1 , nlevels  ! layer loop
        Do jl = 1,nchannels
           ztr1 = zcont(jl)/zeffem(jk-1,jl)
           ztr2 = (1._JPRB-(zsum(jl)-zcont(jl))-ztr1*zcldfr(jk-1,jl)) &
                & /(1._JPRB-zcldfr(jk-1,jl))
           zcadj = Min(zcldfr(jk,jl),zcldfr(jk-1,jl))
           zcont(jl) = zcldem(jk,jl)*(zcadj*(1._JPRB-zcldem(jk-1,jl))*ztr1  &
                & +(zcldfr(jk,jl)-zcadj)*ztr2)
           zsum(jl) = zsum(jl) + zcont(jl)
           zclm(jk+1,jl) = zsum(jl)
        Enddo
     Enddo
  Endif

  ! Weight computation
  !

  ! Contribution from clear-sky fraction
  radiance % cs_wtoa(:) = 1._JPRB - zclm(nlevels+1,:)

  ! Contribution from cloudy layers
  Do jk = jk1, nlevels  ! layer loop
     radiance % wtoa( jk, : ) = zclm(jk+1,:) - zclm(jk,:)
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

  If (overlap_scheme == 1) Then
     !* maximum-random (Geleyn and Hollingsworth 1979)
     Do jk = nlevels, 1, -1       ! layer loop
        Do jl = 1,nchannels
           zclear(jl)=zclear(jl)*(1._JPRB-Max(zeffem(jk,jl),zcloud(jl))) &
                & /(1._JPRB-Min(zcloud(jl),unity-repclc))
           zclm(jk,jl) = 1._JPRB - zclear(jl)
           zcloud(jl) = zeffem(jk,jl)
        End Do
     End Do
  Else If (overlap_scheme == 2) Then
     !* maximum-random (Raisanen 1998)
     Do jk = nlevels - 1, 1, -1  ! layer loop
        Do jl = 1,nchannels
           ztr1 = zcont(jl)/zeffem(jk+1,jl)
           ztr2 = (1._JPRB-(zsum(jl)-zcont(jl))-ztr1*zcldfr(jk+1,jl)) &
                & /(1._JPRB-zcldfr(jk+1,jl))
           zcadj = Min(zcldfr(jk,jl),zcldfr(jk+1,jl))
           zcont(jl) = zcldem(jk,jl)*(zcadj*(1._JPRB-zcldem(jk+1,jl))*ztr1  &
                & +(zcldfr(jk,jl)-zcadj)*ztr2)
           zsum(jl) = zsum(jl) + zcont(jl)
           zclm(jk,jl) = zsum(jl)
        End Do
     End Do
  End If

  ! Weight computation
  !

  ! Contribution from clear-sky fraction

  radiance % cs_wsurf(:) = 1._JPRB - zclm(1,:)

  ! Contribution from cloudy layers

  Do jk = 1, nlevels  ! layer loop
     radiance % wsurf( jk, : ) = zclm(jk,:) - zclm(jk+1,:)
  Enddo

End Subroutine rttov_aitosu
