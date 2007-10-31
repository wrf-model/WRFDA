!
Subroutine rttov_intext_prof( &
      & profile_in     ,  &! in
      & profile_out     )  ! inout
  ! Description:
  ! This routine is for use with the cloud test programs (main_testad and main_testk)
  !
  ! Interpolate and extrapolate an input profile to an output profile
  ! pressure levels
  ! Interpoaltion is spline (or log if rttov_intex is uncommented)
  ! Extrapolation below lowest input level :
  !    adiabatic heating, constant relative humidity, constant O3, CO2 profiles
  ! Extrapolation above highest input level
  !    linear extrapolation in T, Q, O3, CO2
  !
  ! Output pressures and number of levels should be initialised
  ! Cloud liquid water is NOT interpolated/extrapolated
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
  !  1.0      2002   original (F. Chevalier)
  !  1.1   01/2003   change rh calculation, add CO2 and
  !                  insert rttov_intex calls in comment (P Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & profile_Type

  Use parkind1, Only : jpim     ,jprb
  Implicit None

!#include "rttov_intex.interface"

  !subroutine arguments:
  Type(profile_Type), Intent(in)    :: profile_in
  Type(profile_Type), Intent(inout) :: profile_out



  !local variables:
  Real(Kind=jprb) :: presfin( profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: tfin(    profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: wvvmfin( profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: o3vmfin( profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: co2vmfin(profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: arx(     profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: ary(     profile_in%nlevels + profile_out%nlevels )
  Real(Kind=jprb) :: textr(   profile_out%nlevels )
  Real(Kind=jprb) :: aryint(  profile_out%nlevels )
  Real(Kind=jprb) :: as( 5,  profile_in%nlevels + profile_out%nlevels )

  Integer(Kind=jpim) :: klevm
  Integer(Kind=jpim) :: nlevels
  Integer(Kind=jpim) :: levbot
  Integer(Kind=jpim) :: levtop
  Integer(Kind=jpim) :: index
  Integer(Kind=jpim) :: i, j
  Integer(Kind=jpim) :: jlev
  Real(Kind=jprb)    :: es
  Real(Kind=jprb)    :: wvvs
  Real(Kind=jprb)    :: rh
  Real(Kind=jprb)    :: const
  Real(Kind=jprb)    :: psurf
  Real(Kind=jprb)    :: gradt, gradq, grado, gradco2

  !- End of header --------------------------------------------------------

  ! Profile_out should have pressure array and the
  ! number of levels already filled

  ! Q and O3 units are ppmv

  klevm   = profile_in  % nlevels
  nlevels = profile_out % nlevels
  psurf   = profile_in % s2m % p

  ! Beware NO interpolation of cloud liquid water profile
  ! If CLW array already associated with memory, then fill with 0.
  profile_out % clw_Data   = .False.
  If( Associated( profile_out % clw) ) Then
     profile_out % clw(:) = 0._JPRB
  End If
  profile_out % ozone_Data = profile_in % ozone_Data
  profile_out % co2_Data   = profile_in % co2_Data
  profile_out % zenangle   = profile_in % zenangle
  profile_out % ctp        = profile_in % ctp
  profile_out % cfraction  = profile_in % cfraction
  profile_out % s2m        = profile_in % s2m
  profile_out % skin       = profile_in % skin



  presfin(1:klevm) = profile_in % p(1:klevm)

  tfin(1:klevm)    = profile_in % t(1:klevm)
  wvvmfin(1:klevm) = profile_in % q(1:klevm)

  tfin(klevm+1)    = profile_in % s2m % t
  wvvmfin(klevm+1) = profile_in % s2m % q

  If( profile_out % ozone_Data ) Then
     o3vmfin(1:klevm) = profile_in % o3(1:klevm)
     o3vmfin(klevm+1) = profile_in % s2m % o
  End If

  If( profile_out % co2_Data ) Then
     co2vmfin(1:klevm) = profile_in % co2(1:klevm)
     co2vmfin(klevm+1) = profile_in % co2(klevm)
  End If

  levbot=0
  Do i=1,profile_out % nlevels
     If(profile_out % p(i) > profile_in % s2m % p ) Then
        levbot=i
        Exit
     Endif
  Enddo

  index = klevm
  If (levbot /= 0 ) Then
     !-----Extrapolates temperature below surface pressure-------------------
     !----- => adiabatic heating

     const= 287._JPRB/1005._JPRB
     Do i = levbot, nlevels
        textr(i)=profile_out % s2m % t * (profile_out % p(i)/psurf)**const
     Enddo

     index=nlevels-levbot+1
     index=index+klevm+1
     tfin((klevm+2):index)= textr(levbot:nlevels)

     presfin((klevm+1))       = psurf
     presfin((klevm+2):index) = profile_out % p(levbot:nlevels)

     !-----Extrapolates water vapour below surface pressure------------------
     !----- => constant relative humidity

     ! Saturated vapour pressure im mb
     es = svp(profile_in % s2m % t)

     ! volume mixing ratio for saturation (no unit)
     wvvs = es / psurf

     ! relative humidity at 2m
     rh = profile_out % s2m % q * 1e-06 / wvvs


     Do i = levbot, nlevels
        es   = svp(textr(i))
        wvvs = es / profile_out % p(i)
        j = klevm + 2 + i - levbot
        wvvmfin(j) = rh * wvvs * 1e+06
     Enddo

  End If
  !-----Extrapolates ozone below surface pressure------------------------
  !----- => constant profile

  If( profile_out % ozone_Data ) Then
     o3vmfin((klevm+2):index) = profile_in % o3(klevm)
  End If

  !-----Extrapolates CO2 below surface pressure------------------------
  !----- => constant profile
  If( profile_out % co2_Data ) Then
     co2vmfin((klevm+2):index) = profile_in % co2(klevm)
  End If

  !-----Extrapolates profile above highest declared level-----------------
  !----- => linear extrapolation -----------------------------------------

  levtop = 1
  Do jlev = 1, index
     If(profile_out % p(jlev) >= profile_in % p(1) ) Exit
     levtop = levtop + 1
  Enddo

  If (levtop /= 1) Then
     gradt = (tfin(1) - tfin(2)) / (presfin(1)-presfin(2))
     gradq = (wvvmfin(1) - wvvmfin(2)) / (presfin(1)-presfin(2))
     Do jlev=index, 1, -1
        tfin(jlev+levtop-1) = tfin(jlev)
        wvvmfin(jlev+levtop-1) = wvvmfin(jlev)
        presfin(jlev+levtop-1) = presfin(jlev)
     Enddo

     index = index + levtop-1
     Do jlev=1,levtop-1
        presfin(jlev) = profile_out % p(jlev)
        tfin(jlev) = tfin(levtop)  &
             & + gradt * (presfin(jlev) - presfin(levtop))
        wvvmfin(jlev) = wvvmfin(levtop)  &
             & + gradq * (presfin(jlev) - presfin(levtop))
     Enddo
  Endif

  If (levtop /= 1 .And. profile_out % ozone_Data) Then
     grado = (o3vmfin(1) - o3vmfin(2)) / (presfin(1)-presfin(2))
     Do jlev=index, 1, -1
        o3vmfin(jlev+levtop-1) = o3vmfin(jlev)
     Enddo

     Do jlev=1,levtop-1
        presfin(jlev) = profile_out % p(jlev)
        o3vmfin(jlev) = o3vmfin(levtop)  &
             & + grado * (presfin(jlev) - presfin(levtop))
     Enddo
  Endif

  If (levtop /= 1 .And. profile_out % co2_Data) Then
     gradco2 = (co2vmfin(1) - co2vmfin(2)) / (presfin(1)-presfin(2))
     Do jlev=index, 1, -1
        co2vmfin(jlev+levtop-1) = co2vmfin(jlev)
     Enddo

     Do jlev=1,levtop-1
        presfin(jlev) = profile_out % p(jlev)
        co2vmfin(jlev) = co2vmfin(levtop)  &
             & + gradco2 * (presfin(jlev) - presfin(levtop))
     Enddo
  Endif

  !-----Interpolates to given pressure grid-------------------------------

  arx(1:index)=presfin(1:index)

  ary(1:index)=tfin(1:index)

  Call spline(arx,ary,profile_out%p,aryint,as,index,nlevels,1)
  !call rttov_intex(index, nlevels, arx, profile_out%p, ary,aryint)
  profile_out%t(:)=aryint(:)

  ary(1:index)=wvvmfin(1:index)
  Call spline(arx,ary,profile_out%p,aryint,as,index,nlevels,1)
  !call rttov_intex(index, nlevels, arx, profile_out%p, ary,aryint)
  profile_out%q(:)=aryint(:)

  !-----Spline interpolation may exceptionnally give negative values------
  !-----for water vapour profiles-----------------------------------------
  !----- => correction
  Do i=1,nlevels
     If (profile_out%q(i) <= 0._JPRB ) profile_out%q(i) = 1.e-03_JPRB
  Enddo


  If( profile_out % ozone_Data ) Then
     ary(1:index)=o3vmfin(1:index)
     Call spline(arx,ary,profile_out%p,aryint,as,index,nlevels,1)
     !call rttov_intex(index, nlevels, arx, profile_out%p, ary,aryint)
     profile_out%o3(:)=aryint(:)
     Do i=1,nlevels
        If (profile_out%o3(i) <= 0._JPRB ) profile_out%o3(i) = 1.e-06_JPRB
     Enddo
  Endif


  If( profile_out % co2_Data ) Then
     ary(1:index)=co2vmfin(1:index)
     Call spline(arx,ary,profile_out%p,aryint,as,index,nlevels,1)
     !call rttov_intex(index, nlevels, arx, profile_out%p, ary,aryint)
     profile_out%co2(:)=aryint(:)
     Do i=1,nlevels
        If (profile_out%co2(i) <= 0._JPRB ) profile_out%co2(i) = 1.e-06_JPRB
     Enddo
  Endif


CONTAINS
!------------------------------------
!     Saturated vapour pressure im mb
     function svp(temp)
!
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
!     Description:
!     Calculates saturated vapour pressure in mb given temperature in K.
!     Value corresponds to SVP over water for temperatures grater than 5 DEG,
!     to SVP over ice for temperatures less that 8 DEG., and to transitional
!     values between.
!
!     Method:
!     This is the formula used by MET.O.11 AND MET.O.2B.
!
!     Owner:
!     Marco Matricardi
!
!     History:
!     Version      Date       Comment
!     1            16-8-1999  Marco Matricardi. ECMWF
!
!     Code description:
!       Language:              Fortran 90.
!       Software Standards:    "European Standards for Writing and Documenting
!                              Exchangeable Fortran 90 code".
!


  Use parkind1, Only : jpim     ,jprb
      implicit none

!     Function type:
  Real(Kind=jprb) :: svp

!     Function arguments:

!       Scalar arguments with intent in:
  Real(Kind=jprb),   intent(in)    ::  temp

!     End of function arguments




!       Local arrays:
  Real(Kind=jprb)        ::  estab(156)



!       Local scalars:
  Real(Kind=jprb)        ::  tt
  Real(Kind=jprb)        ::  e0
  Real(Kind=jprb)        ::  e1
  Integer(Kind=jpim)             ::  ind
  Integer(Kind=jpim)             ::  t0

!-----End of header-----------------------------------------------------

      data  estab(1:60) / &
     & 9.672e-5_JPRB,1.160e-4_JPRB,1.388e-4_JPRB,1.658e-4_JPRB,1.977e-4_JPRB,2.353e-4_JPRB, &
     & 2.796e-4_JPRB,3.316e-4_JPRB,3.925e-4_JPRB,4.638e-4_JPRB,5.472e-4_JPRB,6.444e-4_JPRB, &
     & 7.577e-4_JPRB,8.894e-4_JPRB,1.042e-3_JPRB,1.220e-3_JPRB,1.425e-3_JPRB,1.662e-3_JPRB, &
     & 1.936e-3_JPRB,2.252e-3_JPRB,2.615e-3_JPRB,3.032e-3_JPRB,3.511e-3_JPRB,4.060e-3_JPRB, &
     & 4.688e-3_JPRB,5.406e-3_JPRB,6.225e-3_JPRB,7.159e-3_JPRB,8.223e-3_JPRB,9.432e-3_JPRB, &
     & 1.080e-2_JPRB,1.236e-2_JPRB,1.413e-2_JPRB,1.612e-2_JPRB,1.838e-2_JPRB,2.092e-2_JPRB, &
     & 2.380e-2_JPRB,2.703e-2_JPRB,3.067e-2_JPRB,3.476e-2_JPRB,3.935e-2_JPRB,4.449e-2_JPRB, &
     & 5.026e-2_JPRB,5.671e-2_JPRB,6.393e-2_JPRB,7.198e-2_JPRB,8.097e-2_JPRB,9.098e-2_JPRB, &
     & 1.021e-1_JPRB,1.145e-1_JPRB,1.283e-1_JPRB,1.436e-1_JPRB,1.606e-1_JPRB,1.794e-1_JPRB, &
     & 2.002e-1_JPRB,2.233e-1_JPRB,2.488e-1_JPRB,2.769e-1_JPRB,3.079e-1_JPRB,3.421e-1_JPRB/

        data  estab(61:120) / &
     & 3.798e-1_JPRB,4.213e-1_JPRB,4.669e-1_JPRB,5.170e-1_JPRB,5.720e-1_JPRB,6.323e-1_JPRB, &
     & 6.985e-1_JPRB,7.709e-1_JPRB,8.502e-1_JPRB,9.370e-1_JPRB,   1.032_JPRB,   1.135_JPRB, &
        & 1.248_JPRB,   1.371_JPRB,   1.506_JPRB,   1.652_JPRB,   1.811_JPRB,   1.984_JPRB, &
        & 2.172_JPRB,   2.376_JPRB,   2.597_JPRB,   2.889_JPRB,   3.097_JPRB,   3.522_JPRB, &
       & 3.8619_JPRB,  4.2148_JPRB,  4.5451_JPRB,  4.8981_JPRB,  5.2753_JPRB,   5.678_JPRB, &
       & 6.1078_JPRB,  6.5662_JPRB,  7.0547_JPRB,  7.5753_JPRB,  8.1294_JPRB,  8.7192_JPRB, &
       & 9.3465_JPRB,  10.013_JPRB,  10.722_JPRB,  11.474_JPRB,  12.272_JPRB,  13.119_JPRB, &
       & 14.017_JPRB,  14.969_JPRB,  15.977_JPRB,  17.044_JPRB,  18.173_JPRB,  19.367_JPRB, &
       & 20.630_JPRB,  21.964_JPRB,  23.373_JPRB,  24.861_JPRB,  26.430_JPRB,  28.086_JPRB, &
       & 29.831_JPRB,  31.671_JPRB,  33.608_JPRB,  35.649_JPRB,  37.796_JPRB,  40.055_JPRB/

        data  estab(121:156) / &
       & 42.430_JPRB,  44.927_JPRB,  47.551_JPRB,  50.307_JPRB,  53.200_JPRB,  56.236_JPRB, &
       & 59.422_JPRB,  62.762_JPRB,  66.264_JPRB,  69.934_JPRB,  73.777_JPRB,  77.803_JPRB, &
       & 82.015_JPRB,  86.423_JPRB,  91.034_JPRB,  95.855_JPRB,  100.89_JPRB,  106.16_JPRB, &
       & 111.66_JPRB,  117.40_JPRB,  123.40_JPRB,  129.65_JPRB,  136.17_JPRB,  142.98_JPRB, &
       & 150.07_JPRB,  157.46_JPRB,  165.16_JPRB,  173.18_JPRB,  181.53_JPRB,  190.22_JPRB, &
       & 199.26_JPRB,  208.67_JPRB,  218.45_JPRB,  228.61_JPRB,  239.18_JPRB,  250.16_JPRB/


      tt=temp-183.15_JPRB             ! 183.15_JPRB = 273.15_JPRB - 90
        if (tt.le.0._JPRB) then
          svp=estab(1)
        else
          ind=int(tt)+1
          ind=min(ind,155)
          t0=ind-1
          e0=estab(ind)
          e1=estab(ind+1)
          svp=e0+(tt-t0)*(e1-e0)
        endif

      end function svp
!
!
      subroutine spline(xi,yi,xo,yo,as,ni,no,ii)

!     SPLINE, VERSION OF 17 JUL 81
  Use parkind1, Only : jpim     ,jprb
      implicit none
  Integer(Kind=jpim)                      :: ni, no, ii
  Real(Kind=jprb)                 :: xi(ni)
  Real(Kind=jprb)                 :: yi(ni)
  Real(Kind=jprb)                 :: xo(no)
  Real(Kind=jprb)                 :: yo(no)
  Real(Kind=jprb)                 :: as(5,ni)

  Integer(Kind=jpim)                      :: nim1, i, j, k, l, m
  Real(Kind=jprb)                 :: xm, xn, xx
  Real(Kind=jprb)                 :: c(4)

      if(ii.eq.1) call cubist(ni,xi,yi,as)
      nim1=ni-1
      m=1
      do 150 j=1,no
      xx=xo(j)
      do 100 i=m,nim1
      l=i
      xm=xi(i)
      xn=xi(i+1)
      if(xx.eq.xm) go to 120
      if(xx.eq.xn) go to 110
      if(xx.gt.xm.and.xx.lt.xn) go to 130
  100 continue
  110 continue
      l=l+1
  120 continue
      yo(j)=yi(l)
      m=l
      go to 150
  130 continue
      do  k=1,4
        c(k)=as(k,l)
      enddo
      yo(j)=c(1)+xx*(c(2)+xx*(c(3)+xx*c(4)))
      m=l
  150 continue
      end subroutine spline

      subroutine cubist(n,x,y,as)

!     CUBIST, VERSION OF 17 JUL 81
!     CALLED BY 'SPLINE'
!     CUBIC SPLINE GENERATOR BY W.HIBBARD, MOD'D BY H.WOOLF.  SECOND DERIV.
!     NOT CONTINUOUS. THE DEGREES OF FREEDOM GAINED ARE USED IN AN
!     ATTEMPT TO AVOID  OSCILLATIONS.
!     FIT TO THE POINTS (X(I),Y(I)) I=1,...,N .

  Use parkind1, Only : jpim     ,jprb
      implicit none
  Integer(Kind=jpim)                      :: n
  Real(Kind=jprb)                 :: x(n),y(n),as(5,n)

  Integer(Kind=jpim)                      :: m,i
  Real(Kind=jprb)                 :: t, w, z, s, u, v
  Real(Kind=jprb)                 :: zs, zq, ws, wq
  Real(Kind=jprb)                 :: aa, ba, ca, da

      m=n-1
      do   i=1,m
      if(i .eq. 1) go to 110
      t=(y(i+1)-y(i-1))/(x(i+1)-x(i-1))
      go to 120
  110 continue
      w=(y(2)+y(3))/2.0_JPRB
      z=(x(2)+x(3))/2.0_JPRB
      t=(w-y(1))/(z-x(1))
      t=2.0_JPRB*(y(2)-y(1))/(x(2)-x(1))-t
  120 continue
      if(i .eq. m) go to 130
      s=(y(i+2)-y(i))/(x(i+2)-x(i))
      go to 140
  130 continue
      w=(y(n-1)+y(n-2))/2.0_JPRB
      z=(x(n-1)+x(n-2))/2.0_JPRB
      s=(y(n)-w)/(x(n)-z)
      s=2.0_JPRB*(y(n)-y(n-1))/(x(n)-x(n-1))-s
  140 continue
      u=y(i+1)
      v=y(i)
      w=(x(i+1)+x(i))/2.0_JPRB
      z=(x(i+1)-x(i))/2.0_JPRB
      zs=z*z
      zq=z*zs
      ws=w*w
      wq=w*ws
      aa=.5*(u+v)-.25*z*(s-t)
      ba=.75*(u-v)/z-.25*(s+t)
      ca=.25*(s-t)/z
      da=.25*(s+t)/zs-.25*(u-v)/zq
      as(1,i)=aa-ba*w+ca*ws-da*wq
      as(2,i)=ba-2.0_JPRB*ca*w+3.0_JPRB*da*ws
      as(3,i)=ca-3.0_JPRB*da*w
      as(4,i)=da
      as(5,i)=0._JPRB
      enddo
      end subroutine cubist


End Subroutine rttov_intext_prof
