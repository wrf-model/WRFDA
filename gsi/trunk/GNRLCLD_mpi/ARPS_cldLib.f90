!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ARPS_cldLib 
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 
!
! ABSTRACT: 
!  This file include a collection of subroutines that are related to 
!              cloud analysis from ARPS cloud analysis 
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!
!
!   input argument list:
!
!   output argument list:
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!
!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_STABILITY               ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_stability (nz,t_1d,zs_1d,p_mb_1d,kbtm,ktop               &
           ,dte_dz_1d,thetae_1d)
!
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns stability at a given level given
!  1D temperature and pressure array inputs
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on LAPS cloud analysis code of 07/95
!
!  MODIFICATION HISTORY:
!
!  05/11/96  (J. Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: nz         ! number of vertical model levels
  REAL :: t_1d(nz)      ! temperature (degree Kelvin) profile
  REAL :: zs_1d(nz)     ! heights (m MSL) of each level
  REAL :: p_mb_1d(nz)   ! pressure (mb) at each level
  INTEGER :: kbtm,ktop  ! indices of the bottom and top cloud layer
!
!  OUTPUT:
  REAL :: dte_dz_1d(nz) ! stability array
!
!  LOCAL:
  REAL :: thetae_1d(nz) ! (equivalent) potential temperature.
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: k,km1,kp1,klow,khigh
  REAL :: os_fast
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Calculate Stability
!
!-----------------------------------------------------------------------
!
  klow  = MAX(kbtm-1,1)
  khigh = MIN(ktop+1,nz-1)

  DO k = klow,khigh
    thetae_1d(k)  = os_fast(t_1d(k), p_mb_1d(k))
  END DO ! k
  
  dte_dz_1d=0.

  DO k = kbtm,ktop
    km1  = MAX(k-1,1)
    kp1  = MIN(k+1,nz-1)

    IF( (zs_1d(kp1) - zs_1d(km1)) <= 0.) THEN
      write(6,*) 'GNRLCLD_mpi, get_stability: Error in get_stability '
      write(6,*) 'GNRLCLD_mpi, get_stability: k,kp1,km1 = ',k,kp1,km1
      write(6,*) 'GNRLCLD_mpi, get_stability: zs_1d(kp1),zs_1d(km1)= ',zs_1d(kp1),zs_1d(km1),        &
                 (zs_1d(kp1) - zs_1d(km1))
      call STOP2(114)
    ELSE 
      dte_dz_1d(k) = (thetae_1d(kp1) - thetae_1d(km1))                  &
                           / (zs_1d(kp1) - zs_1d(km1))
    END IF
  END DO ! k

  RETURN
END SUBROUTINE get_stability


!
!##################################################################
!##################################################################
!######                                                      ######
!######            FUNCTION OS_FAST                          ######
!######                                                      ######
!##################################################################
!##################################################################
!

  FUNCTION os_fast(tk,p)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMPERATURE OS
!  (K) FOR A PARCEL OF AIR SATURATED AT TEMPERATURE T (K)
!  AND PRESSURE P (MILLIBARS).
!
!
!-----------------------------------------------------------------------
!
!  AUTHOR: (BAKER,SCHLATTER)
!  05/17/1982
!
!
!  MODIFICATION HISTORY:
!  05/11/96 (Jian Zhang)
!  Modified for ADAS grid. Add document stuff.
!
!-----------------------------------------------------------------------
!
!  Variables declaration
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  REAL :: tk     ! temperature in kelvin
  REAL :: p      ! pressure in mb
!
!  OUTPUT:
  REAL :: os_fast  ! equivalent potential temperature
!
!  LOCAL:
  REAL :: b            ! empirical const. approx.= latent heat of
                     ! vaporiz'n for water devided by the specific
                     ! heat at const. pressure for dry air.
  DATA b/2.6518986/

  REAL :: tc,x,w
  REAL :: eslo
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  tc = tk - 273.15
!
!-----------------------------------------------------------------------
!
!  From W routine
!
!-----------------------------------------------------------------------
!
  x= eslo(tc)
  w= 622.*x/(p-x)

  os_fast= tk*((1000./p)**.286)*(EXP(b*w/tk))

  RETURN
  END FUNCTION os_fast



!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_CLOUDTYPE               ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_cloudtype(temp_k,dte_dz,cbase_m,ctop_m                   &
           ,itype,c2_type)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns cloud type at a given point given
!  temperature and stability inputs
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 05/1995
!
!  MODIFICATION HISTORY:
!
!  05/11/96  (J. Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  REAL :: temp_k       ! temperature
  REAL :: dte_dz       ! stability factor
  REAL :: cbase_m      ! height at cloud base level
  REAL :: ctop_m       ! height at cloud top level
!
!  OUTPUT:
  INTEGER :: itype     ! cloud type index
  CHARACTER (LEN=2) :: c2_type
!
!  LOCAL:
  CHARACTER (LEN=2) :: c2_cldtyps(10)

  DATA c2_cldtyps /'St','Sc','Cu','Ns','Ac'                             &
                  ,'As','Cs','Ci','Cc','Cb'/
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL :: depth_m,temp_c
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
  temp_c = temp_k - 273.15
  depth_m = ctop_m - cbase_m
!
!-----------------------------------------------------------------------
!
!  Go from Stability to Cloud Type
!
!-----------------------------------------------------------------------
!
  IF ( temp_c >= -10.) THEN
    IF (dte_dz >= +.001) THEN
      itype = 1      ! St
    ELSE IF (dte_dz < +.001 .AND. dte_dz >= -.001)  THEN
      itype = 2      ! Sc
    ELSE IF (dte_dz < -.001 .AND. dte_dz >= -.005)  THEN
      itype = 3      ! Cu
    ELSE ! dte_dz .lt. -.005
      IF(depth_m > 5000) THEN
        itype = 10   ! Cb
      ELSE  ! depth < 5km
        itype = 3    ! Cu
      END IF
    END IF

  ELSE IF (temp_c < -10. .AND. temp_c >= -20.) THEN

    IF (dte_dz < 0.) THEN
      IF(depth_m > 5000) THEN
        itype = 10   ! Cb
      ELSE
        itype = 5    ! Ac
      END IF
    ELSE
      itype = 6      ! As
    END IF

  ELSE               ! temp_c.lt.-20.

    IF (dte_dz >= +.0005) THEN
      itype = 7      ! Cs
    ELSE IF (dte_dz < +.0005 .AND. dte_dz >= -.0005) THEN
      itype = 8      ! Ci
    ELSE             ! dte_dz .lt. -.0005
      itype = 9      ! Cc
    END IF

    IF(depth_m > 5000 .AND. dte_dz < -.0000) THEN
      itype = 10     ! Cb
    END IF

  END IF

  c2_type = c2_cldtyps(itype)

  RETURN
END SUBROUTINE get_cloudtype

!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_SFM_1D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_sfm_1d (nz,zcb,zctop,zs_1d,p_mb_1d,t_1d,ql,qi,cldt,      &
                       l_prt)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!c-----------------------------------------------------------------
!c
!c    This is the streamlined version of the Smith-Feddes
!c    and Temperature Adjusted LWC calculation methodologies
!c    produced at Purdue University under sponsorship
!c    by the FAA Technical Center.
!c
!c    Currently, this subroutine will only use the Smith-
!c    Feddes and will only do so as if there are solely
!c    stratiform clouds present, however, it is very easy
!c    to switch so that only the Temperature Adjusted
!c    method is used.
!c
!c    Dilution by glaciation is also included, it is a
!c    linear function of in cloud temperature going from
!c    all liquid water at -10 C to all ice at -30 C
!c    as such the amount of ice is also calculated
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 07/1995
!
!  MODIFICATION HISTORY:
!
!  05/16/96 (Jian Zhang)
!           Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: nz             ! number of model vertical levels
  REAL :: zs_1d(nz)         ! physical height (m) at each scalar level
  REAL :: p_mb_1d(nz)       ! pressure (mb) at each level
  REAL :: t_1d(nz)          ! temperature (K) at each level

  REAL :: zcb               ! cloud base height (m)
  REAL :: zctop             ! cloud top height (m)
!
!  OUTPUT:
  REAL :: ql(nz)            ! liquid water content (g/kg)
  REAL :: qi(nz)            ! ice water content (g/kg)
  REAL :: cldt(nz)
!
!  LOCAL:
  REAL :: calw(200)
  REAL :: cali(200)
  REAL :: catk(200)
  REAL :: entr(200)
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  REAL :: dz,rv,rair,grav,cp,rlvo,rlso,dlvdt,eso
  REAL :: c,a1,b1,c1,a2,b2,c2
  REAL :: delz,delt,cldbtm,cldbp,cldtpt,tbar
  REAL :: arg,fraclw,tlwc
  REAL :: temp,press,zbase,alw,zht,ht,y
  REAL :: rl,es,qvs1,p,des,dtz,es2,qvs2
  INTEGER :: i,j,k,nlevel,nlm1,ip,kctop,kctop1,kcb,kcb1
  REAL :: dtdz,dttdz,zcloud,entc,tmpk
  LOGICAL :: l_prt
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Initialize 1d liquid water and ice arrays (for 100m layers)
!
!-----------------------------------------------------------------------
!
  DO i=1,200
    calw(i)=0.0
    cali(i)=0.0
  END DO
!      if(i_prt.le.20) then
!        i_prt=i_prt+1
!        l_prt=.true.
!      else
!        l_prt=.false.
!      endif
!
!-----------------------------------------------------------------------
!
!  Preset some constants and coefficients.
!
!-----------------------------------------------------------------------
!
  dz=100.0                ! m
  rv=461.5                ! J/deg/kg
  rair=287.04             ! J/deg/kg
  grav=9.81               ! m/s2
  cp=1004.                ! J/deg/kg
  rlvo=2.5003E+6          ! J/kg
  rlso=2.8339E+6          ! J/kg
  dlvdt=-2.3693E+3        ! J/kg/K
  eso=610.78              ! pa
  c=0.01
  a1=8.4897
  b1=-13.2191
  c1=4.7295
  a2=10.357
  b2=-28.2416
  c2=8.8846
!
!-----------------------------------------------------------------------
!
!  Calculate indices of cloud top and base
!
!-----------------------------------------------------------------------
!
  DO k=1,nz-1
    IF(zs_1d(k) < zcb .AND. zs_1d(k+1) > zcb) THEN
      kcb=k
      kcb1=kcb+1
    END IF
    IF(zs_1d(k) < zctop .AND. zs_1d(k+1) > zctop) THEN
      kctop=k
      kctop1=kctop+1
    END IF
  END DO
  IF(l_prt) THEN
    WRITE(6,*) ' get_sfm_1d: input at cloud base:'
    WRITE(6,600) zcb,kcb,zs_1d(kcb),t_1d(kcb)                           &
                 ,kcb1,zs_1d(kcb1),t_1d(kcb1)
    600     FORMAT(1X,' base=',f8.0,' kcb=',i2,' ht=',f8.0,' T=',f6.1,  &
                     ' kcb1=',i2,' ht=',f8.0,' T=',f6.1)
    WRITE(6,*) ' get_sfm_1d: input at cloud top:'
    WRITE(6,601) zctop,kctop,zs_1d(kctop),t_1d(kctop)                   &
                 ,kctop1,zs_1d(kctop1),t_1d(kctop1)
    601     FORMAT(1X,' top=',f8.0,' kctop=',i2,' ht=',f8.0,' T=',f6.1, &
                     ' kctop1=',i2,' ht=',f8.0,' T=',f6.1)
  END IF
!
!-----------------------------------------------------------------------
!
!  Obtain cloud base and top conditions
!
!-----------------------------------------------------------------------
!
  delz   = zs_1d(kcb+1)-zs_1d(kcb)
  delt   = t_1d(kcb+1)-t_1d(kcb)
  cldbtm = delt*(zcb-zs_1d(kcb))/delz+t_1d(kcb)
  tbar   = (cldbtm+t_1d(kcb))/2.
  arg    = -grav*(zcb-zs_1d(kcb))/rair/tbar
  cldbp  = p_mb_1d(kcb)*EXP(arg)
  delz   = zs_1d(kctop+1)-zs_1d(kctop)
  delt   = t_1d(kctop+1)-t_1d(kctop)
  cldtpt = delt*(zctop-zs_1d(kctop))/delz+t_1d(kctop)
!
!-----------------------------------------------------------------------
!
!  Calculate cloud lwc profile for cloud base/top pair
!
!-----------------------------------------------------------------------
!
  temp   = cldbtm
  press  = cldbp*100.0
  zbase  = zcb
  nlevel = ((zctop-zcb)/100.0)+1
  IF(nlevel <= 0) nlevel=1
  alw    = 0.0
  calw(1)= 0.0
  cali(1)= 0.0
  catk(1)= temp
  entr(1)= 1.0
  nlm1   = nlevel-1
  IF(nlm1 < 1) nlm1=1
  zht    = zbase

  DO j=1,nlm1
    rl   = rlvo+(273.15-temp)*dlvdt
    arg  = rl*(temp-273.15)/273.15/temp/rv
    es   = eso*EXP(arg)
    qvs1 = 0.622*es/(press-es)
!        rho1 = press/(rair*temp)
    arg  = -grav*dz/rair/temp
    p    = press*EXP(arg)

    IF(l_prt) THEN
      WRITE(6,605) j,zht,temp,press,1000.0*qvs1,es,rl
      605       FORMAT(1X,i2,' ht=',f8.0,' T=',f6.1,' P=',f9.1,' qvs=', &
                       f7.3,' es=',f6.1,' Lv=',e8.3)
    END IF
!
!-----------------------------------------------------------------------
!
!  Calculate saturated adiabatic lapse rate
!
!-----------------------------------------------------------------------
!
    des   = es*rl/temp/temp/rv
    dtz   = -grav*((1.0+0.621*es*rl/(press*rair*temp))/                 &
                 (cp+0.621*rl*des/press))
    zht   = zht+dz
    press = p
    temp  = temp+dtz*dz
    rl    = rlvo+(273.15-temp)*dlvdt
    arg   = rl*(temp-273.15)/273.15/temp/rv
    es2   = eso*EXP(arg)
    qvs2  = 0.622*es2/(press-es2)

    IF(l_prt) THEN
      WRITE(6,605) j+1,zht,temp,press,1000.0*qvs2,es2,rl
!605       format(1x,i2,' ht=',f8.0,' T=',f6.1,' P=',f9.1,' qvs=',
!  :           f7.3,' es=',f6.1,' Lv=',e8.3)
    END IF

!        rho2  = press/(rair*temp)
!        alw   = alw+(qvs1-qvs2)*(rho1+rho2)/2.   ! kg/m3

    alw   = alw+(qvs1-qvs2)                   ! kg/kg
    calw(j+1) = alw

    IF (l_prt) THEN
      WRITE(6,9015) j,1000.0*calw(j+1),zht
      9015      FORMAT(1X,'j=',i3,'  adiab.lwc =',f7.3,'  alt =',f8.0)
    END IF
!
!-----------------------------------------------------------------------
!
!  Reduction of lwc by entrainment
!
!-----------------------------------------------------------------------
!
    ht = (zht-zbase)*.001
!
!c   ------------------------------------------------------------------
!c
!c                          skatskii's curve(convective)
!c
!c   ------------------------------------------------------------------
!c      if(ht.lt.0.3) then
!c        y    = -1.667*(ht-0.6)
!c      elseif(ht.lt.1.0) then
!c        arg1 = b1*b1-4.0*a1*(c1-ht)
!c        y    = (-b1-sqrt(arg1))/(2.0*a1)
!c      elseif(ht.lt.2.9) then
!c        arg2 = b2*b2-4.0*a2*(c2-ht)
!c        y    = (-b2-sqrt(arg2))/(2.0*a2)
!c      else
!c        y    = 0.26
!c      endif
!c
!c   ------------------------------------------------------------------
!c
!c                         warner's curve(stratiform)
!c
!c   ------------------------------------------------------------------
    IF(ht < 0.032) THEN
      y = -11.0*ht+1.0           ! y(ht=0.032) = 0.648
    ELSE IF(ht <= 0.177) THEN
      y = -1.4*ht+0.6915         ! y(ht=0.177) = 0.4437
    ELSE IF(ht <= 0.726) THEN
      y = -0.356*ht+0.505        ! y(ht=0.726) = 0.2445
    ELSE IF(ht <= 1.5) THEN
      y = -0.0608*ht+0.2912      ! y(ht=1.5) = 0.2
    ELSE
      y = 0.20
    END IF
!
!-----------------------------------------------------------------------
!
!  Calculate reduced lwc by entrainment and dilution
!
!  Note at -5 C and warmer, all liquid.   ! changed from -10 KB
!       at -25 C and colder, all ice      ! changed from -30 KB
!       Linear ramp between.
!
!-----------------------------------------------------------------------
!
    IF(temp < 268.15) THEN
      IF(temp > 248.15) THEN
        fraclw=0.05*(temp-248.15)
      ELSE
        fraclw=0.0
      END IF
    ELSE
      fraclw=1.0
    END IF

    tlwc=1000.*y*calw(j+1)                ! g/kg
    calw(j+1)=tlwc*fraclw
    cali(j+1)=tlwc*(1.-fraclw)
    catk(j+1)=temp
    entr(j+1)=y

    IF(l_prt) THEN
      WRITE(6,*) ' Get_sfm_1d: entrainment dilution'
      WRITE(6,608) j+1,ht,1000.0*tlwc,calw(j+1),cali(j+1)
      608       FORMAT(1X,i2,' ht=',f8.3,' alw=',f8.3,' lwc=',f7.3,     &
                       ' ice=',f7.3)
    END IF

  END DO
!
!-----------------------------------------------------------------------
!
!  Alternative calculation procedure using the observed or
!  inferred in cloud temperature profile
!
!-----------------------------------------------------------------------
!
  IF(.true.) GO TO 455        ! forced goto, diseffect the following
  nlevel = (zctop-zcb)/100.0
  temp   = cldbtm
  press  = cldbp*100.0
  IF(nlevel <= 0) nlevel=0
  alw     = 0.0
  calw(1) = 0.0
  nlm1    = nlevel-1
  IF(nlm1 < 1) nlm1=1
  dtdz = (cldtpt-cldbtm)/(zctop-zcb)
  zht  = zbase
  DO j=1,nlm1
    rl   = rlvo+(temp-273.15)*dlvdt
    arg  = rl*(273.15-temp)/273.15/temp/rv
    es   = eso*EXP(arg)
    qvs1 = 0.622*es/(press-es)
!        rho1 = press/(rair*temp)
    arg  = -grav*dz/rair/temp
    p    = press*EXP(arg)
    des  = es*rl/temp/temp/rv
    dtz  = -grav*((1.0+0.621*es*rl/(press*rair*temp))/                  &
                  (cp+0.621*rl*des/press))
    IF(dtdz < dtz) THEN
      dttdz = dtz-(dtdz-dtz)
    ELSE
      dttdz = dtdz
    END IF
    zht   = zht+dz
    press = p
    temp  = temp+dttdz*dz
    rl    = rlvo+(273.15-temp)*dlvdt
    arg   = rl*(temp-273.15)/273.15/temp/rv
    es2   = eso*EXP(arg)
    qvs2  = 0.622*es2/(press-es2)
!        rho2  = press/(rair*temp)
!        alw   = alw+(qvs1-qvs2)*(rho1+rho2)/2.   ! kg/m3

    alw   = alw+(qvs1-qvs2)                   ! kg/kg
    IF(alw < 0.0) alw=0.0
!
!-----------------------------------------------------------------------
!
!  Application of a simple linear glaciation
!c   ---------------------------------------------------------------
!c
!c         all liquid T > -15 C
!c         partially liquid -15 C > T > -25 C
!c         all ice    T < -25 C
!c
!-----------------------------------------------------------------------
!
    IF(cldtpt < 258.15) THEN
      IF(cldtpt > 248.15) THEN
        fraclw = 0.1*(cldtpt-248.15)
      ELSE
        fraclw = 0.0
      END IF
    ELSE
      fraclw = 1.0
    END IF
    calw(j+1) = alw*fraclw*1000.0
    WRITE(6,9015) j,calw(j+1),zht
!9015   format(1x,'j=',i3,'  adiab.lwc =',f9.5,'  alt =',f8.0)
  END DO
  455   CONTINUE
!c
!-----------------------------------------------------------------------
!
!  Obtain profile of LWCs at the given grid point
!
!-----------------------------------------------------------------------
!
  DO ip=2,nz-1
    IF(zs_1d(ip) <= zcb .OR. zs_1d(ip) > zctop) THEN
      ql(ip)=0.0
      qi(ip)=0.0
      cldt(ip)=t_1d(ip)
    ELSE
      DO j=2,nlevel
        zcloud = zcb+(j-1)*dz
        IF(zcloud >= zs_1d(ip)) THEN
          ql(ip) = (zs_1d(ip)-zcloud+100.)*(calw(j)-calw(j-1))*0.01     &
                       +calw(j-1)
          qi(ip) = (zs_1d(ip)-zcloud+100.)*(cali(j)-cali(j-1))*0.01     &
                       +cali(j-1)
          tmpk = (zs_1d(ip)-zcloud+100.)*(catk(j)-catk(j-1))*0.01     &
                       +catk(j-1)
          entc = (zs_1d(ip)-zcloud+100.)*(entr(j)-entr(j-1))*0.01     &
                       +entr(j-1)
          cldt(ip) = (1.-entc)*t_1d(ip) + entc*tmpk

          IF(l_prt) THEN
            WRITE(6,*) ' Get_sfm_1d: assigning ql(ip),qi(ip)'
            WRITE(6,609) ip,zs_1d(ip),zcb,zctop
            609             FORMAT(1X,' ip=',i2,' ht=',f8.0,' zcb='     &
                                    ,f8.0,' zctop=',f8.0)
            WRITE(6,610) j,zcloud,j-1,zcloud-dz
            610             FORMAT(1X,' j=',i2,' z=',f8.0,' j-1=',i2,' z=',f8.0)
            WRITE(6,611) calw(j),calw(j-1),ql(ip)                       &
                       , cali(j),cali(j-1),qi(ip)
            611             FORMAT(1X,' lwc_j=',f7.3,' lwc_1=',f7.3,' ql=',f7.3, &
                                       ' ice_j=',f7.3,' ice_1=',f7.3,' qi=',f7.3)
          END IF
          EXIT
        END IF
      END DO
!      475      CONTINUE
    END IF
  END DO
!
!-----------------------------------------------------------------------
!
!  Write out file of lwc comparisons
!
!-----------------------------------------------------------------------
!
!  9001 FORMAT(i7)
!  9002 FORMAT(1X,3I2,1X,14F8.2,i2,i3)
!  9004 FORMAT(1X,2E15.8,'ihr, imin, isec=',3(i8,1X))
!  9005 FORMAT(2X,'Predicted LWC',8X,'Observed LWC',/)
!  9014 FORMAT(1X,8E15.8)
  RETURN
END SUBROUTINE get_sfm_1d


!
!
!##################################################################
!##################################################################
!######                                                      ######
!######              SUBROUTINE PCP_TYPE_3D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE pcp_type_3d (nx,ny,nz,temp_3d,rh_3d,p_pa_3d                  &
           ,radar_3d,l_mask,cldpcp_type_3d,istatus)

!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine returns 3D cloud and precipitation type field.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/1996  Based on the LAPS cloud analysis code developed by
!           Steve Albers.
!
!  This program modifies the most significant 4 bits of the integer
!  array by inserting multiples of 16.
!
!  MODIFICATION HISTORY:
!
!  05/16/96 (J. Zhang)
!           Modified for ADAS format. Added full documentation.
!  01/20/98 (J. Zhang)
!           Fixed a bug that no precip. type was assigned for a
!           grid point at the top of the radar echo with Tw
!           falling in the range of 0 to 1.3 degree C.
!  01/21/98 (J. Zhang)
!           Fixed a bug that does the freezing/refreezing test
!           on ice precipitates.
!  02/17/98 (J. Zhang)
!           Change the hail diagnose procedure.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single,i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: nx,ny,nz                  ! Model grid size
  REAL :: temp_3d(nx,ny,nz)            ! temperature (K)
  REAL :: rh_3d(nx,ny,nz)              ! relative humudity
  REAL :: p_pa_3d(nx,ny,nz)            ! pressure (Pascal)
  REAL(r_kind),intent(in) :: radar_3d(nx,ny,nz)           ! radar refl. (dBZ)
!
!  OUTPUT:
  INTEGER :: istatus
  INTEGER :: cldpcp_type_3d(nx,ny,nz)! cld/precip type
  INTEGER :: itype                   ! cld/precip type index
  LOGICAL :: l_mask(nx,ny)             ! "Potential" Precip Type
!
!  LOCAL functions:
  REAL :: dwpt                         ! for dew point calcl'n
  REAL :: wb_melting_thres             ! define melting temp. thresh.
  REAL :: tw                           ! for wet-bulb temp calcl'n
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: i,j,k,k_upper
  REAL :: t_c,td_c,t_wb_c,temp_lower_c,temp_upper_c,tbar_c              &
       ,p_mb,thickns,frac_below_zero
  INTEGER :: iprecip_type,iprecip_type_last,iflag_melt                  &
          ,iflag_refreez
  REAL :: zero_c,rlayer_refreez_max,rlayer_refreez
  INTEGER :: n_zr,n_sl,n_last
  REAL :: tmelt_c
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!-----------------------------------------------------------------------
!
  istatus=0
!
!-----------------------------------------------------------------------
!
!  Stuff precip type into cloud type array
!  0 - No Precip
!  1 - Rain
!  2 - Snow
!  3 - Freezing Rain
!  4 - Sleet
!  5 - Hail
!
!-----------------------------------------------------------------------
!
  zero_c = 273.15
  rlayer_refreez_max = 0.0

  n_zr = 0
  n_sl = 0
  n_last = 0

  DO j = 1,ny-1
    DO i = 1,nx-1

      iflag_melt = 0
      iflag_refreez = 0
      rlayer_refreez = 0.0

      iprecip_type_last = 0

      DO k = nz-1,1,-1

        IF(radar_3d(i,j,k) >= 0. .OR. l_mask(i,j)) THEN
!
!-----------------------------------------------------------------------
!
!  Set refreezing flag
!
!-----------------------------------------------------------------------
!
          t_c  = temp_3d(i,j,k) - zero_c
          td_c = dwpt(t_c,rh_3d(i,j,k))
          p_mb = 0.01*p_pa_3d(i,j,k)

          tmelt_c = wb_melting_thres(t_c,radar_3d(i,j,k))
          t_wb_c = tw(t_c,td_c,p_mb)

          IF(t_wb_c < 0.) THEN
            IF(iflag_melt == 1) THEN
!
!-----------------------------------------------------------------------
!
!  Integrate below freezing temperature times column thickness
!   - ONLY for portion of layer below freezing
!
!-----------------------------------------------------------------------
!
              temp_lower_c = t_wb_c
              k_upper = MIN(k+1,nz-1)
!
!-----------------------------------------------------------------------
!
!  For simplicity and efficiency, the assumption is here made that
!  the wet bulb depression is constant throughout the level.
!
!-----------------------------------------------------------------------
!
              temp_upper_c = t_wb_c + ( temp_3d(i,j,k_upper)            &
                                          - temp_3d(i,j,k))
              IF(temp_upper_c <= 0.) THEN
                frac_below_zero = 1.0
                tbar_c = 0.5 * (temp_lower_c + temp_upper_c)

              ELSE ! Layer straddles the freezing level
                frac_below_zero = temp_lower_c                          &
                                        / (temp_lower_c - temp_upper_c)
                tbar_c = 0.5 * temp_lower_c

              END IF

              thickns = p_pa_3d(i,j,k_upper) - p_pa_3d(i,j,k)
              rlayer_refreez = rlayer_refreez                           &
                   + ABS(tbar_c * thickns * frac_below_zero)

              IF(rlayer_refreez >= 25000.) THEN
                iflag_refreez = 1
              END IF

              rlayer_refreez_max =                                      &
                                MAX(rlayer_refreez_max,rlayer_refreez)

            END IF ! iflag_melt = 1

          ELSE ! Temp > 0C
            iflag_refreez = 0
            rlayer_refreez = 0.0

          END IF ! T < 0.0c, Temp is below freezing
!
!-----------------------------------------------------------------------
!
!  Set melting flag
!
!-----------------------------------------------------------------------
!
          IF(t_wb_c >= tmelt_c) THEN
            iflag_melt = 1
          END IF

          IF(t_wb_c >= tmelt_c) THEN  ! Melted to Rain
            iprecip_type = 1

          ELSE ! Check if below zero_c (Refrozen Precip or Snow)
            IF(t_wb_c < 0.0) THEN
              IF(iflag_melt == 1) THEN
                IF(iprecip_type_last == 1 .OR.iprecip_type_last == 3) THEN
                                   ! test if rain or zr freeze
                  IF(iflag_refreez == 0) THEN ! Freezing Rain
                    n_zr = n_zr + 1
                    IF(n_zr < 30) THEN
                      WRITE(6,5)i,j,k,t_wb_c,temp_3d(i,j,k)             &
                          ,rh_3d(i,j,k)
                      5                     FORMAT('zr',3I3,2F8.2,f8.1)
                    END IF
                    iprecip_type = 3

                  ELSE  ! (iflag_refreez = 1)  ! Sleet
                    n_sl = n_sl + 1
                    iprecip_type = 4
                  END IF ! iflag_refreez .eq. 0
                ELSE
                  iprecip_type = iprecip_type_last  ! Unchanged
                  n_last = n_last + 1
                  IF(n_last < 5) THEN
                    WRITE(6,*)'Unchanged Precip',i,j,k,t_wb_c
                  END IF
                END IF      ! liquid precip. at upper level?

              ELSE    ! iflag_melt =0        ! Snow
                iprecip_type = 2

              END IF   ! iflag_melt = 1?
            ELSE ! t_wb_c >= 0c, and t_wb_c < tmelt_c

              IF (iprecip_type_last == 0) THEN        !   1/20/98
                iprecip_type = 1    ! rain:at echo top and 0<Tw<1.3C
                iflag_melt = 1
              ELSE
                iprecip_type = iprecip_type_last
                n_last = n_last + 1
                IF(n_last < 5) THEN
                  WRITE(6,*)'Unchanged Precip',i,j,k,t_wb_c
                END IF
              END IF

            END IF ! t_wb_c < 0c
          END IF  ! t_wb_c >= tmelt_c

        ELSE ! radar_3d < 0dBZ;  No Radar Echo
          iprecip_type = 0
          iflag_melt = 0
          iflag_refreez = 0
          rlayer_refreez = 0.0

        END IF ! radar_3d(i,j,k).ge.0. .or. l_mask(i,j);  Radar Echo?
!
!-----------------------------------------------------------------------
!
!  Insert most sig 4 bits into array
!
!-----------------------------------------------------------------------
!
        itype = cldpcp_type_3d(i,j,k)
        itype = itype - (itype/16)*16     ! Initialize the 4 bits
        itype = itype + iprecip_type * 16 ! Add in the new value
        cldpcp_type_3d(i,j,k) = itype

        iprecip_type_last = iprecip_type

      END DO ! k
    END DO ! j
  END DO ! i

  DO j = 1,ny-1
    DO i = 1,nx-1
      DO k = 1,nz-1
        IF(radar_3d(i,j,k) >= 50.) THEN
          iprecip_type = 5
          itype = cldpcp_type_3d(i,j,k)
          itype = itype - (itype/16)*16     ! Initialize the 4 bits
          itype = itype + iprecip_type * 16 ! Add in the new value
          cldpcp_type_3d(i,j,k) = itype
        END IF
      END DO ! k
    END DO ! j
  END DO ! i

!  WRITE(6,*)' rlayer_refreez_max = ',rlayer_refreez_max
!  WRITE(6,*)' n_frz_rain/n_sleet = ',n_zr,n_sl
  istatus=1

  RETURN
END SUBROUTINE pcp_type_3d

!
!
!##################################################################
!##################################################################
!######                                                      ######
!######               SUBROUTINE GET_SLWC1D                  ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!

SUBROUTINE get_slwc1d (nk,cbase_m,ctop_m,kbase,ktop                     &
           ,zs_1d,t_1d,p_pa_1d,iflag_slwc,slwc_1d)

!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This routine calls a subroutine "lwc_rep" which calculates
!  the adiabatic liquid water content.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:  Jian Zhang
!  05/96    Based on the LAPS cloud analysis code of 07/1995
!
!  MODIFICATION HISTORY:
!
!  05/13/96  (Jian Zhang)
!            Modified for ADAS format. Added full documentation.
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!  INPUT:
  INTEGER :: iflag_slwc     ! indicator for LWC scheme option
  INTEGER :: nk             ! number of model vertical levels
  REAL :: t_1d(nk)          ! temperature (k) in one model column
  REAL :: zs_1d(nk)         ! heights (m) at grd pts in one model column
  REAL :: p_pa_1d(nk)       ! pressure (pa) in one model column
  REAL :: cbase_m,ctop_m    ! heights (m) of cloud base and top levels
  INTEGER :: kbase,ktop        ! vertical index of cloud base and top levels
!
!  OUTPUT:
  REAL :: slwc_1d(nk)       ! estimated adiabatic liquid water
!
!  LOCAL:
  INTEGER :: i_status1,i_status2 ! flag for subroutine calling
!
!-----------------------------------------------------------------------
!
!  Misc local variables
!
!-----------------------------------------------------------------------
!
  INTEGER :: k
  REAL :: p_low,p_high,cbase_pa,cbase_k,ctop_k,frac_k                   &
       ,grid_top_pa,grid_top_k
  REAL :: fraction,thickness,dlog_space
  REAL :: adiabatic_lwc,adjusted_lwc,adjusted_slwc
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!
!-----------------------------------------------------------------------
!
!  Initialize
!
!-----------------------------------------------------------------------
!
  DO k = 1,nk
    slwc_1d(k) = 0.0
  END DO

  IF(ctop_m > cbase_m) THEN
!
!-----------------------------------------------------------------------
!
!  Determine Lowest and Highest Grid Points within the cloud
!
!-----------------------------------------------------------------------
!
    IF(ktop >= kbase .AND. kbase >= 2) THEN
!
!-----------------------------------------------------------------------
!
!  Get cloud base pressure and temperature
!
!-----------------------------------------------------------------------
!
      cbase_pa = -999.         ! Default value is off the grid
      DO k = 1,nk-2
        IF(zs_1d(k+1) > cbase_m .AND. zs_1d(k) <= cbase_m) THEN
          thickness = zs_1d(k+1) - zs_1d(k)
          fraction = (cbase_m - zs_1d(k))/thickness
          p_low = p_pa_1d(k)
          p_high = p_pa_1d(k+1)
          dlog_space = LOG(p_high/p_low)
          cbase_pa = p_low * EXP(dlog_space*fraction)
        END IF
      END DO ! k

      frac_k=(cbase_m-zs_1d(kbase-1))/(zs_1d(kbase)-zs_1d(kbase-1))
      IF(frac_k /= fraction)                                            &
          PRINT*,' **GET_SLWC1D**  frac=',fraction,' frac_k=',frac_k

      cbase_k = t_1d(kbase-1)*(1.0-frac_k) + t_1d(kbase)*frac_k
!
!-----------------------------------------------------------------------
!
!  Get cloud top temperature
!
!-----------------------------------------------------------------------
!
      frac_k = (ctop_m-zs_1d(ktop-1)) / (zs_1d(ktop)-zs_1d(ktop-1))
      ctop_k = t_1d(ktop-1)*(1.0 - frac_k) + t_1d(ktop) * frac_k
!
!-----------------------------------------------------------------------
!
!  Calculate SLWC at each vertical grid point. For each level
!  we use an assumed cloud extending from the actual cloud base
!  to the height of the grid point in question.
!
!-----------------------------------------------------------------------
!
      DO k=kbase,ktop
        grid_top_pa = p_pa_1d(k)
        grid_top_k = t_1d(k)

        CALL slwc_revb(cbase_pa,cbase_k                                 &
                  ,grid_top_pa,grid_top_k,ctop_k                        &
                  ,adiabatic_lwc,adjusted_lwc,adjusted_slwc             &
                  ,i_status1,i_status2)
!
        IF(i_status2 == 1) THEN
          IF(iflag_slwc == 1) THEN
            slwc_1d(k) = adiabatic_lwc
          ELSE IF(iflag_slwc == 2) THEN
            slwc_1d(k) = adjusted_lwc
          ELSE IF(iflag_slwc == 3) THEN
            slwc_1d(k) = adjusted_slwc
          END IF
        ELSE
          WRITE(6,*)' Error Detected in SLWC'
        END IF
      END DO ! k
    END IF ! ktop > kbase & kbase > 2,  thick enough cloud exists
  END IF ! ctop_m > cbase_m,  cloud exists

  RETURN
END SUBROUTINE get_slwc1d

SUBROUTINE slwc_revb(cb_pa,cb_k,gt_pa,gt_k,ct_k,                        &
           adiabatic_lwc,adjusted_lwc,adjusted_slwc,                    &
           i_status1,i_status2)
!
!.......................HISTORY.............................
!
!     WRITTEN: CA. 1982 BY W. A. COOPER IN HP FORTRAN 4
!
!....... CALCULATES TEMPERATURE T AND LIQUID WATER CONTENT FROM
!..      CLOUD BASE PRESSURE P0 AND TEMPERATURE T0, FOR ADIABATIC
!..      ASCENT TO THE PRESSURE P.
!..     ->  INPUT:  CLOUD BASE PRESSURE P0 AND TEMPERATURE T0
!..                 PRESSURE AT OBSERVATION LEVEL P
!..     ->  OUTPUT: "ADIABATIC" TEMPERATURE T AND LIQUID WATER CONTENT
!
!     MODIFIED: November 1989 by Paul Lawson for LAPS/WISP.  Routine
!               now calculates adiabatic liquid water content
!               (ADIABATIC_LWC) using cloud base pressure and grid-top
!               temperature and pressure.  Also calculated are ADJUSTED_LWC,
!               which adjusts ADIABATIC_LWC using an empirical cloud
!               water depletion algorithm, and ADJUSTED_SLWC, which is
!               ADIABATIC_LWC in regions where T < 0 C adjusted
!               using an empirical algorithm by Marcia Politovich.
!
!               Subroutine is now hardwired for stratiform cloud only.
!               Can be modified to include Cu with input from LAPS main.
!
!               revb: ca 12/89 Calculate adiabatic lwc by going from cloud
!                     base to LAPS grid level instead to cloud top, thus
!                     helping to better calculate in layer clouds.
!                     Add TG (grid temperature) to calcualtion.
!
!               revc: 2/27/90 Correct error in code.  Zero-out slwc when grid
!                     temperature (GT) > 0.
!
!               J.Z.: 4/7/97 Correct error in code
!                     Grid temperature should be TG, not GT.
!
!
!     OUTPUTS:  ADIABATIC_LWC
!               ADJUSTED_LWC
!               ADJUSTED_SLWC
!               I_STATUS1 - 1 when -20 < cld_top_temp < 0 for Stratus
!                           0 Otherwise
!               I_STATUS2 - 1 when valid input data provided from main
!
  DATA eps/0.622/,cpd/1.0042E3/,cw/4.218E3/,rd/287.05/,alhv/2.501E6/
  INTEGER :: cty
  INTEGER :: i_status1, i_status2
  i_status1=1
  i_status2=1
!   2 Print *,'ENTER: P-BASE(mb), T-BASE(C), P-TOP, T-TOP, CLD TYPE'
!  READ(5,*) P0, T0, P, CTT, CTY
!  If(CTY.ne.0.and.CTY.ne.1) Go to 2
!
!  Hardwire cloud type (CTY) for stratus for now
!
  cty=0
!
!.....Convert Pa to mb and Kelvin to Celcius
!
  p0 = cb_pa/100.
  p  = gt_pa/100.
  t0 = cb_k - 273.15
  tg = gt_k - 273.15
  ctt= ct_k - 273.15
!  Print *, 'CTT in Sub = ', CTT
!
!  Check for valid input data...
!
  IF(p0 > 1013..OR.p0 < 50.) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!
  IF(t0 > 50..OR.t0 < -70.) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!
  IF(p > 1013..OR.p < 50.) THEN
    i_status2=0
    RETURN
  ELSE
  END IF
!
!  Set I_STATUS1 = F if 0 < cld top < -20 C (for stratus).
!
  IF(tg >= 0..OR.ctt < -20.) i_status1=0
!
  tk=t0+273.15
  e=vapor(t0)
  r=eps*e/(p0-e)
  cpt=cpd+r*cw
  thetaq=tk*(1000./(p0-e))**(rd/cpt)*EXP(alhv*r/(cpt*tk))
! 1ST APPROX
  t1=tk
  e=vapor(t1-273.15)
  rv=eps*e/(p-e)
  t1=thetaq/((1000./(p-e))**(rd/cpt)*EXP(alhv*rv/(cpt*t1)))
! SUCCESSIVE APPROXIMATIONS
  DO i=1,10
    e=vapor(t1-273.15)
    rv=eps*e/(p-e)
    t1=(thetaq/((1000./(p-e))**(rd/cpt)*EXP(alhv*rv/(cpt*t1)))          &
        +t1)/2.
    t=t1-273.15
!  Print *, P0,T0,P,T,E,RV,THETAQ
  END DO
! GET LWC
  e=vapor(t)
  rv=eps*e/(p-e)
  tw=r-rv
  adiabatic_lwc=tw*p*28.9644/(8.314E7*t1)*1.e9
  IF(adiabatic_lwc < 0.) adiabatic_lwc=0.
!  Print *, 'Adiabtic LWC = ', ADIABATIC_LWC
  IF(tg >= 0.) THEN
!
    adjusted_slwc=0.                                          ! Added 2/27/90
!

    IF(cty == 0.) THEN
      IF(ctt < -20.) THEN
        adjusted_lwc=0.
      ELSE IF(ctt < -15..AND.ctt >= -20.) THEN
        adjusted_lwc=adiabatic_lwc/8.
      ELSE IF(ctt < -10..AND.ctt >= -15.) THEN
        adjusted_lwc=adiabatic_lwc/4.
      ELSE
        adjusted_lwc=adiabatic_lwc/2.
      END IF
    ELSE
      IF(ctt < -25.) THEN
       adjusted_lwc=0.
      ELSE IF(ctt < -15..AND.ctt >= -25.) THEN
        adjusted_lwc=adiabatic_lwc/8.
      ELSE IF(ctt < -10..AND.ctt >= -15.) THEN
        adjusted_lwc=adiabatic_lwc/4.
      ELSE
        adjusted_lwc=adiabatic_lwc/2.
      END IF
    END IF
  ELSE
    IF(cty == 0.) THEN
      IF(ctt < -20.) THEN
        adjusted_lwc=0.
        adjusted_slwc=0.
      ELSE IF(ctt < -15..AND.ctt >= -20.) THEN
        adjusted_lwc=adiabatic_lwc/8.
        adjusted_slwc=adiabatic_lwc/8.
      ELSE IF(ctt < -10..AND.ctt >= -15.) THEN
        adjusted_lwc=adiabatic_lwc/4.
        adjusted_slwc=adiabatic_lwc/4.
      ELSE
        adjusted_lwc=adiabatic_lwc/2.
        adjusted_slwc=adiabatic_lwc/2.
      END IF
    ELSE
      IF(ctt < -25.) THEN
        adjusted_lwc=0.
        adjusted_slwc=0.
      ELSE IF(ctt < -15..AND.ctt >= -25.) THEN
        adjusted_lwc=adiabatic_lwc/8.
        adjusted_slwc=adiabatic_lwc/8.
      ELSE IF(ctt < -10..AND.ctt >= -15.) THEN
        adjusted_lwc=adiabatic_lwc/4.
        adjusted_slwc=adiabatic_lwc/4.
      ELSE
        adjusted_lwc=adiabatic_lwc/2.
        adjusted_slwc=adiabatic_lwc/2.
      END IF
    END IF
  END IF
!  Print *,'Adjusted LWC = ', ADJUSTED_LWC
!  Print *,'Adjusted SLWC = ', ADJUSTED_SLWC
END SUBROUTINE slwc_revb

!  FUNCTION TO CALCULATE VAPOR PRESSURE:
!

  FUNCTION vapor(tfp)
! INPUT IS IN DEGREES C.  IF GT 0, ASSUMED TO BE DEW POINT.  IF
! LESS THAN 0, ASSUMED TO BE FROST POINT.
! ROUTINE CODES GOFF-GRATCH FORMULA
  tvap=273.16+tfp
  IF(tfp > 0.) GO TO 1
! THIS IS ICE SATURATION VAPOR PRESSURE
  IF(tvap <= 0) tvap=1E-20
  e=-9.09718*(273.16/tvap-1.)-3.56654*ALOG10(273.16/tvap)               &
      +0.876793*(1.-tvap/273.16)
  vapor=6.1071*10.**e
  RETURN
  1    CONTINUE
! THIS IS WATER SATURATION VAPOR PRESSURE
  IF(tvap <= 0) tvap=1E-20
  e=-7.90298*(373.16/tvap-1.)+5.02808*ALOG10(373.16/tvap)               &
      -1.3816E-7*(10.**(11.344*(1.-tvap/373.16))-1.)                    &
      +8.1328E-3*(10.**(3.49149*(1-373.16/tvap))-1)
  vapor=1013.246*10.**e
  RETURN
  END FUNCTION vapor

!
!##################################################################
!##################################################################
!######                                                      ######
!######            FUNCTION WB_MELTING_THRES                 ######
!######                                                      ######
!##################################################################
!##################################################################
!
!

  FUNCTION wb_melting_thres(t_c,dbz)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!  This function calculates the wet-bulb threshold for melting snow
!  into rain as a function of dbz and t_c.
!
!  Currently it's simply set to a constant. Later it may be defined
!  as function of temperature and radar echo intensity.
!
!-----------------------------------------------------------------------
!
!  AUTHOR:
!  07/95
!
!  MODIFICATION HISTORY:
!  05/17/96 (Jian Zhang)
!
!-----------------------------------------------------------------------

  wb_melting_thres = 1.3  ! Units are C

  RETURN
  END FUNCTION wb_melting_thres


