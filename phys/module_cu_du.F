MODULE module_cu_du

   USE module_wrf_error

   REAL    , PARAMETER :: cincap = -10.
   REAL    , PARAMETER :: capemin = 10.
   REAL    , PARAMETER :: dpthmin = 1000.
   REAL    , PARAMETER :: alpha = 0.001
   REAL    , PARAMETER :: eps = 0.5
   REAL    , PARAMETER :: Vfall = 5.

!--------------------------------------------------------------------

CONTAINS

   SUBROUTINE DUCU(                                          &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,rho,RAINCV,NCA                                 &
             ,U,V,TH,T,W,dz8w,Z,Pcps,pi                      &
             ,W0AVG,XLV                                      &
             ,CP,RD,RV,G                                      &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            ! optionals
             ,RTHCUTEN,RQVCUTEN                              &
                                                             )
!
!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                            &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV
   REAL,         INTENT(IN   ) :: CP,RD,RV,G,EP2
   REAL,         INTENT(IN   ) :: SVP1,SVP2,SVP3,SVPT0

   INTEGER,      INTENT(IN   ) :: KTAU           

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                          T, &
                                                         QV, &
                                                       dz8w, &
                                                          z, &
                                                       Pcps, &
                                                        rho, &
                                                         pi
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      W0AVG

   REAL,  INTENT(IN   ) :: DT, DX
!
   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                           RAINCV

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
            INTENT(INOUT) ::                            NCA

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(OUT) ::                              CUBOT, &
                                                      CUTOP    

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) :: CU_ACT_FLAG

!
! Optional arguments
!

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
         OPTIONAL,                                           &
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN, &
                                                   RQVCUTEN

!
! LOCAL VARS

   LOGICAL :: flag_qr, flag_qi, flag_qs

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       TH1D, &
                                                       DZ1D, &
                                                        Z1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                    W0AVG1D

   REAL, DIMENSION( kts:kte )::                              &
                                                      DQVDT, &
                                                      DTHDT

   REAL    :: PPRATE,TST,tv,PRS,RHOE,W0,SCR1,DXSQ,tmp,RTHCUMAX

   INTEGER :: i,j,k,i_start,i_end,j_start,j_end,sz,NTST,ICLDCK
!

   DXSQ=DX*DX

   NTST=STEPCU
   ICLDCK=MOD(KTAU,NTST)
   IF(ICLDCK.EQ.0 .or. KTAU .eq. 1) then
!
!  Keep away from specified and relaxation zone (should be for just specified and nested bc)
   sz = 1
   i_start=max(ids+sz,its)
   i_end=min(ide-1-sz,ite)
   j_start=max(jds+sz,jts)
   j_end=min(jde-1-sz,jte)
!
     DO J = j_start, j_end
       DO I= i_start, i_end

            DO k=kts,kte
               DQVDT(k)=0.
               DTHDT(k)=0.
            ENDDO
            RAINCV(I,J)=0.
            CUTOP(I,J)=KTS
            CUBOT(I,J)=KTE+1
!
! assign vars from 3D to 1D

            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               TH1D(K) =TH(I,K,J)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               DZ1D(k)=dz8w(I,K,J)
               Z1D(k)=z(I,K,J)
            ENDDO
            CALL DUCU1D(I, J,                       &
                 U1D,V1D,T1D,QV1D,P1D,DZ1D,Z1D,     &
                 W0AVG1D,DT,DX,DXSQ,RHO1D,TH1D,     &
                 XLV,CP,RD,RV,G,                    &
                 EP2,SVP1,SVP2,SVP3,SVPT0,          &
                 DQVDT,DTHDT,                       &
                 PPRATE,NCA,NTST,                   &
                 CUTOP,CUBOT,                       &
                 ids,ide, jds,jde, kds,kde,         &
                 ims,ime, jms,jme, kms,kme,         &
                 its,ite, jts,jte, kts,kte)
            IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
              DO K=kts,kte
                 RTHCUTEN(I,K,J)=DTHDT(K)
                 RQVCUTEN(I,K,J)=DQVDT(K)
              ENDDO
              RAINCV(I,J)=PPRATE*DT
            ENDIF
       ENDDO
     ENDDO
   ENDIF
!
   END SUBROUTINE DUCU
! ****************************************************************************
!-----------------------------------------------------------
   SUBROUTINE DUCU1D (I, J,                           &
                      U0,V0,T0,QV0,P0,DZQ,Z,W0AVG1D,       &
                      DELT,DX,DXSQ,rhoe,TH0,               &
                      XLV,CP,RD,RV,G,                      &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      DQVDT,DTHDT,                         &
                      PPRATE,NCA,NTST,                     &
                      CUTOP,CUBOT,                         &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte)
!-----------------------------------------------------------
!
      IMPLICIT NONE
!-----------------------------------------------------------
      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J,NTST

!
      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                       T0, &
                                                      TH0, &
                                                      QV0, &
                                                       P0, &
                                                     rhoe, &
                                                      DZQ, &
                                                        Z, &
                                                  W0AVG1D
!
      REAL,  INTENT(IN   ) :: DELT,DX,DXSQ
!

      REAL,  INTENT(IN   ) :: XLV,CP,RD,RV,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0

!
      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                    DQVDT, &
                                                    DTHDT

      REAL,    DIMENSION( ims:ime , jms:jme ),             &
            INTENT(INOUT) ::                          NCA

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(OUT) ::                          CUBOT, &
                                                    CUTOP
     REAL,  INTENT(OUT  ) :: PPRATE
!
!...DEFINE LOCAL VARIABLES...
!
      REAL, DIMENSION( kts:kte ) :: cond,h,hs,qs,x
      REAL    :: buoy,cape,cin,condpr,dh,dq,dt,dtm,ep,es, &
                 evap,hp,mp,qp,qsp,rrk,rrkp, &
                 tadp,tdp,zb,zg,zi,zt
      INTEGER :: ipos,isat,k,kb,ki,kt
      INTEGER :: iprint,jprint
      iprint=73
      jprint=20
!
!...DEFINE PROFILES
      DO k=kts,kte
        h(k)=cp*t0(k)+g*z(k)+xlv*qv0(k)
        es=1000.*svp1*EXP(svp2*(t0(k)-svpt0)/(t0(k)-svp3))
        qs(k)=ep2*es/(p0(k)-es)
        hs(k)=cp*t0(k)+g*z(k)+xlv*qs(k)
        x(k)=xlv*xlv*qs(k)/(cp*rv*t0(k)*t0(k))
        dthdt(k)=0.
        dqvdt(k)=0.
      ENDDO
      pprate=0.
      zg=z(1)-0.5*dzq(1)
!
!...LOOP OVER PARCELS
      loop_origin: DO ki=kts,kte
        hp=h(ki)
        qp=qv0(ki)
        mp=alpha*rhoe(ki)*dzq(ki)
        zi=z(ki)
        buoy=0.
        cape=0.
        cin=0.
        dtm=0.
        isat=0
        ipos=0
        kt=0
        kb=0
        cond=0.
!
!...LIFT PARCEL
        loop_lift: DO k=ki+1,kte
          tadp=t0(ki)+(g/cp)*(z(ki)-z(k))
          ep=p0(k)*qv0(ki)/(ep2+qv0(ki))
          tdp=(svpt0-(svp3/svp2)*ALOG(0.001*ep/svp1))/(1.-(1./svp2)*ALOG(0.001*ep/svp1))
          IF(tadp.GE.tdp)THEN
!         unsaturated
            IF(isat.EQ.1)THEN
              print *,i,j,'sounding warning: unsat above sat'
            ENDIF
            dt=tadp-t0(k)
            cond(k)=0.
            condpr=0.
          ELSE
!         saturated
            IF(isat.EQ.0)THEN
              kb=k
              zb=z(k)-0.5*dzq(k)
            ENDIF
            isat=1
            dh=hp-hs(k)
            dt=(dh/cp)/(1.+x(k))
            qsp=qs(k)+(dh/xlv)*x(k)/(1.+x(k))
!...CONDENSATE PRODUCED
            cond(k)=mp*(qp-qsp)
            qp=qsp
          ENDIF
          buoy=buoy+g*dt*dzq(k)/t0(k)
          cape=max(cape,buoy)
          IF(buoy.GE.cincap)cin=min(cin,buoy)
          IF(dt .GE. 0.)THEN
            kt=k
            zt=z(k)+0.5*dzq(k)
          ELSE IF(dt .LT. 0. .AND. dtm .GE. 0.)THEN
! cloud top is level closest to parcel temperature
            IF(abs(dt) .LT. abs(dtm))THEN
              kt=k
              zt=z(k)+0.5*dzq(k)
            ENDIF
          ENDIF
          dtm=dt
! continue lifting until buoyancy is gone
          IF(buoy.LT.cincap)THEN
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,ki,k,buoy,isat,ipos,' buoy,isat,ipos'
!         capped or cloud top
            EXIT loop_lift
          ENDIF
          IF(buoy.GT.0.)THEN
!         positive area detected
            ipos=1
          ENDIF
          IF(k.EQ.1)THEN
            kt=k
            zt=z(k)+0.5*dzq(k)
            zi=z(ki)
            print *,'sounding warning: cloud top at model top'
          ENDIF
        ENDDO loop_lift
!
!...CHECK FOR CLOUD
        IF(isat.EQ.0)THEN
!       no cloud from lifting - no convection
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,ki,' no saturation'
          CYCLE loop_origin
        ENDIF
        IF(zt-zb.LE.dpthmin)THEN
!       not more than one cloud level - no convection
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,ki,zb,zt,' cloud too shallow'
          CYCLE loop_origin
        ENDIF
        IF(ipos.EQ.0)THEN
!       no buoyancy in cloud - no convection
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,ki,' no buoyancy'
          CYCLE loop_origin
        ENDIF
        IF(cape.LE.capemin)THEN
!       not enough cape
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,cape,' not enough cape'
          CYCLE loop_origin
        ENDIF
!
!...IF CHECK FOR CLOUD SUCCESSFUL
!
!...DETRAINMENT
        dh=hp-hs(kt)
        dt=(dh/cp)/(1.+x(kt))
        dq=qs(kt)+(dh/xlv)*x(kt)/(1.+x(kt))-qv0(kt)
        dthdt(kt)=dthdt(kt)+mp*(th0(kt)/t0(kt))*dt/(rhoe(kt)*dzq(kt))
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,kt,dthdt(kt),mp,th0(kt)/t0(kt),dt,rhoe(k)*dzq(k),' detr'
        dqvdt(kt)=dqvdt(kt)+mp*dq/(rhoe(kt)*dzq(kt))
!
!...SUBSIDENCE
        loop_subsidence: DO k=kt-1,ki,-1 
          dthdt(k)=dthdt(k)+mp*(th0(k+1)-th0(k))/(rhoe(k)*dzq(k))
          dqvdt(k)=dqvdt(k)+mp*(qv0(k+1)-qv0(k))/(rhoe(k)*dzq(k))
if(i.eq.iprint.and.j.eq.jprint)print *,i,j,k,mp,dqvdt(k),dthdt(k),' subs'
        ENDDO loop_subsidence
!
!...RAINFALL
        rrkp=0.
        loop_rainfall: DO k=kt,1,-1
          rrk=rrkp+cond(k)
          rrkp=rrk
        ENDDO loop_rainfall
        pprate=pprate+rrkp
if(i.eq.iprint.and.j.eq.jprint)print *,'conv ',i,j,ki,kb,kt,pprate,zb,zt,condpr,cape
!print *,'cloud ',i,j,ki,kb,kt,pprate,condpr,cin,cape
      ENDDO loop_origin

!-----------------------------------------------------------------------
   END SUBROUTINE DUCU1D
! ***********************************************************************
!====================================================================
   SUBROUTINE ducuinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,      &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,         &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QI,P_QS,P_FIRST_SCALAR

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQRCUTEN, &
                                                          RQICUTEN, &
                                                          RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   INTEGER :: i, j, k, itf, jtf, ktf
   REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RTHCUTEN(i,k,j)=0.
         RQVCUTEN(i,k,j)=0.
         RQCCUTEN(i,k,j)=0.
         RQRCUTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      IF (P_QI .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQICUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QS .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQSCUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      DO j=jts,jtf
      DO i=its,itf
         NCA(i,j)=-100.
      ENDDO
      ENDDO

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         W0AVG(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

   endif
 
   END SUBROUTINE ducuinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!               TL starts here.                            !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  Differentiation of ducu1d in forward (tangent) mode:
!   variations  of output variables: dthdt pprate dqvdt
!   with respect to input variables: dthdt p0 z t0 th0 rhoe dzq
!                dqvdt qv0
! ****************************************************************************
!-----------------------------------------------------------
SUBROUTINE DUCU1D_D(i, j, u0, v0, t0, t0d, qv0, qv0d, p0, p0d, dzq, dzqd&
&  , z, zd, w0avg1d, delt, dx, dxsq, rhoe, rhoed, th0, th0d, xlv, cp, rd&
&  , rv, g, ep2, svp1, svp2, svp3, svpt0, dqvdt, dqvdtd, dthdt, dthdtd, &
&  pprate, pprated, nca, ntst, cutop, cubot, ids, ide, jds, jde, kds, &
&  kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
!-----------------------------------------------------------------------
  REAL, INTENT(IN) :: cp, delt, dx, dxsq, ep2, g, rd, rv, svp1, svp2, &
&  svp3, svpt0, xlv
  INTEGER, INTENT(IN) :: ime, ims, jme, jms, kte, kts
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: cubot, cutop
  REAL, DIMENSION(kts:kte), INTENT(INOUT) :: dqvdt, dthdt
  REAL :: dqvdtd(kts:kte), dthdtd(kts:kte)
  INTEGER, INTENT(IN) :: i, ide, ids, ite, its, j, jde, jds, jte, jts, &
&  kde, kds, kme, kms, ntst
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: nca
  REAL, INTENT(OUT) :: pprate, pprated
  REAL, DIMENSION(kts:kte), INTENT(IN) :: dzq, dzqd, p0, p0d, qv0, qv0d&
&  , rhoe, rhoed, t0, t0d, th0, th0d, u0, v0, w0avg1d, z, zd
  REAL :: abs1, abs2, alpha, arg1, arg1d, arg2, buoy, cape, cin, cond(&
&  kts:kte), condd(kts:kte), condpr, dh, dhd, dq, dqd, dt, dtd, dtm, ep&
&  , eps, es, esd, evap, evapd, h(kts:kte), hd(kts:kte), hp, hpd, hs(kts&
&  :kte), hsd(kts:kte), mp, mpd, qp, qpd, qs(kts:kte), qsd(kts:kte), qsp&
&  , qspd, rrk, rrkd, rrkp, rrkpd, tadp, tdp, vfall, x(kts:kte), xd(kts:&
&  kte), zb, zg, zi, zt
!x  UNKNOWNTYPE :: capemin
!x  UNKNOWNTYPE :: cincap
!x  UNKNOWNTYPE :: dpthmin
   REAL    , PARAMETER :: cincap = -10.
   REAL    , PARAMETER :: capemin = 10.
   REAL    , PARAMETER :: dpthmin = 1000.
   REAL    , PARAMETER :: alpha = 0.001
   REAL    , PARAMETER :: eps = 0.5
   REAL    , PARAMETER :: Vfall = 5.

  INTEGER :: ipos, iprint, isat, jprint, k, kb, ki, kt
  INTRINSIC EXP, MAX, ABS, ALOG, MIN
!-----------------------------------------------------------
!
!
!
!
!
!...DEFINE LOCAL VARIABLES...
!
  iprint = 73
  jprint = 20
  hd(kts:kte) = 0.0
  qsd(kts:kte) = 0.0
  xd(kts:kte) = 0.0
  hsd(kts:kte) = 0.0
!
!...DEFINE PROFILES
  DO k=kts,kte
    hd(k) = cp*t0d(k) + g*zd(k) + xlv*qv0d(k)
    h(k) = cp*t0(k) + g*z(k) + xlv*qv0(k)
    arg1d = (svp2*t0d(k)*(t0(k)-svp3)-svp2*(t0(k)-svpt0)*t0d(k))/(t0(k)-&
&      svp3)**2
    arg1 = svp2*(t0(k)-svpt0)/(t0(k)-svp3)
    esd = 1000.*svp1*arg1d*EXP(arg1)
    es = 1000.*svp1*EXP(arg1)
    qsd(k) = (ep2*esd*(p0(k)-es)-ep2*es*(p0d(k)-esd))/(p0(k)-es)**2
    qs(k) = ep2*es/(p0(k)-es)
    hsd(k) = cp*t0d(k) + g*zd(k) + xlv*qsd(k)
    hs(k) = cp*t0(k) + g*z(k) + xlv*qs(k)
    xd(k) = (xlv**2*qsd(k)*cp*rv*t0(k)**2-xlv**2*qs(k)*cp*rv*(t0d(k)*t0(&
&      k)+t0(k)*t0d(k)))/(cp*rv*t0(k)*t0(k))**2
    x(k) = xlv*xlv*qs(k)/(cp*rv*t0(k)*t0(k))
    dthdtd(k) = 0.0
    dthdt(k) = 0.
    dqvdtd(k) = 0.0
    dqvdt(k) = 0.
  END DO

  pprate = 0.
  zg = z(1) - 0.5*dzq(1)
  pprated = 0.0
!
!...LOOP OVER PARCELS
loop_origin:DO ki=kts,kte
    hpd = hd(ki)
    hp = h(ki)
    qpd = qv0d(ki)
    qp = qv0(ki)
    mpd = alpha*(rhoed(ki)*dzq(ki)+rhoe(ki)*dzqd(ki))
    mp = alpha*rhoe(ki)*dzq(ki)
    zi = z(ki)
    buoy = 0.
    cape = 0.
    cin = 0.
    dtm = 0.
    isat = 0
    ipos = 0
    kt = 0
    kb = 0
    cond = 0.
    condd(kts:kte) = 0.0
!
!...LIFT PARCEL
loop_lift:DO k=ki+1,kte
      tadp = t0(ki) + g/cp*(z(ki)-z(k))
      ep = p0(k)*qv0(ki)/(ep2+qv0(ki))
      arg1 = 0.001*ep/svp1
      arg2 = 0.001*ep/svp1
      tdp = (svpt0-svp3/svp2*ALOG(arg1))/(1.-1./svp2*ALOG(arg2))
      IF (tadp .GE. tdp) THEN
!         unsaturated
        IF (isat .EQ. 1) PRINT*, i, j, &
&                         'sounding warning: unsat above sat'
        dt = tadp - t0(k)
        condd(k) = 0.0
        cond(k) = 0.
        condpr = 0.
      ELSE
!         saturated
        IF (isat .EQ. 0) THEN
          kb = k
          zb = z(k) - 0.5*dzq(k)
        END IF
        isat = 1
        dhd = hpd - hsd(k)
        dh = hp - hs(k)
        dt = dh/cp/(1.+x(k))
        qspd = qsd(k) + ((dhd*x(k)/xlv+dh*xd(k)/xlv)*(1.+x(k))-dh*x(k)*&
&          xd(k)/xlv)/(1.+x(k))**2
        qsp = qs(k) + dh/xlv*x(k)/(1.+x(k))
!...CONDENSATE PRODUCED
        condd(k) = mpd*(qp-qsp) + mp*(qpd-qspd)
        cond(k) = mp*(qp-qsp)
        qpd = qspd
        qp = qsp
      END IF
      buoy = buoy + g*dt*dzq(k)/t0(k)
      IF (cape .LT. buoy) THEN
        cape = buoy
      ELSE
        cape = cape
      END IF

      IF (buoy .GE. cincap) THEN
        IF (cin .GT. buoy) THEN
          cin = buoy
        ELSE
          cin = cin
        END IF
      END IF
      IF (dt .GE. 0.) THEN
        kt = k
        zt = z(k) + 0.5*dzq(k)
      ELSE IF (dt .LT. 0. .AND. dtm .GE. 0.) THEN
        IF (dt .GE. 0.) THEN
          abs1 = dt
        ELSE
          abs1 = -dt
        END IF
        IF (dtm .GE. 0.) THEN
          abs2 = dtm
        ELSE
          abs2 = -dtm
        END IF
! cloud top is level closest to parcel temperature
        IF (abs1 .LT. abs2) THEN
          kt = k
          zt = z(k) + 0.5*dzq(k)
        END IF
      END IF
      dtm = dt
! continue lifting until buoyancy is gone
      IF (buoy .LT. cincap) THEN
        GOTO 100
      ELSE
!         capped or cloud top
        IF (buoy .GT. 0.) ipos = 1
!         positive area detected
        IF (k .EQ. 1) THEN
          kt = k
          zt = z(k) + 0.5*dzq(k)
          zi = z(ki)
          PRINT*, 'sounding warning: cloud top at model top'
        END IF
      END IF

    END DO loop_lift
    GOTO 110
 100 IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, ki, k, buoy, &
&                                            isat, ipos, &
&                                            ' buoy,isat,ipos'
!
!...CHECK FOR CLOUD
 110 IF (isat .EQ. 0) THEN
!       no cloud from lifting - no convection
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, ki, &
&                                             ' no saturation'
    ELSE IF (zt - zb .LE. dpthmin) THEN
!       not more than one cloud level - no convection
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, ki, zb, zt, &
&                                             ' cloud too shallow'
    ELSE IF (ipos .EQ. 0) THEN
!       no buoyancy in cloud - no convection
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, ki, &
&                                             ' no buoyancy'
    ELSE IF (cape .LE. capemin) THEN
!       not enough cape
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, cape, &
&                                             ' not enough cape'
    ELSE
!
!...IF CHECK FOR CLOUD SUCCESSFUL
!
!...DETRAINMENT
      dhd = hpd - hsd(kt)
      dh = hp - hs(kt)
      dtd = (dhd*(1.+x(kt))/cp-dh*xd(kt)/cp)/(1.+x(kt))**2
      dt = dh/cp/(1.+x(kt))
      dqd = qsd(kt) + ((dhd*x(kt)/xlv+dh*xd(kt)/xlv)*(1.+x(kt))-dh*x(kt)&
&        *xd(kt)/xlv)/(1.+x(kt))**2 - qv0d(kt)
      dq = qs(kt) + dh/xlv*x(kt)/(1.+x(kt)) - qv0(kt)

      dthdtd(kt) = dthdtd(kt) + (((mpd*dt+mp*dtd)*th0(kt)/t0(kt)+mp*dt*(&
&        th0d(kt)*t0(kt)-th0(kt)*t0d(kt))/t0(kt)**2)*rhoe(kt)*dzq(kt)-mp&
&        *th0(kt)*dt*(rhoed(kt)*dzq(kt)+rhoe(kt)*dzqd(kt))/t0(kt))/(rhoe&
&        (kt)*dzq(kt))**2
      dthdt(kt) = dthdt(kt) + mp*(th0(kt)/t0(kt))*dt/(rhoe(kt)*dzq(kt))
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, kt, dthdt(kt)&
&                                             , mp, th0(kt)/t0(kt), dt, &
&                                             rhoe(k)*dzq(k), ' detr'
      dqvdtd(kt) = dqvdtd(kt) + ((mpd*dq+mp*dqd)*rhoe(kt)*dzq(kt)-mp*dq*&
&        (rhoed(kt)*dzq(kt)+rhoe(kt)*dzqd(kt)))/(rhoe(kt)*dzq(kt))**2
      dqvdt(kt) = dqvdt(kt) + mp*dq/(rhoe(kt)*dzq(kt))
!
!...SUBSIDENCE
loop_subsidence:DO k=kt-1,ki,-1
        dthdtd(k) = dthdtd(k) + ((mpd*(th0(k+1)-th0(k))+mp*(th0d(k+1)-&
&          th0d(k)))*rhoe(k)*dzq(k)-mp*(th0(k+1)-th0(k))*(rhoed(k)*dzq(k&
&          )+rhoe(k)*dzqd(k)))/(rhoe(k)*dzq(k))**2
        dthdt(k) = dthdt(k) + mp*(th0(k+1)-th0(k))/(rhoe(k)*dzq(k))
        dqvdtd(k) = dqvdtd(k) + ((mpd*(qv0(k+1)-qv0(k))+mp*(qv0d(k+1)-&
&          qv0d(k)))*rhoe(k)*dzq(k)-mp*(qv0(k+1)-qv0(k))*(rhoed(k)*dzq(k&
&          )+rhoe(k)*dzqd(k)))/(rhoe(k)*dzq(k))**2
        dqvdt(k) = dqvdt(k) + mp*(qv0(k+1)-qv0(k))/(rhoe(k)*dzq(k))
        IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, i, j, k, mp, &
&                                               dqvdt(k), dthdt(k), &
&                                               ' subs'
      END DO loop_subsidence
!
!...RAINFALL
      rrkp = 0.
      rrkpd = 0.0
loop_rainfall:DO k=kt,1,-1
        rrkd = rrkpd + condd(k)
        rrk = rrkp + cond(k)
        rrkpd = rrkd  !!!XIAO NOTE
        rrkp = rrk
      END DO loop_rainfall
      pprated = pprated + rrkpd
      pprate = pprate + rrkp
      IF (i .EQ. iprint .AND. j .EQ. jprint) PRINT*, 'conv ', i, j, ki, &
&                                             kb, kt, pprate, zb, zt, &
&                                             condpr, cape
!     PRINT*, 'cloud ', i, j, ki, kb, kt, pprate, condpr, cin, cape
    END IF
  END DO loop_origin
END SUBROUTINE DUCU1D_D

!  Differentiation of ducu in forward (tangent) mode:
!   variations  of output variables: raincv rthcuten rqvcuten
!   with respect to input variables: th t pcps qv z rho dz8w
! optionals
SUBROUTINE DUCU_D(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, rhod, raincv&
&  , raincvd, nca, u, v, th, thd, t, td, w, dz8w, dz8wd, z, zd, pcps, &
&  pcpsd, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, qvd, rthcuten, &
&  rthcutend, rqvcuten, rqvcutend)
  IMPLICIT NONE
!
  INTEGER, INTENT(IN) :: ime, ims, jme, jms, kme, kms
  LOGICAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: cu_act_flag
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: cubot, cutop
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: dz8w, dz8wd&
&  , pcps, pcpsd, pi, qv, qvd, rho, rhod, t, td, th, thd, u, v, w, z, zd
  INTEGER, INTENT(IN) :: ide, ids, ite, its, jde, jds, jte, jts, kde, &
&  kds, ktau, kte, kts, stepcu
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: nca, raincv
  REAL :: raincvd(ims:ime, jms:jme)
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), OPTIONAL :: rqvcuten, &
&  rqvcutend, rthcuten, rthcutend
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: w0avg
  LOGICAL, INTENT(IN) :: warm_rain
  REAL, INTENT(IN) :: cp, dt, dx, ep2, g, rd, rv, svp1, svp2, svp3, &
&  svpt0, xlv
  REAL :: dqvdt(kts:kte), dqvdtd(kts:kte), dthdt(kts:kte), dthdtd(kts:&
&  kte), dxsq, dz1d(kts:kte), dz1dd(kts:kte), p1d(kts:kte), p1dd(kts:kte&
&  ), pprate, pprated, prs, qv1d(kts:kte), qv1dd(kts:kte), rho1d(kts:kte&
&  ), rho1dd(kts:kte), rhoe, rthcumax, scr1, t1d(kts:kte), t1dd(kts:kte)&
&  , th1d(kts:kte), th1dd(kts:kte), tmp, tst, tv, u1d(kts:kte), v1d(kts:&
&  kte), w0, w0avg1d(kts:kte), z1d(kts:kte), z1dd(kts:kte)
  LOGICAL :: flag_qi, flag_qr, flag_qs
  INTEGER :: i, i_end, i_start, icldck, j, j_end, j_start, k, ntst, sz
  INTRINSIC MOD, MAX, PRESENT, MIN
!-------------------------------------------------------------
!
!
!
! Optional arguments
!
!
! LOCAL VARS
!
  dxsq = dx*dx
  ntst = stepcu
  icldck = MOD(ktau, ntst)
  IF (icldck .EQ. 0 .OR. ktau .EQ. 1) THEN
!
!  Keep away from specified and relaxation zone (should be for just specified and nested bc)
    sz = 1
    IF (ids + sz .LT. its) THEN
      i_start = its
    ELSE
      i_start = ids + sz
    END IF
    IF (ide - 1 - sz .GT. ite) THEN
      i_end = ite
    ELSE
      i_end = ide - 1 - sz
    END IF
    IF (jds + sz .LT. jts) THEN
      j_start = jts
    ELSE
      j_start = jds + sz
    END IF
    IF (jde - 1 - sz .GT. jte) THEN
      j_end = jte
    ELSE
      j_end = jde - 1 - sz
    END IF

    raincvd(ims:ime, jms:jme) = 0.0
    rthcutend(ims:ime, kms:kme, jms:jme) = 0.0
    rqvcutend(ims:ime, kms:kme, jms:jme) = 0.0
    qv1dd(kts:kte) = 0.0
    dthdtd(kts:kte) = 0.0
    th1dd(kts:kte) = 0.0
    rho1dd(kts:kte) = 0.0
    p1dd(kts:kte) = 0.0
    z1dd(kts:kte) = 0.0
    dz1dd(kts:kte) = 0.0
    t1dd(kts:kte) = 0.0
    dqvdtd(kts:kte) = 0.0
!
    DO j=j_start,j_end
      DO i=i_start,i_end
        DO k=kts,kte
          dqvdtd(k) = 0.0
          dqvdt(k) = 0.
          dthdtd(k) = 0.0
          dthdt(k) = 0.
        END DO
        raincvd(i, j) = 0.0
        raincv(i, j) = 0.
        cutop(i, j) = kts
        cubot(i, j) = kte + 1
!
! assign vars from 3D to 1D
        DO k=kts,kte
          u1d(k) = u(i, k, j)
          v1d(k) = v(i, k, j)
          t1dd(k) = td(i, k, j)
          t1d(k) = t(i, k, j)
          th1dd(k) = thd(i, k, j)
          th1d(k) = th(i, k, j)
          rho1dd(k) = rhod(i, k, j)
          rho1d(k) = rho(i, k, j)
          qv1dd(k) = qvd(i, k, j)
          qv1d(k) = qv(i, k, j)
          p1dd(k) = pcpsd(i, k, j)
          p1d(k) = pcps(i, k, j)
          w0avg1d(k) = w0avg(i, k, j)
          dz1dd(k) = dz8wd(i, k, j)
          dz1d(k) = dz8w(i, k, j)
          z1dd(k) = zd(i, k, j)
          z1d(k) = z(i, k, j)
        END DO

        CALL DUCU1D_D(i, j, u1d, v1d, t1d, t1dd, qv1d, qv1dd, p1d, p1dd&
&                , dz1d, dz1dd, z1d, z1dd, w0avg1d, dt, dx, dxsq, rho1d&
&                , rho1dd, th1d, th1dd, xlv, cp, rd, rv, g, ep2, svp1, &
&                svp2, svp3, svpt0, dqvdt, dqvdtd, dthdt, dthdtd, pprate&
&                , pprated, nca, ntst, cutop, cubot, ids, ide, jds, jde&
&                , kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts&
&                , jte, kts, kte)
        IF (PRESENT(rthcuten) .AND. PRESENT(rqvcuten)) THEN
          DO k=kts,kte
            rthcutend(i, k, j) = dthdtd(k)
            rthcuten(i, k, j) = dthdt(k)
            rqvcutend(i, k, j) = dqvdtd(k)
            rqvcuten(i, k, j) = dqvdt(k)
          END DO
          raincvd(i, j) = dt*pprated
          raincv(i, j) = pprate*dt
        END IF
      END DO
    END DO
  ELSE
    raincvd(ims:ime, jms:jme) = 0.0
    rthcutend(ims:ime, kms:kme, jms:jme) = 0.0
    rqvcutend(ims:ime, kms:kme, jms:jme) = 0.0
  END IF
END SUBROUTINE DUCU_D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!               AD starts here.                            !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  Differentiation of ducu1d in reverse (adjoint) mode:
!   gradient, with respect to input variables: dthdt p0 z t0 th0
!                rhoe dzq dqvdt qv0
!   of linear combination of output variables: dthdt p0 z t0 th0
!                pprate rhoe dzq dqvdt qv0
! ****************************************************************************
!-----------------------------------------------------------
SUBROUTINE DUCU1D_B(i, j, u0, v0, t0, t0b, qv0, qv0b, p0, p0b, dzq, dzqb&
&  , z, zb0, w0avg1d, delt, dx, dxsq, rhoe, rhoeb, th0, th0b, xlv, cp, &
&  rd, rv, g, ep2, svp1, svp2, svp3, svpt0, dqvdt, dqvdtb, dthdt, dthdtb&
&  , pprate, pprateb, nca, ntst, cutop, cubot, ids, ide, jds, jde, kds, &
&  kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
!-----------------------------------------------------------------------
  REAL, INTENT(IN) :: cp, delt, dx, dxsq, ep2, g, rd, rv, svp1, svp2, &
&  svp3, svpt0, xlv
  INTEGER, INTENT(IN) :: ime, ims, jme, jms, kte, kts
  REAL :: cubot(ims:ime, jms:jme), cutop(ims:ime, jms:jme), dqvdt(kts:&
&  kte), dqvdtb(kts:kte), dthdt(kts:kte), dthdtb(kts:kte), dzqb(kts:kte)&
&  , nca(ims:ime, jms:jme), p0b(kts:kte), pprate, pprateb, qv0b(kts:kte)&
&  , rhoeb(kts:kte), t0b(kts:kte), th0b(kts:kte), zb0(kts:kte)
  INTEGER, INTENT(IN) :: i, ide, ids, ite, its, j, jde, jds, jte, jts, &
&  kde, kds, kme, kms, ntst
  REAL, DIMENSION(kts:kte), INTENT(IN) :: dzq, p0, qv0, rhoe, t0, th0, &
&  u0, v0, w0avg1d, z
  INTEGER :: ad_from, ad_from0, ad_to, branch, ipos, iprint, isat, &
&  jprint, k, kb, ki, kt
  REAL :: abs1, abs2, alpha, buoy, cape, cin, cond(kts:kte), condb(kts:&
&  kte), condpr, dh, dhb, dq, dqb, dt, dtb, dtm, ep, eps, es, esb, evap&
&  , evapb, h(kts:kte), hb(kts:kte), hp, hpb, hs(kts:kte), hsb(kts:kte)&
&  , mp, mpb, qp, qpb, qs(kts:kte), qsb(kts:kte), qsp, qspb, rrk, rrkb, &
&  rrkp, rrkpb, tadp, tdp, temp, temp0, temp1, temp10, temp10b, temp10b0&
&  , temp10b1, temp10b2, temp11, temp11b, temp12, temp12b, temp13, &
&  temp1b, temp1b0, temp1b1, temp2, temp3, temp3b, temp4, temp5, temp5b&
&  , temp6, temp6b, temp6b0, temp7, temp7b, temp8, temp8b, temp8b0, &
&  temp9, temp9b, temp9b0, tempb, vfall, x(kts:kte)
  REAL :: xb(kts:kte), zb, zg, zi, zt

!new definition
   REAL :: ess(kts:kte), buoys(kts:kte), capes(kts:kte)
   REAL :: rrkps(kts:kte), rrks(kts:kte), evaps(kts:kte), evapss(kts:kte)
   REAL :: tadps(kts:kte), tdps(kts:kte)
   REAL :: qps(kts:kte)  ! Zhangl
   INTEGER :: kt_s(kts:kte), kb_s(kts:kte)
!end new definition

!x  UNKNOWNTYPE :: capemin
!x  UNKNOWNTYPE :: cincap
!x  UNKNOWNTYPE :: dpthmin
   REAL    , PARAMETER :: cincap = -10.
   REAL    , PARAMETER :: capemin = 10.
   REAL    , PARAMETER :: dpthmin = 1000.
   REAL    , PARAMETER :: alpha = 0.001
   REAL    , PARAMETER :: eps = 0.5
   REAL    , PARAMETER :: Vfall = 5.

  INTRINSIC EXP, MAX, ABS, ALOG, MIN
!-----------------------------------------------------------
!
!
!
!
!
!...DEFINE LOCAL VARIABLES...
!
  iprint = 73
  jprint = 20
!
! initialization of variables
  condb(kts:kte) = 0.0
  hb(kts:kte) = 0.0
  qsb(kts:kte) = 0.0
  xb(kts:kte) = 0.0
  hsb(kts:kte) = 0.0
  evapb = 0.0; rrkpb = 0.0; rrkb=0.0
! end initizlization
!
!...DEFINE PROFILES
  DO k=kts,kte
    h(k) = cp*t0(k) + g*z(k) + xlv*qv0(k)
    es = 1000.*svp1*EXP(svp2*(t0(k)-svpt0)/(t0(k)-svp3))
    ess(k) = es !new
    qs(k) = ep2*es/(p0(k)-es)
    hs(k) = cp*t0(k) + g*z(k) + xlv*qs(k)
    x(k) = xlv*xlv*qs(k)/(cp*rv*t0(k)*t0(k))
    dthdt(k) = 0.
    dqvdt(k) = 0.
  END DO

  pprate = 0.
!
!...LOOP OVER PARCELS
loop_origin:DO ki=kts,kte
    hp = h(ki)
    qp = qv0(ki)
    mp = alpha*rhoe(ki)*dzq(ki)
    buoy = 0.
    cape = 0.
    dtm = 0.
    isat = 0
    ipos = 0
    kt = 0
    kb = 0
    cond = 0.
!
!...LIFT PARCEL
loop_lift:DO k=ki+1,kte
      tadp = t0(ki) + g/cp*(z(ki)-z(k))
      ep = p0(k)*qv0(ki)/(ep2+qv0(ki))
      tdp = (svpt0-svp3/svp2*ALOG(0.001*ep/svp1))/(1.-1./svp2*ALOG(0.001&
&        *ep/svp1))
      tadps(k) = tadp !new
      tdps(k) = tdp !new
      IF (tadp .GE. tdp) THEN
!         unsaturated
        dt = tadp - t0(k)
        cond(k) = 0.
      ELSE
!         saturated
        IF (isat .EQ. 0) THEN
          kb = k
          zb = z(k) - 0.5*dzq(k)
        END IF
        isat = 1
        dh = hp - hs(k)
        dt = dh/cp/(1.+x(k))
        qsp = qs(k) + dh/xlv*x(k)/(1.+x(k))
        qps(k) = qp                            ! Zhangl
        cond(k) = mp*(qp-qsp)
        qp = qsp
      END IF
      buoy = buoy + g*dt*dzq(k)/t0(k)
      buoys(ki) = buoy !new
      IF (cape .LT. buoy) THEN
        cape = buoy
      ELSE
        cape = cape
      END IF
      capes(ki) = cape !new
      IF (dt .GE. 0.) THEN
        kt = k
        zt = z(k) + 0.5*dzq(k)
      ELSE IF (dt .LT. 0. .AND. dtm .GE. 0.) THEN
        IF (dt .GE. 0.) THEN
          abs1 = dt
        ELSE
          abs1 = -dt
        END IF
        IF (dtm .GE. 0.) THEN
          abs2 = dtm
        ELSE
          abs2 = -dtm
        END IF
! cloud top is level closest to parcel temperature
        IF (abs1 .LT. abs2) THEN
          kt = k
          zt = z(k) + 0.5*dzq(k)
        END IF
      END IF
      dtm = dt
! continue lifting until buoyancy is gone
      IF (buoy .LT. cincap) THEN
!         capped or cloud top
        EXIT loop_lift
      END IF
        IF (buoy .GT. 0.) ipos = 1
!         positive area detected
        IF (k .EQ. 1) THEN
          kt = k
          zt = z(k) + 0.5*dzq(k)
        END IF
    END DO loop_lift
    kt_s(ki) = kt !new
    kb_s(ki) = kb !new
!
!...CHECK FOR CLOUD
    IF (isat .EQ. 0) THEN
!       no cloud from lifting - no convection
      goto 500
      CYCLE loop_origin
    END IF
    IF (zt - zb .LE. dpthmin) THEN
!       not more than one cloud level - no convection
      goto 500
      CYCLE loop_origin
    ENDIF
    IF (ipos .EQ. 0) THEN
!       no buoyancy in cloud - no convection
      goto 500
      CYCLE loop_origin
    ENDIF
    IF (cape .LE. capemin) THEN
!       not enough cape
      goto 500
      CYCLE loop_origin
    ENDIF
!
!...IF CHECK FOR CLOUD SUCCESSFUL
!
!...DETRAINMENT
      dh = hp - hs(kt)
      dt = dh/cp/(1.+x(kt))
      dq = qs(kt) + dh/xlv*x(kt)/(1.+x(kt)) - qv0(kt)
!
!...SUBSIDENCE

!...RAINFALL
      rrkp = 0.
loop_rainfall:DO k=kt,1,-1
        rrk = rrkp + cond(k)
        rrkps(k) = rrkp !new
        rrkp = rrk
      END DO loop_rainfall
!
! adjoint start
!
      rrkpb = pprateb
      DO k=1,kt_s(ki)
        rrkb = rrkpb  !!!XIAO NOTE
        rrkpb=rrkb
        condb(k) = condb(k) + rrkb
      END DO
      rrkpb=0.
      mpb = 0.0  !examine later
      DO k=ki,kt-1,1
        temp9 = rhoe(k)*dzq(k)
        temp10b = dqvdtb(k)/temp9
        temp9b = -(mp*(qv0(k+1)-qv0(k))*temp10b/temp9)
        temp8 = rhoe(k)*dzq(k)
        temp9b0 = dthdtb(k)/temp8
        mpb = mpb + (th0(k+1)-th0(k))*temp9b0 + (qv0(k+1)-qv0(k))*&
&          temp10b
        qv0b(k+1) = qv0b(k+1) + mp*temp10b
        qv0b(k) = qv0b(k) - mp*temp10b
        temp8b0 = -(mp*(th0(k+1)-th0(k))*temp9b0/temp8)
        rhoeb(k) = rhoeb(k) + dzq(k)*temp8b0 + dzq(k)*temp9b
        dzqb(k) = dzqb(k) + rhoe(k)*temp8b0 + rhoe(k)*temp9b
        th0b(k+1) = th0b(k+1) + mp*temp9b0
        th0b(k) = th0b(k) - mp*temp9b0
      END DO
      temp7 = rhoe(kt)*dzq(kt)
      temp8b = dqvdtb(kt)/temp7
      temp7b = -(mp*dq*temp8b/temp7)
      mpb = mpb + dq*temp8b
      dqb = mp*temp8b
      rhoeb(kt) = rhoeb(kt) + dzq(kt)*temp7b
      dzqb(kt) = dzqb(kt) + rhoe(kt)*temp7b
      temp6 = t0(kt)*rhoe(kt)
      temp6b = dthdtb(kt)/(temp6*dzq(kt))
      temp6b0 = -(th0(kt)*mp*dt*temp6b/(temp6*dzq(kt)))
      th0b(kt) = th0b(kt) + mp*dt*temp6b
      mpb = mpb + th0(kt)*dt*temp6b
      dtb = th0(kt)*mp*temp6b
      t0b(kt) = t0b(kt) + dzq(kt)*rhoe(kt)*temp6b0
      rhoeb(kt) = rhoeb(kt) + dzq(kt)*t0(kt)*temp6b0
      dzqb(kt) = dzqb(kt) + temp6*temp6b0
      temp5 = xlv*(x(kt)+1.)
      temp5b = dqb/temp5
      qsb(kt) = qsb(kt) + dqb
      temp4 = cp*(x(kt)+1.)
      dhb = dtb/temp4 + x(kt)*temp5b
      xb(kt) = xb(kt) + (dh-dh*x(kt)*xlv/temp5)*temp5b - dh*cp*dtb/temp4&
&        **2
      qv0b(kt) = qv0b(kt) - dqb
      hpb = dhb
      hsb(kt) = hsb(kt) - dhb
500   continue
!...LIFT PARCEL
      hp = h(ki)
      qp = qv0(ki)
      mp = alpha*rhoe(ki)*dzq(ki)
loop_lift1:DO k=kte,ki+1,-1
!loop_lift1:DO k=ki+1,kte
      IF (tadps(k) .GE. tdps(k)) THEN
        condb(k) = 0.
      ELSE
        dh = hp - hs(k)
        qsp = qs(k) + dh/xlv*x(k)/(1.+x(k))
        qspb = qpb - mp*condb(k)
!Zhangl        mpb = mpb + (qp-qsp)*condb(k)
        mpb = mpb + (qps(k)-qsp)*condb(k)
        qpb = mp*condb(k)
        condb(k) = 0.0
        temp3 = xlv*(x(k)+1.)
        temp3b = qspb/temp3
        qsb(k) = qsb(k) + qspb
        dhb = x(k)*temp3b
        xb(k) = xb(k) + (dh-dh*x(k)*xlv/temp3)*temp3b
        hpb = hpb + dhb
        hsb(k) = hsb(k) - dhb
        dhb = 0.
        qp = qsp
      END IF
    END DO loop_lift1
!
    condb(kts:kte) = 0.0
    rhoeb(ki) = rhoeb(ki) + alpha*dzq(ki)*mpb
    dzqb(ki) = dzqb(ki) + alpha*rhoe(ki)*mpb
    mpb = 0.
    qv0b(ki) = qv0b(ki) + qpb
    qpb = 0.
    hb(ki) = hb(ki) + hpb
    hpb  = 0.
  END DO loop_origin

!!!!!!!!!!!!!!!!!!!!!!!!
  pprateb = 0.
  DO k=kte,kts,-1
    dqvdtb(k) = 0.0
    dthdtb(k) = 0.0
    temp1 = cp*rv*t0(k)**2
    temp1b = xlv**2*xb(k)/temp1
    qsb(k) = qsb(k) + xlv*hsb(k) + temp1b
    temp1b0 = ep2*qsb(k)/(p0(k)-ess(k))
    temp1b1 = -(ess(k)*temp1b0/(p0(k)-ess(k)))
    esb = temp1b0 - temp1b1
    temp0 = t0(k) - svp3
    temp = (t0(k)-svpt0)/temp0
    tempb = svp1*1000.*EXP(svp2*temp)*svp2*esb/temp0
    t0b(k) = t0b(k) + cp*hb(k) + (1.0-temp)*tempb + cp*hsb(k) - qs(k)*cp&
&      *rv*2*t0(k)*temp1b/temp1
    xb(k) = 0.0
    zb0(k) = zb0(k) + g*hb(k) + g*hsb(k)
    hsb(k) = 0.0
    p0b(k) = p0b(k) + temp1b1
    qsb(k) = 0.0
    qv0b(k) = qv0b(k) + xlv*hb(k)
    hb(k) = 0.0
  END DO
END SUBROUTINE DUCU1D_B

!  Differentiation of ducu in reverse (adjoint) mode:
!   gradient, with respect to input variables: th raincv t rthcuten
!                pcps qv z rqvcuten rho dz8w
!   of linear combination of output variables: raincv rthcuten
!                rqvcuten
! optionals
SUBROUTINE DUCU_B(ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms&
&  , kme, its, ite, jts, jte, kts, kte, dt, ktau, dx, rho, rhob, raincv&
&  , raincvb, nca, u, v, th, thb, t, tb, w, dz8w, dz8wb, z, zb, pcps, &
&  pcpsb, pi, w0avg, xlv, cp, rd, rv, g, ep2, svp1, svp2, svp3, svpt0, &
&  stepcu, cu_act_flag, warm_rain, cutop, cubot, qv, qvb, rthcuten, &
&  rthcutenb, rqvcuten, rqvcutenb)
  IMPLICIT NONE
!
  INTEGER, INTENT(IN) :: ime, ims, jme, jms, kme, kms
  LOGICAL :: cu_act_flag(ims:ime, jms:jme)
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: dz8w, pcps, &
&  pi, qv, rho, t, th, u, v, w, z
  INTEGER, INTENT(IN) :: ide, ids, ite, its, jde, jds, jte, jts, kde, &
&  kds, ktau, kte, kts, stepcu
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), OPTIONAL :: rqvcuten, &
&  rqvcutenb, rthcuten, rthcutenb
  REAL :: cubot(ims:ime, jms:jme), cutop(ims:ime, jms:jme), dz8wb(ims:&
&  ime, kms:kme, jms:jme), nca(ims:ime, jms:jme), pcpsb(ims:ime, kms:kme&
&  , jms:jme), qvb(ims:ime, kms:kme, jms:jme), raincv(ims:ime, jms:jme)&
&  , raincvb(ims:ime, jms:jme), rhob(ims:ime, kms:kme, jms:jme), tb(ims:&
&  ime, kms:kme, jms:jme), thb(ims:ime, kms:kme, jms:jme), w0avg(ims:ime&
&  , kms:kme, jms:jme), zb(ims:ime, kms:kme, jms:jme)
  LOGICAL, INTENT(IN) :: warm_rain
  REAL, INTENT(IN) :: cp, dt, dx, ep2, g, rd, rv, svp1, svp2, svp3, &
&  svpt0, xlv
  REAL :: dqvdt(kts:kte), dqvdtb(kts:kte), dthdt(kts:kte), dthdtb(kts:&
&  kte), dxsq, dz1d(kts:kte), dz1db(kts:kte), p1d(kts:kte), p1db(kts:kte&
&  ), pprate, pprateb, prs, qv1d(kts:kte), qv1db(kts:kte), rho1d(kts:kte&
&  ), rho1db(kts:kte), rhoe, rthcumax, scr1, t1d(kts:kte), t1db(kts:kte)&
&  , th1d(kts:kte), th1db(kts:kte), tmp, tst, tv, u1d(kts:kte), v1d(kts:&
&  kte), w0, w0avg1d(kts:kte), z1d(kts:kte), z1db(kts:kte)
  LOGICAL :: flag_qi, flag_qr, flag_qs, result1, result2
  INTEGER :: branch, i, i_end, i_start, icldck, j, j_end, j_start, k, &
&  ntst, sz
  INTRINSIC MOD, MAX, PRESENT, MIN
  EXTERNAL PRESENT_B
!-------------------------------------------------------------
!
!
!
! Optional arguments
!
!
! LOCAL VARS
!
! Initialize local variables
!
    qv1db(kts:kte) = 0.0
    dthdtb(kts:kte) = 0.0
    th1db(kts:kte) = 0.0
    rho1db(kts:kte) = 0.0
    p1db(kts:kte) = 0.0
    z1db(kts:kte) = 0.0
    dz1db(kts:kte) = 0.0
    t1db(kts:kte) = 0.0
    dqvdtb(kts:kte) = 0.0
    pprateb = 0.    !tested, this line no use, can be removed
!
  ntst = stepcu
  icldck = MOD(ktau, ntst)
  IF (icldck .EQ. 0 .OR. ktau .EQ. 1) THEN
!
!  Keep away from specified and relaxation zone (should be for just specified and nested bc)
    sz = 1
    IF (ids + sz .LT. its) THEN
      i_start = its
    ELSE
      i_start = ids + sz
    END IF
    IF (ide - 1 - sz .GT. ite) THEN
      i_end = ite
    ELSE
      i_end = ide - 1 - sz
    END IF
    IF (jds + sz .LT. jts) THEN
      j_start = jts
    ELSE
      j_start = jds + sz
    END IF
    IF (jde - 1 - sz .GT. jte) THEN
      j_end = jte
    ELSE
      j_end = jde - 1 - sz
    END IF
!
! Calculate basic state variables
!
    DO j=j_start,j_end
      DO i=i_start,i_end
        DO k=kts,kte
          dqvdt(k) = 0.
          dthdt(k) = 0.
        END DO
!
! assign vars from 3D to 1D
        DO k=kts,kte
          t1d(k) = t(i, k, j)
          th1d(k) = th(i, k, j)
          rho1d(k) = rho(i, k, j)
          qv1d(k) = qv(i, k, j)
          p1d(k) = pcps(i, k, j)
          dz1d(k) = dz8w(i, k, j)
          z1d(k) = z(i, k, j)
        END DO
        CALL DUCU1D(i, j, u1d, v1d, t1d, qv1d, p1d, dz1d, z1d, w0avg1d, &
&              dt, dx, dxsq, rho1d, th1d, xlv, cp, rd, rv, g, ep2, svp1&
&              , svp2, svp3, svpt0, dqvdt, dthdt, pprate, nca, ntst, &
&              cutop, cubot, ids, ide, jds, jde, kds, kde, ims, ime, jms&
&              , jme, kms, kme, its, ite, jts, jte, kts, kte)
        result1 = PRESENT(rthcuten)
        result2 = PRESENT(rqvcuten)
        IF (result1 .AND. result2) THEN
          DO k=kts,kte
            rthcuten(i, k, j) = dthdt(k)
            rqvcuten(i, k, j) = dqvdt(k)
          END DO
        ELSE
        END IF
!
! Start adjoint
!
        IF (result1 .AND. result2) THEN
          pprateb = dt*raincvb(i, j)
          raincvb(i, j) = 0.0
          DO k=kte,kts,-1
            dqvdtb(k) = dqvdtb(k) + rqvcutenb(i, k, j)
            rqvcutenb(i, k, j) = 0.0
            dthdtb(k) = dthdtb(k) + rthcutenb(i, k, j)
            rthcutenb(i, k, j) = 0.0
          END DO
        ELSE
          pprateb = 0.0
        END IF
        CALL DUCU1D_B(i, j, u1d, v1d, t1d, t1db, qv1d, qv1db, p1d, p1db&
&                , dz1d, dz1db, z1d, z1db, w0avg1d, dt, dx, dxsq, rho1d&
&                , rho1db, th1d, th1db, xlv, cp, rd, rv, g, ep2, svp1, &
&                svp2, svp3, svpt0, dqvdt, dqvdtb, dthdt, dthdtb, pprate&
&                , pprateb, nca, ntst, cutop, cubot, ids, ide, jds, jde&
&                , kds, kde, ims, ime, jms, jme, kms, kme, its, ite, jts&
&                , jte, kts, kte)
        DO k=kte,kts,-1
          zb(i, k, j) = zb(i, k, j) + z1db(k)
          z1db(k) = 0.0
          dz8wb(i, k, j) = dz8wb(i, k, j) + dz1db(k)
          dz1db(k) = 0.0
          pcpsb(i, k, j) = pcpsb(i, k, j) + p1db(k)
          p1db(k) = 0.0
          qvb(i, k, j) = qvb(i, k, j) + qv1db(k)
          qv1db(k) = 0.0
          rhob(i, k, j) = rhob(i, k, j) + rho1db(k)
          rho1db(k) = 0.0
          thb(i, k, j) = thb(i, k, j) + th1db(k)
          th1db(k) = 0.0
          tb(i, k, j) = tb(i, k, j) + t1db(k)
          t1db(k) = 0.0
        END DO
        raincvb(i, j) = 0.0
        DO k=kte,kts,-1
          dthdtb(k) = 0.0
          dqvdtb(k) = 0.0
        END DO
      END DO
    END DO
        qv1db(kts:kte) = 0.0
        dthdtb(kts:kte) = 0.0
        th1db(kts:kte) = 0.0
        rho1db(kts:kte) = 0.0
        p1db(kts:kte) = 0.0
        z1db(kts:kte) = 0.0
        dz1db(kts:kte) = 0.0
        t1db(kts:kte) = 0.0
        dqvdtb(kts:kte) = 0.0
        rqvcutenb(ims:ime, kms:kme, jms:jme) = 0.0
        rthcutenb(ims:ime, kms:kme, jms:jme) = 0.0
        raincvb(ims:ime, jms:jme) = 0.0
  ELSE
    raincvb(ims:ime, jms:jme) = 0.0
    rthcutenb(ims:ime, kms:kme, jms:jme) = 0.0
    rqvcutenb(ims:ime, kms:kme, jms:jme) = 0.0
  END IF
END SUBROUTINE DUCU_B

END MODULE module_cu_du

