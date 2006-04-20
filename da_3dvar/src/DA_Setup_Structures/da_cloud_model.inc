SUBROUTINE da_cloud_model ( TB, PB, QB, QCWB, QRNB, ZB, ZFB, DT, kts, kte )

!-----------------------------------------------------------------
! PURPOSE: Calculate DT (=dz/w) using cumulus parameterization 
!          of a one-dimensional cloud model.
!
! HISTORY: August 2004 of F90 version.  Jianfeng Gu
!-----------------------------------------------------------------

!  Calculate DT

   implicit none

   integer, intent(IN)                     :: kts, kte
   real, intent(IN),  dimension(kts:kte)   :: TB, PB, QB, QCWB, QRNB, ZB
   real, intent(IN),  dimension(kts:kte+1) :: ZFB
   real, intent(OUT), dimension(kts:kte)   :: DT

   integer                    :: k
   real                       :: P0, Z0, T0, Q0
   real                       :: PLCL, ZLCL, TLCL, QLCL
   integer                    :: KCB, KCT
   real                       :: PCT, ZCT
   real, dimension(kts:kte)   :: ZC, TC, QC, PP, QT
   real, dimension(kts:kte)   :: TCV, TBV, B
   real                       :: G, ALPHA, RC, MU, XX, YY
   real, dimension(kts:kte+1) :: W0, W

   IF (trace_use) CALL da_trace_entry("da_cloud_model")

   G=9.81
   ALPHA=0.5
   RC=100.0
   MU=0.183/RC

   DO k = kts, kte+1
     W0(k)=0.0  
     W(k)=0.0  
   ENDDO

   DO k = kts, kte
     PP(k)=PB(k)/100.0
     DT(k)=0.0
   ENDDO

   P0 = PP(kts)
   Z0 = ZB(kts)
   T0 = MAX(TB(kts),303.)

   CALL QFRMRH ( P0, T0, 95., Q0 )

   CALL LCL ( P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL )

   CALL QFRMRH ( PLCL, TLCL, 95., QLCL )

   CALL CUMULUS ( ZLCL, TLCL, QLCL, PLCL, PP, TB,            &
                  ZC, TC, QC, KCB, KCT, PCT, ZCT, kts, kte )

   DO k = KCB, KCT
   
     TCV(k) = TC(k) * ( 1. + 0.608 * QC(k) )
     TBV(k) = TB(k) * ( 1. + 0.608 * QB(k) )
   
     B(k) = ( TCV(k)-TBV(k) ) / TBV(k)

     QT(k) = QC(k) + QCWB(k) + QRNB(k)

   ENDDO

   W0(KCB) = 0.0
   DO k = KCB+1, KCT+1
        XX = 1.0+2.0*MU*(ZFB(k)-ZFB(k-1))
        YY = 2.0*G*( B(k-1)/(1.+ALPHA) - QT(k-1) ) * ( ZFB(k)-ZFB(k-1) )
     W0(k) =  ( W0(k-1)+YY ) / XX
   ENDDO
     
   DO k = KCB, KCT+1
     IF ( W0(k) >= 0. ) THEN
       W(k) = SQRT( W0(k) )
     ENDIF
   ENDDO


   DO k = KCT, KCB+1, -1
     IF ( W(k) >= 0.01 ) THEN
       DT(k) = (ZB(k)-ZB(k-1))/W(k)
     ELSE
       DT(k) = 0.0
     ENDIF
   ENDDO

   IF (trace_use) CALL da_trace_exit("da_cloud_model")



END SUBROUTINE da_cloud_model



SUBROUTINE LCL( P0, Z0, T0, Q0, PLCL, ZLCL, TLCL, QLCL )

   implicit none

   real, intent(IN)   :: P0, Z0, T0, Q0
   real, intent(OUT)  :: PLCL, ZLCL, TLCL, QLCL

   real   :: DP, QS, EPS

    DP=5.
    PLCL=300.

  reports: &
  DO

    TLCL=T0*((PLCL/P0)**0.286)

    CALL QFRMRH( PLCL, TLCL, 100., QS )

    EPS=QS-Q0

    IF( EPS >= 0. ) THEN

      ZLCL=(1004./9.81)*(T0-TLCL)+Z0
      QLCL=QS
      RETURN

    ELSE

      PLCL=PLCL+DP

      IF ( PLCL >= P0 ) THEN
        ZLCL=Z0
        QLCL=Q0
        PLCL=P0
        RETURN
      END IF

    END IF

  ENDDO reports


END SUBROUTINE LCL



SUBROUTINE CUMULUS ( ZCB, TCB, QCB, PCB, PK, TE,                &
                     Z, T, Q, LCB, LCT, PCT, ZCT, kts, kte )

   implicit none

   integer, intent(IN)                      :: kts, kte
   real, intent(INOUT)                      :: ZCB, TCB, QCB, PCB
   real, intent(IN),   dimension(kts:kte)   :: PK, TE
   real, intent(OUT),  dimension(kts:kte)   :: Z, T, Q
   integer, intent(OUT)                     :: LCB, LCT
   real, intent(OUT)                        :: PCT, ZCT

   integer   :: k, IA, L, NCB
   real      :: G, CP, R, HL, A, B, EM, ET, P
   real      :: TLL, QLL, PLL, ZLL, TBAR, PBAR, QBAR
   real      :: DP, DZ, DDT, DT

      G=9.81
      CP=1004.
      R=2000./7.
      HL=2.49E06
      A=17.27
      B=35.86
      DT=0.1
      IA=1000

   DO k = kts, kte
     Z(k) = 0.0
     T(k) = 0.0
     Q(k) = 0.0
   ENDDO

      EM=G*ZCB+CP*TCB+HL*QCB

      NCB=kts

      IF ( PK(kte) > PCB ) THEN
        NCB=kte
      END IF

    DO L=kte-1,kts,-1

      IF ( PK(L) > PCB ) THEN
        NCB=L+1
        EXIT
      END IF

    ENDDO


   DO L=NCB,kte

      P=PK(L)

    DO K=1,IA

     IF ( L == NCB ) THEN

      TLL=TCB
      QLL=QCB
      PLL=PCB
      ZLL=ZCB

      ELSE

      TLL=T(L-1)
      QLL=Q(L-1)
      PLL=PK(L-1)
      ZLL=Z(L-1)

     ENDIF

      T(L)=TLL-(K*DT)

      CALL QFRMRH( P, T(L), 100., Q(L) )

      TBAR=0.5*(T(L)+TLL)
      QBAR=0.5*(Q(L)+QLL)
      PBAR=0.5*(P+PLL)
      DP=PLL-P
      DZ=(R*TBAR*(1.+0.61*QBAR)*DP)/(G*PBAR)
      Z(L)=ZLL+DZ
      ET=G*Z(L)+CP*T(L)+HL*Q(L)
      IF( (ET-EM) <= 0. ) EXIT

    ENDDO

   ENDDO

      LCT=NCB

    DO K=kte,NCB+1,-1

      DDT=T(K)-TE(K)

      IF ( DDT >= 0. ) THEN
        LCT=K
        EXIT
      END IF

    ENDDO

      LCB=LCT

    DO K=NCB,kte

      DDT=T(K)-TE(K)

      IF ( DDT >= 0. ) THEN
        LCB=K
        EXIT
      END IF

    ENDDO

      PCT=PK(LCT)
      ZCT=Z(LCT)
      PCB=PK(LCB)
      ZCB=Z(LCB)

END SUBROUTINE CUMULUS



SUBROUTINE QFRMRH ( P, T, RH, Q )

   implicit none

   real, intent(IN)    :: P, T, RH
   real, intent(OUT)   :: Q

   real   :: A, B, E, QS

      A=17.26
      B=35.86
      IF(T <= 263.) A=21.87
      IF(T <= 263.) B= 7.66
      E=6.11*EXP(A*(T-273.16)/(T-B))
      QS=0.622*E/(P-0.378*E)
      Q=QS*RH/100.

END SUBROUTINE QFRMRH
