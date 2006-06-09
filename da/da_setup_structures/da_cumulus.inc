SUBROUTINE da_cumulus ( ZCB, TCB, QCB, PCB, PK, TE,                &
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

      CALL da_qfrmrh( P, T(L), 100., Q(L) )

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

END SUBROUTINE da_cumulus
