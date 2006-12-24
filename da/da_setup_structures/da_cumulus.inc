subroutine da_cumulus (ZCB, TCB, QCB, PCB, PK, TE,                &
                     Z, T, Q, LCB, LCT, PCT, ZCT, kts, kte)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                      :: kts, kte
   real, intent(inout)                      :: ZCB, TCB, QCB, PCB
   real, intent(in),   dimension(kts:kte)   :: PK, TE
   real, intent(out),  dimension(kts:kte)   :: Z, T, Q
   integer, intent(out)                     :: LCB, LCT
   real, intent(out)                        :: PCT, ZCT

   integer   :: k, IA, L, NCB
   real      :: G, CP, R, HL, EM, ET, P
   real      :: TLL, QLL, PLL, ZLL, TBAR, PBAR, QBAR
   real      :: DP, DZ, DDT, DT

   G=9.81
   CP=1004.
   R=2000./7.
   HL=2.49E06
   DT=0.1
   IA=1000

   do k = kts, kte
      Z(k) = 0.0
      T(k) = 0.0
      Q(k) = 0.0
   end do

   EM=G*ZCB+CP*TCB+HL*QCB

   NCB=kts

   if (PK(kte) > PCB) then
      NCB=kte
   end if

   do L=kte-1,kts,-1
      if (PK(L) > PCB) then
         NCB=L+1
         exit
      end if
   end do

   do L=NCB,kte
      P=PK(L)
      do K=1,IA
         if (L == NCB) then
            TLL=TCB
            QLL=QCB
            PLL=PCB
            ZLL=ZCB
         else
            TLL=T(L-1)
            QLL=Q(L-1)
            PLL=PK(L-1)
            ZLL=Z(L-1)
         end if

         T(L)=TLL-(K*DT)

         call da_qfrmrh(P, T(L), 100., Q(L))

         TBAR=0.5*(T(L)+TLL)
         QBAR=0.5*(Q(L)+QLL)
         PBAR=0.5*(P+PLL)
         DP=PLL-P
         DZ=(R*TBAR*(1.+0.61*QBAR)*DP)/(G*PBAR)
         Z(L)=ZLL+DZ
         ET=G*Z(L)+CP*T(L)+HL*Q(L)
         if ((ET-EM) <= 0.) exit
      end do
   end do

   LCT=NCB

   do K=kte,NCB+1,-1
      DDT=T(K)-TE(K)

      if (DDT >= 0.) then
         LCT=K
         exit
      end if
   end do

   LCB=LCT

   do K=NCB,kte
      DDT=T(K)-TE(K)
      if (DDT >= 0.) then
         LCB=K
         exit
      end if
   end do

   PCT=PK(LCT)
   ZCT=Z(LCT)
   PCB=PK(LCB)
   ZCB=Z(LCB)

end subroutine da_cumulus


