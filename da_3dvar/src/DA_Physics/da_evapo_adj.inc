SUBROUTINE da_evapo_adj(DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV_B,  &
                 PREC1,PREC2,PRE,  &
                 SCR39,SCR59,SCR79,DUM1119,DUM419,QV_A,PRE9,kts,kte)

!  Rainwater evaporation

   implicit none

   integer                   :: kts, kte, k
   real                      :: PREC1, PREC2
   real, dimension(kts:kte)  :: DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV_B
   real, dimension(kts:kte)  :: PRE
   real, dimension(kts:kte)  :: TMP
   real, dimension(kts:kte)  :: SCR39,SCR59,SCR79,DUM1119,DUM419,QV_A
   real, dimension(kts:kte)  :: PRE9

   DO K=kts, kte

      if (DT(k) <= 0.) cycle

      if (SCR3(K) > 0.) then
         PRE(k)=(QV_B(K)/SCR5(K)-1.)*(PREC1*SCR7(K)*SCR7(K)+   &
                 PREC2*DUM111(K)*SCR7(K)**BVT2)/DUM41(K)
      else
         PRE(k)=0.
      endif
   ENDDO

   DO K=kts, kte

      if (DT(k) <= 0.) cycle
 
      SCR39(K)=0.
      IF(PRE(K) < -SCR3(K)/DT(K) ) THEN
         SCR39(K)=-PRE9(K)/DT(K)
         PRE9(K)=0.
      ENDIF

      SCR59(K)=0.
      SCR79(K)=0.
      DUM419(K)=0.
      DUM1119(K)=0.
      if (SCR3(K) > 0.) then
         TMP(K)=(PREC1*SCR7(K)*SCR7(K)+  &
                   PREC2*DUM111(K)*SCR7(K)**BVT2)/DUM41(K)
         QV_A(K)=QV_A(K)+TMP(K)/SCR5(K)*PRE9(K)
         SCR59(K)=-TMP(K)*QV_B(K)/SCR5(K)**2*PRE9(K)
         SCR79(K)=(QV_B(K)/SCR5(K)-1.)/DUM41(K)*(2.*PREC1*SCR7(K)+  &
                  PREC2*DUM111(K)*BVT2*SCR7(K)**(BVT2-1.))*PRE9(K)
         DUM1119(K)=(QV_B(K)/SCR5(K)-1.)/DUM41(K)*  &
                   PREC2*SCR7(K)**BVT2*PRE9(K)
         DUM419(K)=-(QV_B(K)/SCR5(K)-1.)/DUM41(K)*TMP(K)*PRE9(K)
      endif

   ENDDO

END SUBROUTINE da_evapo_adj
