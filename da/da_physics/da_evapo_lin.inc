subroutine da_evapo_lin(DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV_B,  &
                 PREC1,PREC2,PRE,  &
                 SCR39,SCR59,SCR79,DUM1119,DUM419,QV_A,PRE9,kts,kte)

   ! Rainwater evaporation

   implicit none

   integer, intent(in)                   :: kts, kte
   real, intent(in)                      :: PREC1, PREC2
   real, dimension(kts:kte), intent(in)  :: DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV_B
   real, dimension(kts:kte), intent(out)  :: PRE
   real, dimension(kts:kte), intent(in)  :: SCR39,SCR59,SCR79,DUM1119,DUM419,QV_A
   real, dimension(kts:kte), intent(out) :: PRE9

   integer                   :: k
   real, dimension(kts:kte)  :: TMP

   do K=kts, kte
      if (DT(k) <= 0.) cycle

      if (SCR3(K) > 0.) then
         TMP(K)=(PREC1*SCR7(K)*SCR7(K)+  &
                   PREC2*DUM111(K)*SCR7(K)**BVT2)/DUM41(K)
         PRE9(K)=TMP(K)/SCR5(K)*QV_A(K)  &
                   -TMP(K)*QV_B(K)/SCR5(K)**2*SCR59(K)  &
                   +(QV_B(k)/SCR5(K)-1.)/DUM41(K)*(2.*PREC1*SCR7(K)+  &
                      PREC2*DUM111(K)*BVT2*SCR7(K)**(BVT2-1.))*SCR79(K)  &
                   +(QV_B(k)/SCR5(K)-1.)/DUM41(K)*  &
                      PREC2*SCR7(K)**BVT2*DUM1119(K)  &
                   -(QV_B(k)/SCR5(K)-1.)/DUM41(K)*TMP(K)*DUM419(K)
         PRE(k)=(QV_B(K)/SCR5(K)-1.)*(PREC1*SCR7(K)*SCR7(K)+   &
                 PREC2*DUM111(K)*SCR7(K)**BVT2)/DUM41(K)
      else
         PRE9(K) = 0.
         PRE(k)=0.
      end if

      if (PRE(K) < -SCR3(K)/DT(K) ) then
         PRE9(K)=-SCR39(K)/DT(K)
         PRE(k)=-SCR3(K)/DT(K)
      end if

   end do

end subroutine da_evapo_lin


