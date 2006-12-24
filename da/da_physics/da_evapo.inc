subroutine da_evapo(DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV,  &
                 PREC1,PREC2,PRE,kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Rainwater evaporation
   !-----------------------------------------------------------------------

   implicit none

   integer,intent(in)                   :: kts, kte
   real,intent(in)                      :: PREC1, PREC2
   real, dimension(kts:kte),intent(in)  :: DT,SCR3,SCR5,SCR7,DUM111,DUM41,QV
   real, dimension(kts:kte),intent(out)  :: PRE

   integer                   :: k

   do K=kts, kte
      if (DT(k) <= 0.) cycle

      if (SCR3(K) > 0.) then
         PRE(k)=(QV(K)/SCR5(K)-1.)*(PREC1*SCR7(K)*SCR7(K)+   &
                 PREC2*DUM111(K)*SCR7(K)**BVT2)/DUM41(K)
      else
         PRE(k)=0.
      end if

      PRE(k)=AMAX1(-SCR3(k)/DT(k),PRE(k))
   end do

end subroutine da_evapo


