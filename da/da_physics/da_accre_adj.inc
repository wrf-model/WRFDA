subroutine da_accre_adj(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,  &
                  SCR29,SCR79,PRA9,kts,kte)

   ! Accretion (Collection)

   implicit none

   integer, intent(in)       :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT,SCR2,SCR3,SCR7
   real, dimension(kts:kte), intent(in)  :: PRA
   real, dimension(kts:kte), intent(out) :: SCR29,SCR79
   real, dimension(kts:kte), intent(in)  :: PRA9
   real, intent(in)                      :: PRAC,BVT3

   integer       :: k

   do K=kts, kte
      if (DT(k) <= 0.) cycle

      if (SCR3(k) > 0.) then
         SCR79(K)=SCR79(K)+PRAC*BVT3*SCR7(K)**(BVT3-1.)*SCR2(K)*PRA9(K)
         SCR29(K)=SCR29(K)+ PRAC*SCR7(K)**BVT3*PRA9(K)
      end if

   end do

end subroutine da_accre_adj


