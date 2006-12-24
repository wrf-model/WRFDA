subroutine da_accre_lin(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,  &
                  SCR29,SCR79,PRA9,kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Accretion (Collection)
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                   :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT,SCR2,SCR3,SCR7
   real, dimension(kts:kte), intent(out) :: PRA
   real, dimension(kts:kte), intent(in)  :: SCR29,SCR79
   real, dimension(kts:kte), intent(out) :: PRA9
   real, intent(in)                      :: PRAC,BVT3

   integer                   :: k

   do K=kts, kte
      if (DT(k) <= 0.) cycle

      if (SCR3(k) > 0.) then
         PRA9(K) = PRAC*BVT3*SCR7(K)**(BVT3-1.)*SCR2(K)*SCR79(K)  &
                      + PRAC*SCR7(K)**BVT3*SCR29(K)
         PRA(k) = PRAC*SCR7(K)**BVT3*SCR2(K)
      else
         PRA9(K) = 0.
         PRA(k) = 0.
      end if
   end do

end subroutine da_accre_lin


