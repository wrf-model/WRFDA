subroutine da_accre(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: Accretion (Collection)
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)       :: kts, kte
   real, dimension(kts:kte), intent(in)  :: DT,SCR2,SCR3,SCR7
   real, dimension(kts:kte), intent(out) :: PRA

   real, intent(in)                      :: PRAC,BVT3

   integer       :: k

   do K=kts, kte
      if (DT(k) <= 0.) cycle

      if (SCR3(k) > 0.) then
         PRA(k) = PRAC*SCR7(K)**BVT3*SCR2(K)
      else
         PRA(k) = 0.
      end if
   end do

end subroutine da_accre


