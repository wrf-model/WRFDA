SUBROUTINE da_accre(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,kts,kte)

!  Accretion (Collection)

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT,SCR2,SCR3,SCR7
   real, dimension(kts:kte)  :: PRA
   real                      :: PRAC,BVT3

   DO K=kts, kte

      if (DT(k) <= 0.) cycle

      if (SCR3(k) > 0.) then
         PRA(k) = PRAC*SCR7(K)**BVT3*SCR2(K)
      else
         PRA(k) = 0.
      endif

   ENDDO

END SUBROUTINE da_accre

