SUBROUTINE da_accre_lin(DT,SCR2,SCR3,SCR7,PRAC,BVT3,PRA,  &
                  SCR29,SCR79,PRA9,kts,kte)

!  Accretion (Collection)

   implicit none

   integer                   :: kts, kte, k
   real, dimension(kts:kte)  :: DT,SCR2,SCR3,SCR7
   real, dimension(kts:kte)  :: PRA
   real, dimension(kts:kte)  :: SCR29,SCR79,PRA9
   real                      :: PRAC,BVT3

   DO K=kts, kte

      if (DT(k) <= 0.) cycle

      if (SCR3(k) > 0.) then
         PRA9(K) = PRAC*BVT3*SCR7(K)**(BVT3-1.)*SCR2(K)*SCR79(K)  &
                      + PRAC*SCR7(K)**BVT3*SCR29(K)
         PRA(k) = PRAC*SCR7(K)**BVT3*SCR2(K)
      else
         PRA9(K) = 0.
         PRA(k) = 0.
      endif

   ENDDO

END SUBROUTINE da_accre_lin
