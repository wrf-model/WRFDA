SUBROUTINE da_wdt(H,W,TERR,DT,kts,kte)

!  Calculate DT

   implicit none

   integer                      :: kts, kte, k
   real, dimension(kts:kte)     :: H, DT
   real, dimension(kts:kte+1)   :: W
   real                         :: TERR

   DO k=kte,kts+1,-1

      if (W(k) >= 0.1) then
         DT(k)=(H(k)-H(k-1))/W(k)
      else
         DT(k)=0.
      endif

   ENDDO

   if (W(kts) >= 0.1) then
      DT(kts)=(H(kts)-TERR)/W(kts)
   else
      DT(kts)=0.
   endif


END SUBROUTINE da_wdt

