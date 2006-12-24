subroutine da_wdt(H,W,TERR,DT,kts,kte)

   !----------------------------------------------------------------------
   ! Purpose: Calculate DT
   !----------------------------------------------------------------------

   implicit none

   integer,intent(in)                    :: kts, kte
   real, dimension(kts:kte),intent(in)   :: h
   real, dimension(kts:kte),intent(out)  :: dt
   real, dimension(kts:kte+1),intent(in) :: w
   real,intent(in)                       :: terr

   integer                     :: k

   do k=kte,kts+1,-1
      if (w(k) >= 0.1) then
         dt(k)=(h(k)-h(k-1))/w(k)
      else
         dt(k)=0.
      end if
   end do

   if (w(kts) >= 0.1) then
      dt(kts)=(h(kts)-terr)/w(kts)
   else
      dt(kts)=0.0
   end if

end subroutine da_wdt


