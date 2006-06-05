subroutine da_change_date( ccyy, mm, dd, delta )

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer, dimension(12) :: mmday

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   mmday(2) = 28

   if (mod(ccyy,4) == 0) then
      mmday(2) = 29

      if ( mod(ccyy,100) == 0) then
         mmday(2) = 28
      endif

      if(mod(ccyy,400) == 0) then
         mmday(2) = 29
      end if
   endif

   dd = dd + delta

   if(dd == 0) then
      mm = mm - 1

      if(mm == 0) then
         mm = 12
         ccyy = ccyy - 1
      endif

      dd = mmday(mm)
   elseif ( dd .gt. mmday(mm) ) then
      dd = 1
      mm = mm + 1
      if(mm > 12 ) then
         mm = 1
         ccyy = ccyy + 1
      end if
   end if
end subroutine da_change_date
