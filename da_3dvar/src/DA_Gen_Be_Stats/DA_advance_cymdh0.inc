Subroutine DA_advance_cymdh0 ( ccyy, mm, dd, hh, delta )

!-------------------------------------------------------------------------
!  HISTORY: 09/01/2004                                       Mi-Seon Lee
!
!  Purpose: To advance the time.
!
!  Input : ccyy, mm, dd, hh, (old) and delta
!  Output: ccyy, mm, dd, hh  (new)
!                                       Y.-R. Guo  02/17/2005
!-------------------------------------------------------------------------

   implicit none

   integer :: ccyy, mm, dd, hh, delta, index, sign, i, j

   hh = hh + delta
  
   do while ( hh < 0 )
      hh = hh + 24
      CALL DA_change_date ( ccyy, mm, dd, -1 )
   end do
   
   do while ( hh > 23 )
      hh = hh - 24
      CALL DA_change_date ( ccyy, mm, dd, 1 )
   end do

   if ((hh < 0) .OR. (hh > 23)) then
      print *, 'hh=', hh
      print *, 'This is incredible. Let us stop here.'
      stop
   endif
   
END Subroutine DA_advance_cymdh0
