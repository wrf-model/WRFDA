subroutine toGrid (x, ib, ie, i, dx, dxm)
   
!  Transfer obs. x to grid i and calculate its
!  distance to grid i and i+1

   implicit none

   real,                     intent(in)  :: x
   integer,                  intent(in)  :: ib, ie
   real,                     intent(out) :: dx, dxm
   integer,                  intent(out) :: i
   
   i = int (x)

   if (i < ib) then
      i = ib
   else if (i >= ie) then
      i = ie - 1
   endif

   dx = x - real(i)
   dxm= 1.0 - dx

end subroutine toGrid

