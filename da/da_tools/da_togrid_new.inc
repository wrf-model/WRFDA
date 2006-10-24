subroutine da_togrid_new (x, ib, ie, i, dx, dxm)
   
   ! Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1

   implicit none

   real,                     intent(in)  :: x(:,:)
   integer,                  intent(in)  :: ib, ie
   real,                     intent(out) :: dx(:,:), dxm(:,:)
   integer,                  intent(out) :: i(:,:)
   
   i(:,:) = int (x(:,:))

   where(i(:,:) < ib)  i(:,:) = ib
   where(i(:,:) >= ie) i(:,:) = ie-1
 
   dx(:,:) = x(:,:) - real(i(:,:))
   dxm(:,:)= 1.0 - dx(:,:)

end subroutine da_togrid_new

