subroutine da_togrid_new (x, ib, ie, i, dx, dxm)
   
   ! Transfer obs. x to grid i and calculate its
   ! distance to grid i and i+1

   implicit none

   real,                     intent(in)  :: x(:,:)
   integer,                  intent(in)  :: ib, ie
   real,                     intent(out) :: dx(:,:), dxm(:,:)
   integer,                  intent(out) :: i(:,:)

   if (trace_use) call da_trace_entry("da_togrid_new")
   
   i(:,:) = int (x(:,:))

   where(i(:,:) < ib)  i(:,:) = ib
   where(i(:,:) >= ie) i(:,:) = ie-1
 
   dx(:,:) = x(:,:) - real(i(:,:))
   dxm(:,:)= 1.0 - dx(:,:)

   if (trace_use) call da_trace_exit("da_togrid_new")

end subroutine da_togrid_new

