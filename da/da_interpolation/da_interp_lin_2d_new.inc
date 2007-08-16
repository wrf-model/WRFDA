subroutine da_interp_lin_2d_new(fm2d, is, ie, js, je, i, j, dx, dy, dxm, dym, fo2d, num)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: num
   integer, intent(in)  :: is, ie, js, je
   integer, intent(in)  :: i(num), j(num)
   real   , intent(in)  :: dx(num), dxm(num), dy(num), dym(num)
   real   , intent(in)  :: fm2d(is:ie,js:je) ! Input variable
   real   , intent(out) :: fo2d(num)  ! Output variable 
   
   integer :: point

   if (trace_use) call da_trace_entry("da_interp_lin_2d_new")

   do point =1,num
      fo2d(point) = dym(point)*(dxm(point)*fm2d(i(point),j(point)) + dx(point)*fm2d(i(point)+1,j(point))) &
         + dy(point) *(dxm(point)*fm2d(i(point),j(point)+1) + dx(point)*fm2d(i(point)+1,j(point)+1))
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_2d_new")

end subroutine da_interp_lin_2d_new


