subroutine da_interp_lin_2d_adj_new(fm2d, is, ie, js, je,&
                             i, j, dx, dy, dxm, dym, &
                             fo2d,num)


   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none

   integer,            intent(in)     :: num
   integer,            intent(in)     :: is, ie, js, je
   integer,            intent(in)     :: i(num), j(num)
   real   ,            intent(in)     :: dx(num), dxm(num), dy(num), dym(num)
   real   ,            intent(in)     :: fo2d(num)

   real   ,            intent(inout)  :: fm2d(is:ie,js:je) ! Input variable

   integer :: point

   if (trace_use) call da_trace_entry("da_interp_lin_2d_adj_new")

   do point=1,num
      fm2d(i(point)  ,j(point) ) = dym(point)*dxm(point)* fo2d(point) + fm2d(i(point)  ,j(point) )
      fm2d(i(point)+1,j(point) ) = dym(point)*dx(point) * fo2d(point) + fm2d(i(point)+1,j(point) )
      fm2d(i(point)  ,j(point)+1) = dy(point) *dxm(point)* fo2d(point) + fm2d(i(point)  ,j(point)+1)
      fm2d(i(point)+1,j(point)+1) = dy(point) *dx(point) * fo2d(point) + fm2d(i(point)+1,j(point)+1)
   end do

   if (trace_use) call da_trace_exit("da_interp_lin_2d_adj_new")

end subroutine da_interp_lin_2d_adj_new


