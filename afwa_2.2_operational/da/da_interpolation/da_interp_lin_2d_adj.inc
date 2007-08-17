subroutine da_interp_lin_2d_adj(fm2d, is, ie, js, je,&
                             i, j, dx, dy, dxm, dym, &
                             fo2d)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,            intent(in)     :: is, ie, js, je
   integer,            intent(in)     :: i, j
   real   ,            intent(in)     :: dx, dxm, dy, dym
   real   ,            intent(in)     :: fo2d   

   real   ,            intent(inout)  :: fm2d(is:ie,js:je) ! Input variable

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d_adj")

   fm2d(i  ,j ) = dym*dxm* fo2d + fm2d(i  ,j )
   fm2d(i+1,j ) = dym*dx * fo2d + fm2d(i+1,j )
   fm2d(i  ,j+1) = dy *dxm* fo2d + fm2d(i  ,j+1)
   fm2d(i+1,j+1) = dy *dx * fo2d + fm2d(i+1,j+1)

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d_adj")

end subroutine da_interp_lin_2d_adj


