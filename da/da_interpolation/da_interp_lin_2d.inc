subroutine da_interp_lin_2d(fm2d, is, ie, js, je, &
                         i, j, dx, dy, dxm, dym, &
                         fo2d)

   implicit none

   integer,                intent(in)  :: is, ie, js, je
   integer,                intent(in)  :: i, j
   real   ,                intent(in)  :: dx, dxm, dy, dym
   real   ,                intent(in)  :: fm2d(is:ie,js:je) ! Input variable
   real   ,                intent(out) :: fo2d  ! Output variable 

   if (trace_use_frequent) call da_trace_entry("da_interp_lin_2d")

   fo2d   = dym*(dxm*fm2d(i,j  ) + dx*fm2d(i+1,j  )) &
          + dy *(dxm*fm2d(i,j+1) + dx*fm2d(i+1,j+1))

   if (trace_use_frequent) call da_trace_exit("da_interp_lin_2d")

end subroutine da_interp_lin_2d

