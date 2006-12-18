subroutine Interp_lin_2D(fm2d, is, ie, js, je, &
                         i, j, dx, dy, dxm, dym, &
                         fo2d)
  implicit none

  integer,                intent(in)  :: is, ie, js, je
  integer,                intent(in)  :: i, j
  real   ,                intent(in)  :: dx, dxm, dy, dym
  real   ,                intent(in)  :: fm2d(is:ie,js:je) ! Input variable
  real   ,                intent(out) :: fo2d  ! Output variable 

  fo2d   = dym*(dxm*fm2d(i,j  ) + dx*fm2d(i+1,j  )) &
         + dy *(dxm*fm2d(i,j+1) + dx*fm2d(i+1,j+1))

end subroutine Interp_lin_2D

