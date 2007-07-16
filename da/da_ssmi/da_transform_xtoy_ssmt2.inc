subroutine da_transform_xtoy_ssmt2 (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer                      :: n        ! Loop counter.
   integer                      :: i, j, k  ! Index dimension.
   real                         :: dx, dxm  ! 
   real                         :: dy, dym  !
   

   if (iv%num_ssmt2 > 0) then
      do n=1, iv % num_ssmt2
         y%ssmt2(n)%rh(:) = 0.0

         k = iv % ssmt2(n) % info % levels

         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt2(n)%loc%i
         dy = iv%ssmt2(n)%loc%dy
         dym = iv%ssmt2(n)%loc%dym
         j = iv%ssmt2(n)%loc%j
         dx = iv%ssmt2(n)%loc%dx
         dxm = iv%ssmt2(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
         call da_interp_lin_3d(grid%xa % rh, i, j, dx, dy, dxm, dym, &
                            y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
       end do
   end if

end subroutine da_transform_xtoy_ssmt2


