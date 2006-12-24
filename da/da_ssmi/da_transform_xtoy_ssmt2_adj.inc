subroutine da_transform_xtoy_ssmt2_adj(iv, xp, jo_grad_y, jo_grad_x)

   implicit none

   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j, k  ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if (iv%num_ssmt2 > 0) then
      do n=1, iv % num_ssmt2
         k = iv % ssmt2(n) % info % levels

         if (k < 1) cycle
      
         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt2(n)%loc%i
         dy = iv%ssmt2(n)%loc%dy
         dym = iv%ssmt2(n)%loc%dym
         j = iv%ssmt2(n)%loc%j
         dx = iv%ssmt2(n)%loc%dx
         dxm = iv%ssmt2(n)%loc%dxm

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d_adj(jo_grad_x % rh, xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#else
         call da_interp_lin_3d_adj(jo_grad_x % rh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                jo_grad_y%ssmt2(n)%rh, k, iv%ssmt2(n)%zk, k)
#endif
      end do 
   end if

end subroutine da_transform_xtoy_ssmt2_adj


