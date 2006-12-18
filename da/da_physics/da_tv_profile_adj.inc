subroutine da_tv_profile_adj(xp,jo_grad_x,xb,i,j,dx,dxm,dy,dym,    &
              pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)

   ! This is the adjoint routine for Tv_profile

   implicit none

   type (x_type) , intent(inout)  :: jo_grad_x ! grad_x(jo)
   type (xb_type),  intent(in)    :: xb        ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Dimensions and xpose buffers.
   integer, intent(in)                        :: i, j      ! OBS location
   real, intent(in)                           :: dx, dxm   ! Interpolation weights.
   real, intent(in)                           :: dy, dym   ! Interpolation weights.

   real, dimension(xp%kts-1:xp%kte+1), intent(in)    :: pre_ma,tv_ma
   real, dimension(xp%kts-1:xp%kte+1), intent(out)    :: ADJ_pre_ma,ADJ_tv_ma

   integer                        :: ii,jj,ks,ke
   real, dimension(2,2,xp%kts:xp%kte)    :: ADJ_tv_m

   ks = xp%kts; ke = xp%kte

   ADJ_tv_m(1,1,ks:ke) = dym*dxm * ADJ_tv_ma (ks:ke)
   ADJ_tv_m(2,1,ks:ke) = dym*dx *  ADJ_tv_ma (ks:ke)
   ADJ_tv_m(1,2,ks:ke) = dy*dxm*   ADJ_tv_ma (ks:ke)
   ADJ_tv_m(2,2,ks:ke) = dy*dx*    ADJ_tv_ma (ks:ke)
   jo_grad_x%p(i,j,ks:ke)    = jo_grad_x%p(i,j,ks:ke) + dym*dxm  * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i+1,j,ks:ke)  = jo_grad_x%p(i+1,j,ks:ke) + dym*dx * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i,j+1,ks:ke)  = jo_grad_x%p(i,j+1,ks:ke) + dy*dxm * ADJ_pre_ma(ks:ke)
   jo_grad_x%p(i+1,j+1,ks:ke)= jo_grad_x%p(i+1,j+1,ks:ke) + dy*dx* ADJ_pre_ma(ks:ke)
   ADJ_tv_ma (ks:ke)     = 0.
   ADJ_pre_ma(ks:ke)     = 0.

   do ii=i,i+1
      do jj=j,j+1
         jo_grad_x%t(ii,jj,ks:ke) = jo_grad_x%t(ii,jj,ks:ke) + &
                  ADJ_tv_m(ii-i+1,jj-j+1,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke))
         jo_grad_x%q(ii,jj,ks:ke) = jo_grad_x%q(ii,jj,ks:ke) + &
                  0.61*xb%t(ii,jj,ks:ke)*ADJ_tv_m(ii-i+1,jj-j+1,ks:ke)
      end do
   end do

end subroutine da_tv_profile_adj


