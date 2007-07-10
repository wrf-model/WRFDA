subroutine da_tv_profile_adj(grid,jo_grad_x,i,j,dx,dxm,dy,dym,    &
   pre_ma,tv_ma,ADJ_pre_ma,ADJ_tv_ma)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint routine for da_tv_profile
   !-----------------------------------------------------------------------

   implicit none

   type (x_type) , intent(inout)  :: jo_grad_x ! grad_x(jo)
   type (domain),  intent(in)     :: grid
   integer, intent(in)                        :: i, j      ! OBS location
   real, intent(in)                           :: dx, dxm   ! interpolation weights.
   real, intent(in)                           :: dy, dym   ! interpolation weights.

   real, dimension(kts-1:kte+1), intent(in)    :: pre_ma,tv_ma
   real, dimension(kts-1:kte+1), intent(inout) :: ADJ_pre_ma,ADJ_tv_ma

   integer                        :: ii,jj
   real, dimension(2,2,kts:kte)    :: ADJ_tv_m

   ADJ_tv_m(1,1,kts:kte) = dym*dxm * ADJ_tv_ma (kts:kte)
   ADJ_tv_m(2,1,kts:kte) = dym*dx *  ADJ_tv_ma (kts:kte)
   ADJ_tv_m(1,2,kts:kte) = dy*dxm*   ADJ_tv_ma (kts:kte)
   ADJ_tv_m(2,2,kts:kte) = dy*dx*    ADJ_tv_ma (kts:kte)
   jo_grad_x%p(i,j,kts:kte)    = jo_grad_x%p(i,j,kts:kte) + dym*dxm  * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i+1,j,kts:kte)  = jo_grad_x%p(i+1,j,kts:kte) + dym*dx * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i,j+1,kts:kte)  = jo_grad_x%p(i,j+1,kts:kte) + dy*dxm * ADJ_pre_ma(kts:kte)
   jo_grad_x%p(i+1,j+1,kts:kte)= jo_grad_x%p(i+1,j+1,kts:kte) + dy*dx* ADJ_pre_ma(kts:kte)
   ADJ_tv_ma (kts:kte)     = 0.
   ADJ_pre_ma(kts:kte)     = 0.

   do ii=i,i+1
      do jj=j,j+1
         jo_grad_x%t(ii,jj,kts:kte) = jo_grad_x%t(ii,jj,kts:kte) + &
                  ADJ_tv_m(ii-i+1,jj-j+1,kts:kte)*(1.+0.61*grid%xb%q(ii,jj,kts:kte))
         jo_grad_x%q(ii,jj,kts:kte) = jo_grad_x%q(ii,jj,kts:kte) + &
                  0.61*grid%xb%t(ii,jj,kts:kte)*ADJ_tv_m(ii-i+1,jj-j+1,kts:kte)
      end do
   end do

end subroutine da_tv_profile_adj


