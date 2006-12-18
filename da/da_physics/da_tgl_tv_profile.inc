subroutine da_tgl_tv_profile(xp,xa,xb,i,j,dx,dxm,dy,dym,             &
           pre_ma,tv_ma,TGL_pre_ma,TGL_tv_ma)

   ! This is the tangent-linear routine for Tv_profile

   implicit none

   type (x_type),  intent(in)     :: xa       ! gridded analysis increment.
   type (xb_type),  intent(in)    :: xb       ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Dimensions and xpose buffers.
   integer, intent(in)            :: i, j     ! OBS location
   real, intent(in)               :: dx, dxm  ! Interpolation weights.
   real, intent(in)               :: dy, dym  ! Interpolation weights.
   real, dimension(xp%kts-1:xp%kte+1),intent(out)      :: TGL_pre_ma,TGL_tv_ma
   real, dimension(xp%kts-1:xp%kte+1),intent(out)      :: pre_ma,tv_ma

   integer                        :: ii,jj,ks,ke
   real, dimension(2,2,xp%kts:xp%kte)       :: tv_m,TGL_tv_m

   ks = xp%kts; ke = xp%kte

   do ii=i,i+1
      do jj=j,j+1
         TGL_tv_m(ii-i+1,jj-j+1,ks:ke) = xa%t(ii,jj,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke)) +  &
                                      0.61*xb%t(ii,jj,ks:ke)*xa%q(ii,jj,ks:ke)
         tv_m(ii-i+1,jj-j+1,ks:ke)     = xb%t(ii,jj,ks:ke)*(1.+0.61*xb%q(ii,jj,ks:ke))
      end do
   end do
 
   TGL_pre_ma(ks:ke) = dym* ( dxm * xa%p(i,j,ks:ke) + dx * xa%p(i+1,j,ks:ke) ) + &
                       dy * ( dxm * xa%p(i,j+1,ks:ke) + dx * xa%p(i+1,j+1,ks:ke) )
   TGL_tv_ma (ks:ke) = dym* ( dxm * TGL_tv_m(1,1,ks:ke) + dx * TGL_tv_m(2,1,ks:ke) ) + &
                       dy * ( dxm * TGL_tv_m(1,2,ks:ke) + dx * TGL_tv_m(2,2,ks:ke) )
   pre_ma(ks:ke) = dym* ( dxm * xb%p(i,j,ks:ke) + dx * xb%p(i+1,j,ks:ke) ) + &
                   dy * ( dxm * xb%p(i,j+1,ks:ke) + dx * xb%p(i+1,j+1,ks:ke) )
   tv_ma (ks:ke) = dym* ( dxm * tv_m (1,1,ks:ke) + dx * tv_m (2,1,ks:ke) ) + &
                   dy * ( dxm * tv_m (1,2,ks:ke) + dx * tv_m (2,2,ks:ke) )
 
end subroutine da_tgl_tv_profile


