subroutine da_tv_profile(xp, xb, i, j, dx, dxm, dy, dym, pre_ma, tv_ma)

   ! This subroutine calculates virtual temperature (tv_ma) on each level
   ! (pre_ma, pressure at the level) at the observed location (i,j). 
   ! dx, dxm, dy, dym are horizontal interpolation weighting.

   implicit none

   type (xb_type), intent(in)     :: xb       ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Dimensions and xpose buffers.
   integer, intent(in)            :: i, j     ! OBS location
   real, intent(in)               :: dx, dxm  ! Interpolation weights.
   real, intent(in)               :: dy, dym  ! Interpolation weights.
   real, dimension(xp%kts-1:xp%kte+1), intent(out)   :: pre_ma,tv_ma
                          
   integer                        :: ii,jj,ks,ke    ! Index dimension.
   real, dimension(2,2,xp%kts:xp%kte)   :: tv_m     ! Virtual temperatures

   ks = xp%kts; ke = xp%kte

   ! Virtual temperature

   do ii=i,i+1
      do jj=j,j+1
         tv_m(ii-i+1,jj-j+1,ks:ke) = xb%t(ii,jj,ks:ke) * &
            (1. + 0.61*xb%q(ii,jj,ks:ke))
      end do
   end do

   ! Horizontal interpolation to the obs. pt.

   pre_ma(ks:ke) = dym* ( dxm * xb%p(i,j,ks:ke) + dx * xb%p(i+1,j,ks:ke) ) + &
                   dy * ( dxm * xb%p(i,j+1,ks:ke) + dx * xb%p(i+1,j+1,ks:ke) )

   tv_ma (ks:ke) = dym* ( dxm * tv_m (1,1,ks:ke) + dx * tv_m (2,1,ks:ke) ) + &
                   dy * ( dxm * tv_m (1,2,ks:ke) + dx * tv_m (2,2,ks:ke) )

end subroutine da_tv_profile


