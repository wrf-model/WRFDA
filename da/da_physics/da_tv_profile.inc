subroutine da_tv_profile(grid, i, j, dx, dxm, dy, dym, pre_ma, tv_ma)

   !--------------------------------------------------------------------------
   ! Purpose: Calculates virtual temperature (tv_ma) on each level
   ! (pre_ma, pressure at the level) at the observed location (i,j). 
   ! dx, dxm, dy, dym are horizontal interpolation weighting.
   !--------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)  :: grid
   integer,       intent(in)  :: i, j     ! OBS location
   real,          intent(in)  :: dx, dxm  ! interpolation weights.
   real,          intent(in)  :: dy, dym  ! interpolation weights.
   real,          intent(out) :: pre_ma(kts-1:kte+1)
   real,          intent(out) :: tv_ma(kts-1:kte+1)
                          
   integer                        :: ii,jj    ! index dimension.
   real, dimension(2,2,kts:kte)   :: tv_m     ! Virtual temperatures

   ! Virtual temperature

   do ii=i,i+1
      do jj=j,j+1
         tv_m(ii-i+1,jj-j+1,kts:kte) = grid%xb%t(ii,jj,kts:kte) * &
            (1.0 + 0.61*grid%xb%q(ii,jj,kts:kte))
      end do
   end do

   ! Horizontal interpolation to the obs. pt.

   pre_ma(kts:kte) = dym* ( dxm * grid%xb%p(i,j,kts:kte) + dx * grid%xb%p(i+1,j,kts:kte) ) + &
                   dy * ( dxm * grid%xb%p(i,j+1,kts:kte) + dx * grid%xb%p(i+1,j+1,kts:kte) )

   tv_ma (kts:kte) = dym* ( dxm * tv_m (1,1,kts:kte) + dx * tv_m (2,1,kts:kte) ) + &
                   dy * ( dxm * tv_m (1,2,kts:kte) + dx * tv_m (2,2,kts:kte) )

end subroutine da_tv_profile


