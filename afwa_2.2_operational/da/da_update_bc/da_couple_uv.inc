subroutine da_couple_uv (u,  v,  mu, mub,  msfu, msfv, &
                       ids, ide, jds, jde, kds, kde)

   implicit none

   integer, intent(in) :: ids, ide, jds, jde, kds, kde

   real, dimension(ids:ide+1, jds:jde  , kds:kde), intent(inout) :: u
   real, dimension(ids:ide  , jds:jde+1, kds:kde), intent(inout) :: v

   real, dimension(ids:ide+1, jds:jde ),          intent(in  ) :: msfu
   real, dimension(ids:ide  , jds:jde+1),          intent(in  ) :: msfv

   real, dimension(ids:ide  , jds:jde ),          intent(in  ) :: mu, mub

   real, allocatable, dimension(:, :) :: muu, muv

   allocate(muu(ids:ide+1, jds:jde ))
   allocate(muv(ids:ide  , jds:jde+1))

   ! couple variables u, v

   call da_calc_mu_uv (mu, mub, muu, muv, &
                     ids, ide, jds, jde)

   call da_couple (muu, u, msfu, &
                 ids, ide+1, jds, jde, kds, kde)

   call da_couple (muv, v, msfv, &
                 ids, ide, jds, jde+1, kds, kde)

   deallocate(muu)
   deallocate(muv)

end subroutine da_couple_uv


