subroutine da_calculate_residual_airsr( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   !----------------------------------------------------------------------
   ! Purpose: Calculates residual at AIRS retrieval locations                   
   !----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (oi%num_airsr > 0) then
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)

      do n=1, oi%num_airsr
         do k=1, oi%airsr(n)%info%levels
           np_available = np_available + 4

           re%airsr(n)%t(k) = da_residual( n, k, y%airsr(n)%t(k), &
                                      oi%airsr(n)%t(k), n_obs_bad % t)
           re%airsr(n)%q(k) = da_residual( n, k, y%airsr(n)%q(k), &
                                      oi%airsr(n)%q(k), n_obs_bad % q)
         end do
      end do

      np_missing = np_missing + n_obs_bad % t % num % miss + &
                               n_obs_bad % q % num % miss
      np_bad_data = np_bad_data + n_obs_bad % t % num % bad + &
                                 n_obs_bad % q % num % bad
      np_obs_used = np_obs_used + n_obs_bad % t % num % use + &
                                 n_obs_bad % q % num % use

   end if

end subroutine da_calculate_residual_airsr


