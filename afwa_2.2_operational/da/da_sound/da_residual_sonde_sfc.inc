subroutine da_residual_sonde_sfc(oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for sonde surface obs
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual vector (O-A).

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   if (oi%num_sound < 1) return

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % p % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, oi%num_sound
      np_available = np_available + 5

      re%sonde_sfc(n)%u = da_residual(n, 0, y%sonde_sfc(n)%u, &
                                    oi%sonde_sfc(n)%u, n_obs_bad % u)
      re%sonde_sfc(n)%v = da_residual(n, 0, y%sonde_sfc(n)%v, &
                                 oi%sonde_sfc(n)%v, n_obs_bad % v)
      re%sonde_sfc(n)%t = da_residual(n, 0, y%sonde_sfc(n)%t, &
                                 oi%sonde_sfc(n)%t, n_obs_bad % t)
      re%sonde_sfc(n)%p = da_residual(n, 0, y%sonde_sfc(n)%p, &
                                 oi%sonde_sfc(n)%p, n_obs_bad % p)
      re%sonde_sfc(n)%q = da_residual(n, 0, y%sonde_sfc(n)%q, &
                                 oi%sonde_sfc(n)%q, n_obs_bad % q)
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + &
               n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
               n_obs_bad % p % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
                n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
                n_obs_bad % p % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
                n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
                n_obs_bad % p % num % use + n_obs_bad % q % num % use

end subroutine da_residual_sonde_sfc


