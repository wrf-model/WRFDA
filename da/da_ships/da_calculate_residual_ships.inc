subroutine da_calculate_residual_ships( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(inout)     :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   if ( oi%num_ships > 0 ) then
      n_obs_bad % u % num = number(0, 0, 0)
      n_obs_bad % v % num = number(0, 0, 0)
      n_obs_bad % t % num = number(0, 0, 0)
      n_obs_bad % p % num = number(0, 0, 0)
      n_obs_bad % q % num = number(0, 0, 0)

      do n=1, oi%num_ships
         np_available = np_available + 5

         re%ships(n)%u = da_residual( n, 0, y%ships(n)%u, &
                                      oi%ships(n)%u, n_obs_bad % u )
         re%ships(n)%v = da_residual( n, 0, y%ships(n)%v, &
                                   oi%ships(n)%v, n_obs_bad % v )
         re%ships(n)%t = da_residual( n, 0, y%ships(n)%t, &
                                   oi%ships(n)%t, n_obs_bad % t )
         re%ships(n)%p = da_residual( n, 0, y%ships(n)%p, &
                                   oi%ships(n)%p, n_obs_bad % p )
         re%ships(n)%q = da_residual( n, 0, y%ships(n)%q, &
                                   oi%ships(n)%q, n_obs_bad % q )
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

   end if

end subroutine da_calculate_residual_ships


