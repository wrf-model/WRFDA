subroutine da_calculate_residual_satem( oi, y, re,                        &
                                        np_missing, np_bad_data,          &
                                        np_obs_used, np_available )

  implicit none

  type (ob_type), intent(in)        :: oi     ! Innovation vector (O-B).
  type (y_type) , intent(in)        :: y      ! y = H (xa)
  type (y_type) , intent(inout)     :: re     ! Residual structure.

  integer       , intent(inout)     :: np_available, np_obs_used,          &
                                       np_missing  , np_bad_data

  type (bad_data_type)              :: n_obs_bad
  integer                           :: n, k

  if ( oi%num_Satem > 0 ) then
    n_obs_bad % thickness % num = number(0, 0, 0)

    do n=1, oi%num_Satem
       do k=1, oi%satem(n)%info%levels
          np_available = np_available + 1
          re%Satem(n)%thickness(k) = da_residual( n, k, y%Satem(n)%thickness(k), &
                             oi%Satem(n)%thickness(k), n_obs_bad % thickness )
        end do
    end do

    np_missing = np_missing + n_obs_bad % thickness % num % miss
    np_bad_data = np_bad_data + n_obs_bad % thickness % num % bad
    np_obs_used = np_obs_used + n_obs_bad % thickness % num % use

  end if

end subroutine da_calculate_residual_satem

