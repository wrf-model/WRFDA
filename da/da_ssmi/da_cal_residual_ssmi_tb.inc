subroutine da_cal_residual_ssmi_tb( oi, y, re, &
                                          np_missing, np_bad_data, &
                                          np_obs_used, np_available )

   implicit none

   type (ob_type), intent(in)        :: oi     ! Obs increment vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(out)       :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   n_obs_bad % t % num = number(0, 0, 0)

   do n=1, oi%num_ssmi_tb

      np_available = np_available + 7

      re%ssmi_tb(n)%tb19v = da_residual(n, 0, y%ssmi_tb(n)%tb19v, &
                            oi%ssmi_tb(n)%tb19v, n_obs_bad % t )
      re%ssmi_tb(n)%tb19h = da_residual(n, 0, y%ssmi_tb(n)%tb19h, &
                            oi%ssmi_tb(n)%tb19h, n_obs_bad % t )
      re%ssmi_tb(n)%tb22v = da_residual(n, 0, y%ssmi_tb(n)%tb22v, &
                            oi%ssmi_tb(n)%tb22v, n_obs_bad % t )
      re%ssmi_tb(n)%tb37v = da_residual(n, 0, y%ssmi_tb(n)%tb37v, &
                            oi%ssmi_tb(n)%tb37v, n_obs_bad % t )
      re%ssmi_tb(n)%tb37h = da_residual(n, 0, y%ssmi_tb(n)%tb37h, &
                            oi%ssmi_tb(n)%tb37h, n_obs_bad % t )
      re%ssmi_tb(n)%tb85v = da_residual(n, 0, y%ssmi_tb(n)%tb85v, &
                            oi%ssmi_tb(n)%tb85v, n_obs_bad % t )
      re%ssmi_tb(n)%tb85h = da_residual(n, 0, y%ssmi_tb(n)%tb85h, &
                            oi%ssmi_tb(n)%tb85h, n_obs_bad % t )
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss  

   np_bad_data = np_bad_data + n_obs_bad % t % num % bad  

   np_obs_used = np_obs_used + n_obs_bad % t % num % use     

end subroutine da_cal_residual_ssmi_tb


