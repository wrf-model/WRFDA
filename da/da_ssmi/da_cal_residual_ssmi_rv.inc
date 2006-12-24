subroutine da_cal_residual_ssmi_rv(oi, y, re, &
                                                  np_missing, np_bad_data, &
                                                  np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)        :: oi     ! Obs increment vector (O-B).
   type (y_type) , intent(in)        :: y      ! y = H (xa)
   type (y_type) , intent(out)       :: re     ! Residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   n_obs_bad % Speed % num = number(0, 0, 0)
   n_obs_bad % q % num     = number(0, 0, 0)

   do n=1, oi%num_ssmi_retrieval
      np_available = np_available + 2

      re%ssmi_retrieval(n)%Speed = da_residual(n, 0, y%ssmi_retrieval(n)%Speed, &
                               oi%ssmi_retrieval(n)%Speed, n_obs_bad % Speed)
      re%ssmi_retrieval(n)%tpw   = da_residual(n, 0, y%ssmi_retrieval(n)%tpw,   &
                               oi%ssmi_retrieval(n)%tpw, n_obs_bad % q      )
   end do

   np_missing  = np_missing + n_obs_bad % Speed % num % miss    &
                            + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % Speed % num % bad    &
                             + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % Speed % num % use    &
                             + n_obs_bad % q % num % use

end subroutine da_cal_residual_ssmi_rv


