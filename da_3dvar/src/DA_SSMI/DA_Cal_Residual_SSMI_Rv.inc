SUBROUTINE DA_Cal_Residual_SSMI_Rv( oi, y, re, &
                                                  np_missing, np_bad_data, &
                                                  np_obs_used, np_available )

   IMPLICIT NONE

   TYPE (ob_type), INTENT(IN)        :: oi     ! Obs increment vector (O-B).
   TYPE (y_type) , INTENT(IN)        :: y      ! y = H (xa)
   TYPE (y_type) , INTENT(OUT)       :: re     ! Residual structure.

   INTEGER       , INTENT(INOUT)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   TYPE (bad_data_type)              :: n_obs_bad
   INTEGER                           :: n

   n_obs_bad % Speed % num = number(0, 0, 0)
   n_obs_bad % q % num     = number(0, 0, 0)

   DO n=1, oi%num_ssmi_retrieval

    if( oi%ssmi_retrieval(n)%loc%proc_domain_with_halo ) then

      np_available = np_available + 2

      re%ssmi_retrieval(n)%Speed = residual(n, 0, y%ssmi_retrieval(n)%Speed, &
                               oi%ssmi_retrieval(n)%Speed, n_obs_bad % Speed )
      re%ssmi_retrieval(n)%tpw   = residual(n, 0, y%ssmi_retrieval(n)%tpw,   &
                               oi%ssmi_retrieval(n)%tpw, n_obs_bad % q       )
    endif

   END DO

   np_missing  = np_missing + n_obs_bad % Speed % num % miss    &
                            + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % Speed % num % bad    &
                             + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % Speed % num % use    &
                             + n_obs_bad % q % num % use

END SUBROUTINE DA_Cal_Residual_SSMI_Rv

