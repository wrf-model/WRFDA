SUBROUTINE DA_Cal_Residual_SSMI_Tb( oi, y, re, &
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

   n_obs_bad % t % num = number(0, 0, 0)

   DO n=1, oi%num_ssmi_tb

    if( oi%ssmi_tb(n)%loc%proc_domain_with_halo ) then

      np_available = np_available + 7

      re%ssmi_tb(n)%tb19v = residual(n, 0, y%ssmi_tb(n)%tb19v, &
                            oi%ssmi_tb(n)%tb19v, n_obs_bad % t )
      re%ssmi_tb(n)%tb19h = residual(n, 0, y%ssmi_tb(n)%tb19h, &
                            oi%ssmi_tb(n)%tb19h, n_obs_bad % t )
      re%ssmi_tb(n)%tb22v = residual(n, 0, y%ssmi_tb(n)%tb22v, &
                            oi%ssmi_tb(n)%tb22v, n_obs_bad % t )
      re%ssmi_tb(n)%tb37v = residual(n, 0, y%ssmi_tb(n)%tb37v, &
                            oi%ssmi_tb(n)%tb37v, n_obs_bad % t )
      re%ssmi_tb(n)%tb37h = residual(n, 0, y%ssmi_tb(n)%tb37h, &
                            oi%ssmi_tb(n)%tb37h, n_obs_bad % t )
      re%ssmi_tb(n)%tb85v = residual(n, 0, y%ssmi_tb(n)%tb85v, &
                            oi%ssmi_tb(n)%tb85v, n_obs_bad % t )
      re%ssmi_tb(n)%tb85h = residual(n, 0, y%ssmi_tb(n)%tb85h, &
                            oi%ssmi_tb(n)%tb85h, n_obs_bad % t )
    endif

   END DO

   np_missing  = np_missing  + n_obs_bad % t % num % miss  

   np_bad_data = np_bad_data + n_obs_bad % t % num % bad  

   np_obs_used = np_obs_used + n_obs_bad % t % num % use     

END SUBROUTINE DA_Cal_Residual_SSMI_Tb
