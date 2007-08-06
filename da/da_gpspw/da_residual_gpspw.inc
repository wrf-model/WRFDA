subroutine da_residual_gpspw(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (ob_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n

   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%num_gpspw
      np_available = np_available + 1
      re%gpspw(n)%tpw = da_residual(n, 0, y%gpspw(n)%tpw, iv%gpspw(n)%tpw, n_obs_bad % q)
   end do

   np_missing  = np_missing  + n_obs_bad % q % num % miss 
   np_bad_data = np_bad_data + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % q % num % use


end subroutine da_residual_gpspw


