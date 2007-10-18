subroutine da_residual_geoamv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates residual vector for Geo. CMV's
   !               
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_geoamv")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)

   do n=1, iv%info(geoamv)%nlocal
      do k=1, iv%info(geoamv)%levels(n)
         np_available = np_available + 2
         re%geoamv(n)%u(k) = da_residual(n, 0, y%geoamv(n)%u(k), iv%geoamv(n)%u(k), n_obs_bad % u)
         re%geoamv(n)%v (k)= da_residual(n, 0, y%geoamv(n)%v(k), iv%geoamv(n)%v(k), n_obs_bad % v)
      end do
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + n_obs_bad % v % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + n_obs_bad % v % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + n_obs_bad % v % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_geoamv")

end subroutine da_residual_geoamv


2
      call da_max_error_qc_new (it, iv%info(ships), n, iv%ships(n)%u, max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', failed, check_max_iv_print)
      call da_max_error_qc_new (it, iv%info(ships), n, iv%ships(n)%v, max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
      call da_max_error_qc_new (it, iv%info(ships), n, iv%ships(n)%p, max_error_p, itp, itpf, check_max_iv_unit, 'Press', failed, check_max_iv_print)         
      call da_max_error_qc_new (it, iv%info(ships), n, iv%ships(n)%t, max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)
      ! Mixing ratio, same ratio as relative humidity
      call da_max_error_qc_new (it, iv%info(ships), n, iv%ships(n)%q, max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', failed, check_max_iv_print)
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_ships")

end subroutine da_check_max_iv_ships


