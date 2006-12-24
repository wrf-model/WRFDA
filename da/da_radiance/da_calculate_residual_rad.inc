subroutine da_calculate_residual_rad(iv, y, re,                 &
                                        np_missing, np_bad_data, &
                                        np_obs_used, np_available)

  !---------------------------------------------------------------------------
  ! Purpose: calculate obs residual and counting obs number.
  !
  ! Method:  re = (d - h delta_x)
  !---------------------------------------------------------------------------

   implicit none


   type (ob_type), intent(in)        :: iv     ! innovation vector (o-b).
   type (y_type) , intent(in)        :: y      ! y = h (xa)
   type (y_type) , intent(inout)     :: re     ! residual structure.

   integer       , intent(inout)     :: np_available, np_obs_used,          &
                                        np_missing  , np_bad_data

   type (bad_data_type)              :: n_obs_bad
   integer                           :: n, k, i

   if (iv%num_inst < 1) return

   if (trace_use) call da_trace_entry("da_calculate_residual_rad")

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

      n_obs_bad % rad % num = number(0, 0, 0)

      do n=1, iv%instid(i)%num_rad
         do k=1, iv%instid(i)%nchan
            np_available = np_available + iv%instid(i)%nchan
            re%instid(i)%rad(n)%tb(k) = da_residual(n, k, &
               y%instid(i)%rad(n)%tb(k), &
               iv%instid(i)%rad(n)%tb(k), n_obs_bad % rad)
         end do
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % rad % num % miss
   np_bad_data = np_bad_data + n_obs_bad % rad % num % bad
   np_obs_used = np_obs_used + n_obs_bad % rad % num % use 

   if (trace_use) call da_trace_exit("da_calculate_residual_rad")
   
end subroutine da_calculate_residual_rad


