subroutine da_residual_ssmi_tb(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Obs increment vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n

   if (trace_use) call da_trace_entry("da_residual_ssmi_tb")

   n_obs_bad % t % num = number_type(0, 0, 0)

   do n=1, iv%nlocal(ssmi_tb)
      np_available = np_available + 7

      re%ssmi_tb(n)%tb19v = da_residual(n, 0, y%ssmi_tb(n)%tb19v, iv%ssmi_tb(n)%tb19v, n_obs_bad % t)
      re%ssmi_tb(n)%tb19h = da_residual(n, 0, y%ssmi_tb(n)%tb19h, iv%ssmi_tb(n)%tb19h, n_obs_bad % t)
      re%ssmi_tb(n)%tb22v = da_residual(n, 0, y%ssmi_tb(n)%tb22v, iv%ssmi_tb(n)%tb22v, n_obs_bad % t)
      re%ssmi_tb(n)%tb37v = da_residual(n, 0, y%ssmi_tb(n)%tb37v, iv%ssmi_tb(n)%tb37v, n_obs_bad % t)
      re%ssmi_tb(n)%tb37h = da_residual(n, 0, y%ssmi_tb(n)%tb37h, iv%ssmi_tb(n)%tb37h, n_obs_bad % t)
      re%ssmi_tb(n)%tb85v = da_residual(n, 0, y%ssmi_tb(n)%tb85v, iv%ssmi_tb(n)%tb85v, n_obs_bad % t)
      re%ssmi_tb(n)%tb85h = da_residual(n, 0, y%ssmi_tb(n)%tb85h, iv%ssmi_tb(n)%tb85h, n_obs_bad % t)
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss  
   np_bad_data = np_bad_data + n_obs_bad % t % num % bad  
   np_obs_used = np_obs_used + n_obs_bad % t % num % use    

   if (trace_use) call da_trace_exit("da_residual_ssmi_tb") 

end subroutine da_residual_ssmi_tb


-----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation and
   !        Rescale input to adjoint routine :
   !-------------------------------------------------------------------------

   call da_get_y_lhs_value(iv, y, partial_lhs, pertile_lhs, adj_sum_lhs)

   cv = 0.0
   cg_jcdf = 1

   ! WHY?
   ! call da_zero_vp_type(vp)
   ! call da_zero_vp_type(vv)
   ! call da_zero_x(grid%xa)      

   call da_transform_vtoy_adj(0,cv_size, be, ep, cv, iv, vp, vv, xbx, y, grid, &
      config_flags, cg_jcdf)

   adj_rhs = sum(cv(1:cv_size) * cv_2(1:cv_size))

   !-------------------------------------------------------------------------
   !      Print output:
   !-------------------------------------------------------------------------

#ifdef DM_PARALLEL
   if (global) then
      adj_sum_rhs = adj_rhs
   else
      call mpi_allreduce(adj_rhs, adj_sum_rhs, 1, true_mpi_real, mpi_sum, &
                       comm, ierr)
   end if
#else
   adj_sum_rhs = adj_rhs
   adj_sum_lhs = partial_lhs
#endif

#ifdef DM_PARALLEL
   if (rootproc) then
      write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < y, y     > = ', adj_sum_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') &
         'Whole Domain  < v, v_adj > = ', adj_sum_rhs
   end if
#else
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < y, y     > = ', adj_sum_lhs
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < v, v_adj > = ', adj_sum_rhs
#endif

   if (trace_use_dull) call da_trace_exit("da_check_vtoy_adjoint")

end subroutine da_check_vtoy_adjoint


