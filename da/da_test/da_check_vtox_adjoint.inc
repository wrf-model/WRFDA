subroutine da_residual_ssmi_rv(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

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

   if (trace_use) call da_trace_entry("da_residual_ssmi_rv")

   n_obs_bad % Speed % num = number_type(0, 0, 0)
   n_obs_bad % q % num     = number_type(0, 0, 0)

   do n=1, iv%nlocal(ssmi_rv)
      np_available = np_available + 2

      re%ssmi_rv(n)%Speed = da_residual(n, 0, y%ssmi_rv(n)%Speed, &
         iv%ssmi_rv(n)%Speed, n_obs_bad % Speed)
      re%ssmi_rv(n)%tpw   = da_residual(n, 0, y%ssmi_rv(n)%tpw,   &
         iv%ssmi_rv(n)%tpw, n_obs_bad % q      )
   end do

   np_missing  = np_missing + n_obs_bad % Speed % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % Speed % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % Speed % num % use + n_obs_bad % q % num % use

   if (trace_use) call da_trace_exit("da_residual_ssmi_rv")

end subroutine da_residual_ssmi_rv


---------------------------------------------------------
   ! [2.0] Perform x = U v transform:
   !----------------------------------------------------------------------

   call da_zero_x (grid%xa)

   call da_transform_vtox(grid,cv_size, xbx, be, ep, cv1, vv, vp)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation: 
   !----------------------------------------------------------------------

   adj_par_lhs = sum(grid%xa % u(its:ite, jts:jte, kts:kte)**2) / typical_u_rms**2 &
               + sum(grid%xa % v(its:ite, jts:jte, kts:kte)**2) / typical_v_rms**2 &     
               + sum(grid%xa % p(its:ite, jts:jte, kts:kte)**2) / typical_p_rms**2 &     
               + sum(grid%xa % t(its:ite, jts:jte, kts:kte)**2) / typical_t_rms**2 &     
               + sum(grid%xa % q(its:ite, jts:jte, kts:kte)**2) / typical_q_rms**2 &     
               + sum(grid%xa % rho(its:ite,jts:jte,kts:kte)**2)/ typical_rho_rms**2 & 
               + sum(grid%xa % psfc(its:ite, jts:jte)**2) / typical_p_rms**2             

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      adj_par_lhs = adj_par_lhs &
              + sum(grid%xa % rh(its:ite, jts:jte, kts:kte)**2) / typical_rh_rms**2
   end if

   if (use_radarobs) then
      adj_par_lhs = adj_par_lhs &
         + sum(grid%xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
         + sum(grid%xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------

   grid%xa % u(:,:,:) = grid%xa % u(:,:,:) / typical_u_rms**2
   grid%xa % v(:,:,:) = grid%xa % v(:,:,:) / typical_v_rms**2
   grid%xa % p(:,:,:) = grid%xa % p(:,:,:) / typical_p_rms**2
   grid%xa % t(:,:,:) = grid%xa % t(:,:,:) / typical_t_rms**2
   grid%xa % q(:,:,:) = grid%xa % q(:,:,:) / typical_q_rms**2
   grid%xa % rho(:,:,:) = grid%xa % rho(:,:,:) / typical_rho_rms**2

   grid%xa % psfc(:,:) = grid%xa % psfc(:,:) / typical_p_rms**2

   if (cv_options_hum == cv_options_hum_relative_humidity) then
      grid%xa % rh(:,:,:) = grid%xa % rh(:,:,:) / typical_rh_rms**2
   end if

   if (use_radarobs) then
      grid%xa %wh(:,:,:) = grid%xa %wh(:,:,:) / typical_w_rms**2
      grid%xa % w(:,:,:) = 0.0
   else
      grid%xa %w (:,:,:) = grid%xa %w (:,:,:) / typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv2)

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

   adj_par_rhs = sum(cv1(1:cv_size) * cv2(1:cv_size))

   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)

   if (global) then
      adj_sum_rhs = adj_par_rhs
   else
      adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)
   end if
   write (unit=stdout,fmt='(A,2F15.2)') &
      'TEST_COVERAGE_da_check_vtox_adjoint:  adj_sum_lhs,adj_sum_rhs = ', &
      adj_sum_lhs,adj_sum_rhs

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
         'Whole  Domain: < v_adj, v > = ', adj_sum_rhs
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vtox_adjoint: Finished'

   if (trace_use) call da_trace_exit("da_check_vtox_adjoint")

end subroutine da_check_vtox_adjoint


