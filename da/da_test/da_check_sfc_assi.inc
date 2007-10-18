subroutine da_residual_ships(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for ship obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n

   if (trace_use_dull) call da_trace_entry("da_residual_ships")

   n_obs_bad % u % num = number_type(0, 0, 0)
   n_obs_bad % v % num = number_type(0, 0, 0)
   n_obs_bad % t % num = number_type(0, 0, 0)
   n_obs_bad % p % num = number_type(0, 0, 0)
   n_obs_bad % q % num = number_type(0, 0, 0)

   do n=1, iv%nlocal(ships)
      np_available = np_available + 5

      re%ships(n)%u = da_residual(n, 0, y%ships(n)%u, iv%ships(n)%u, n_obs_bad % u)
      re%ships(n)%v = da_residual(n, 0, y%ships(n)%v, iv%ships(n)%v, n_obs_bad % v)
      re%ships(n)%t = da_residual(n, 0, y%ships(n)%t, iv%ships(n)%t, n_obs_bad % t)
      re%ships(n)%p = da_residual(n, 0, y%ships(n)%p, iv%ships(n)%p, n_obs_bad % p)
      re%ships(n)%q = da_residual(n, 0, y%ships(n)%q, iv%ships(n)%q, n_obs_bad % q)
   end do

   np_missing = np_missing + n_obs_bad % u % num % miss + &
      n_obs_bad % v % num % miss + n_obs_bad % t % num % miss + &
      n_obs_bad % p % num % miss + n_obs_bad % q % num % miss
   np_bad_data = np_bad_data + n_obs_bad % u % num % bad + &
      n_obs_bad % v % num % bad + n_obs_bad % t % num % bad + &
      n_obs_bad % p % num % bad + n_obs_bad % q % num % bad
   np_obs_used = np_obs_used + n_obs_bad % u % num % use + &
      n_obs_bad % v % num % use + n_obs_bad % t % num % use + &
      n_obs_bad % p % num % use + n_obs_bad % q % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_ships")

end subroutine da_residual_ships


(synop)
      call da_transform_xtopsfc(grid, iv%synop(n), y%synop(n))


      pertile_lhs = pertile_lhs &
                  + y%synop(n)%u * y%synop(n)%u &
                  + y%synop(n)%v * y%synop(n)%v &
                  + y%synop(n)%t * y%synop(n)%t &
                  + y%synop(n)%p * y%synop(n)%p &
                  + y%synop(n)%q * y%synop(n)%q

      if (iv%info(synop)%proc_domain(1,n)) then
         partial_lhs = partial_lhs &
                     + y%synop(n)%u * y%synop(n)%u &
                     + y%synop(n)%v * y%synop(n)%v &
                     + y%synop(n)%t * y%synop(n)%t &
                     + y%synop(n)%p * y%synop(n)%p &
                     + y%synop(n)%q * y%synop(n)%q
      end if
   end do

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   call da_zero_x(grid%xa)

   do n=1, iv%nlocal(synop)
      call da_transform_xtopsfc_adj(grid,iv%synop(n),y%synop(n),grid%xa)
   end do

   call da_transform_xtowtq_adj (grid)
   
   pertile_rhs = sum(grid%xa%u(ims:ime, jms:jme, kms:kme) * &
      xa2_u(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%v(ims:ime, jms:jme, kms:kme) * &
      xa2_v(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%t(ims:ime, jms:jme, kms:kme) * &
      xa2_t(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%p(ims:ime, jms:jme, kms:kme) * &
      xa2_p(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%q(ims:ime, jms:jme, kms:kme) * &
      xa2_q(ims:ime, jms:jme, kms:kme)) + &
                 sum(grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjivnt test equation:
   !-------------------------------------------------------------------------
   
   partial_rhs = &
      sum(grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite,jts:jte,kts:kte)) + &
      sum(grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))
   
   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------
   
   write(unit=stdout, fmt='(A,1pe22.14)') &
        ' Tile < y, y     > = ', pertile_lhs, &
        ' Tile < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real(partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real(partial_rhs)
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_check_sfc_assi_A:  adj_ttl_lhs,adj_ttl_rhs = ', &
      adj_ttl_lhs,adj_ttl_rhs
   if (rootproc) then
      write(unit=stdout, fmt='(A,1pe22.14)') &
         ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') &
         ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if

   ! recover grid%xa
   grid%xa%u(ims:ime, jms:jme, kms:kme) = xa2_u(ims:ime, jms:jme, kms:kme)
   grid%xa%v(ims:ime, jms:jme, kms:kme) = xa2_v(ims:ime, jms:jme, kms:kme)
   grid%xa%t(ims:ime, jms:jme, kms:kme) = xa2_t(ims:ime, jms:jme, kms:kme)
   grid%xa%p(ims:ime, jms:jme, kms:kme) = xa2_p(ims:ime, jms:jme, kms:kme)
   grid%xa%q(ims:ime, jms:jme, kms:kme) = xa2_q(ims:ime, jms:jme, kms:kme)

   grid%xa%psfc(ims:ime, jms:jme) = xa2_psfc(ims:ime, jms:jme)
   grid%xa%tgrn(ims:ime, jms:jme) = xa2_tgrn(ims:ime, jms:jme)
   grid%xa%u10 (ims:ime, jms:jme) = xa2_u10 (ims:ime, jms:jme)
   grid%xa%v10 (ims:ime, jms:jme) = xa2_v10 (ims:ime, jms:jme)
   grid%xa%t2  (ims:ime, jms:jme) = xa2_t2  (ims:ime, jms:jme)
   grid%xa%q2  (ims:ime, jms:jme) = xa2_q2  (ims:ime, jms:jme)

   call wrf_shutdown

   if (trace_use) call da_trace_exit("da_check_sfc_assi")
   
end subroutine da_check_sfc_assi



