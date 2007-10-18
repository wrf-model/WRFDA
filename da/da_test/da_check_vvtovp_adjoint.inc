subroutine da_residual_ssmt1(iv, y, re, np_missing, np_bad_data,np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual structure.

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_ssmt1")

   n_obs_bad % t % num = number_type(0, 0, 0)

   do n=1, iv%info(ssmt1)%nlocal
      do k=1, iv%info(ssmt1)%levels(n)
         np_available = np_available + 1
         re%ssmt1(n)%t(k) = da_residual(n, k, y%ssmt1(n)%t(k), iv%ssmt1(n)%t(k), n_obs_bad % t)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % t % num % miss
   np_bad_data = np_bad_data + n_obs_bad % t % num % bad
   np_obs_used = np_obs_used + n_obs_bad % t % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_ssmt1")

end subroutine da_residual_ssmt1


    :: vv2_alpha(ims:ime,jms:jme,1:ne)

   if (trace_use) call da_trace_entry("da_check_vvtovp_adjoint")

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   write(unit=stdout, fmt='(/a/)') 'da_check_vvtovp_adjoint: Test Results:'
      
   !----------------------------------------------------------------------
   ! [2.0] Perform Vp = U_v Vv transform:
   !----------------------------------------------------------------------

   call da_vertical_transform('u', be, &
                               xb % vertical_inner_product, &
                               vv, vp)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation:
   !----------------------------------------------------------------------

   adj_par_lhs = sum(vp % v1(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp1_sumsq &
               + sum(vp % v2(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp2_sumsq &
               + sum(vp % v3(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp3_sumsq &
               + sum(vp % v4(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp4_sumsq &
               + sum(vp % v5(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp5_sumsq

   if (be % ne > 0) then
      adj_par_lhs = adj_par_lhs + &
         sum(vp % alpha(its:ite,jts:jte,1:be%ne)**2) * inv_typ_vpalpha_sumsq
   end if

   !----------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !----------------------------------------------------------------------

   vp % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp1_sumsq
   vp % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp2_sumsq
   vp % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp3_sumsq
   vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp4_sumsq
   vp % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp5_sumsq

   if (be % ne > 0) then
      vp % alpha(its:ite,jts:jte,1:be%ne) = &
         vp % alpha(its:ite,jts:jte,1:be%ne) * inv_typ_vpalpha_sumsq
   end if

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   vv2_v1(its:ite,jts:jte,1:be%v1%mz) = vv % v1(its:ite,jts:jte,1:be%v1%mz)
   vv2_v2(its:ite,jts:jte,1:be%v2%mz) = vv % v2(its:ite,jts:jte,1:be%v2%mz)
   vv2_v3(its:ite,jts:jte,1:be%v3%mz) = vv % v3(its:ite,jts:jte,1:be%v3%mz)
   vv2_v4(its:ite,jts:jte,1:be%v4%mz) = vv % v4(its:ite,jts:jte,1:be%v4%mz)
   vv2_v5(its:ite,jts:jte,1:be%v5%mz) = vv % v5(its:ite,jts:jte,1:be%v5%mz)

   if (be % ne > 0) then
      vv2_alpha(its:ite,jts:jte,1:be%ne) = vv % alpha(its:ite,jts:jte,1:be%ne)
   end if      

   call da_vertical_transform('u_adj', be, &
                               xb % vertical_inner_product, &
                               vv, vp)

   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------

   adj_par_rhs = 0.0
   if (be % v1 % mz > 0) &
      adj_par_rhs = sum(vv % v1(its:ite,jts:jte,1:be%v1%mz) * &
         vv2_v1(its:ite,jts:jte,1:be%v1%mz)) + adj_par_rhs
   if (be % v2 % mz > 0) &
      adj_par_rhs = sum(vv % v2(its:ite,jts:jte,1:be%v2%mz) * &
         vv2_v2(its:ite,jts:jte,1:be%v2%mz)) + adj_par_rhs
   if (be % v3 % mz > 0) &
      adj_par_rhs = sum(vv % v3(its:ite,jts:jte,1:be%v3%mz) * &
         vv2_v3(its:ite,jts:jte,1:be%v3%mz)) + adj_par_rhs
   if (be % v4 % mz > 0) &
      adj_par_rhs = sum(vv % v4(its:ite,jts:jte,1:be%v4%mz) * &
         vv2_v4(its:ite,jts:jte,1:be%v4%mz)) + adj_par_rhs
   if (be % v5 % mz == 1) &
      adj_par_rhs = sum(vv % v5(its:ite,jts:jte,1:be%v5%mz) * &
         vv2_v5(its:ite,jts:jte,1:be%v5%mz)) + adj_par_rhs
   if (be % ne > 0) &
      adj_par_rhs = sum(vv % alpha(its:ite,jts:jte,1:be%ne) * &
         vv2_alpha(its:ite,jts:jte,1:be%ne)) + adj_par_rhs

   !----------------------------------------------------------------------
   ! [7.0] Print output:
   !----------------------------------------------------------------------

   write(unit=stdout, fmt='(a,1pe22.14)') &
        'Single domain < vp,     vp > = ', adj_par_lhs, &
        'Single domain < Vv_adj, Vv > = ', adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)

   write(unit=stdout,fmt='(A,2F15.2)') &
      'TEST_COVERAGE_da_check_vvtovp_adjoint:  adj_sum_lhs,adj_sum_rhs = ', &
      adj_sum_lhs,adj_sum_rhs

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < Vp, Vp >     = ', adj_sum_lhs, &
         'Whole  Domain: < Vv_adj, Vv > = ', adj_sum_rhs
   end if
      
   vv % v1(its:ite,jts:jte,1:be%v1%mz) = vv2_v1(its:ite,jts:jte,1:be%v1%mz)
   vv % v2(its:ite,jts:jte,1:be%v2%mz) = vv2_v2(its:ite,jts:jte,1:be%v2%mz)
   vv % v3(its:ite,jts:jte,1:be%v3%mz) = vv2_v3(its:ite,jts:jte,1:be%v3%mz)
   vv % v4(its:ite,jts:jte,1:be%v4%mz) = vv2_v4(its:ite,jts:jte,1:be%v4%mz)
   vv % v5(its:ite,jts:jte,1:be%v5%mz) = vv2_v5(its:ite,jts:jte,1:be%v5%mz)

   if (be % ne > 0) then
      vv % alpha(its:ite,jts:jte,1:be%ne) = vv2_alpha(its:ite,jts:jte,1:be%ne)
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vvtovp_adjoint: Test Finished.'

   if (trace_use) call da_trace_exit("da_check_vvtovp_adjoint")

end subroutine da_check_vvtovp_adjoint


