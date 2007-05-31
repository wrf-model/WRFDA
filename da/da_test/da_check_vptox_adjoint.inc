subroutine da_check_vptox_adjoint(grid,ne, xb, be, ep, xp, vp, xa)

   !---------------------------------------------------------------------------
   ! Purpose: Test Vp to X routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < x, x > = < v_adj, v >.
   !---------------------------------------------------------------------------

   implicit none

   type (domain)                    :: grid

   integer, intent(in)              :: ne   ! Ensemble size.
   type (xb_type), intent(in)       :: xb   ! first guess.
   type (be_type), intent(in)       :: be   ! background errors.
   type (ep_type), intent(in)       :: ep   ! Ensemble perturbation type.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   type (vp_type),intent(inout)     :: vp   ! grdipt/level cv (local).
   type (x_type), intent(inout)     :: xa   ! grad_x(jo)

   real                             :: adj_par_lhs ! < x, x >
   real                             :: adj_par_rhs ! < v_adj, v >
   real                             :: adj_sum_lhs ! < x, x >
   real                             :: adj_sum_rhs ! < v_adj, v >
   real                             :: vp2_v1(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v2(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v3(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v4(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v5(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_alpha(ims:ime,jms:jme,1:ne)

   if (trace_use) call da_trace_entry("da_check_vptox_adjoint")

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   write(unit=stdout, fmt='(/a/)') 'da_check_vptox_adjoint: Test Results:'

   call da_zero_x(xa)

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)

   call da_psichi_to_uv(vp % v1, vp % v2, xb % coefx, &
                        xb % coefy , xa % u, xa % v)

   adj_par_lhs = sum(xa % u(its:ite,jts:jte,:)**2) / typical_u_rms**2
   adj_par_lhs = sum(xa % v(its:ite,jts:jte,:)**2) / typical_v_rms**2 + &
      adj_par_lhs

   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2

   vp%v1(:,:,:)=0.
   vp%v2(:,:,:)=0.

   call da_psichi_to_uv_adj(xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2)

   adj_par_rhs = sum(vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:))
   adj_par_rhs = sum(vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:)) + &
      adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)
   write(unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_da_check_vptox_adjoint A:  adj_sum_lhs,adj_sum_rhs = ', &
      adj_sum_lhs,adj_sum_rhs

   if (rootproc) then
      write(unit=stdout, fmt='(/a/)') &
          ' da_check_da_psichi_to_uv: Adjoint Test Results:'

      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
          'Whole  Domain: < u_v,     u_v         > = ', adj_sum_lhs, &
          'Whole  Domain: < psi_chi, psi_chi_adj > = ', adj_sum_rhs
   end if

   vp%v1(:,:,:) = vp2_v1(:,:,:)
   vp%v2(:,:,:) = vp2_v2(:,:,:)

   call da_zero_x(xa)

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)
   vp2_v3(:,:,:) = vp % v3(:,:,:)
   vp2_v4(:,:,:) = vp % v4(:,:,:)
   vp2_v5(:,:,:) = vp % v5(:,:,:)
   if (be % ne > 0) vp2_alpha(:,:,:) = vp % alpha(:,:,:)

   !-------------------------------------------------------------------------
   ! [2.0] Perform x = U vp transform:
   !-------------------------------------------------------------------------

   call da_transform_vptox(grid,xb, vp, xp, xa, be, ep)

   !-------------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation:
   !-------------------------------------------------------------------------

   !  xa % u(:,:,:) = 0.0
   !  xa % v(:,:,:) = 0.0
   !  xa % t(:,:,:) = 0.0
   !  xa % q(:,:,:) = 0.0
   !  xa%psfc(:,:) = 0.0

   !  xa % p(:,:,:) = 0.0
   !  xa % rho(:,:,:) = 0.0
   !  xa % w(:,:,:) = 0.0
   !  xa % wh(:,:,:) = 0.0
   !  xa % rh(:,:,:) = 0.0
   !  xa % qt(:,:,:) = 0.0
   !  xa % qcw(:,:,:) = 0.0
   !  xa % qrn(:,:,:) = 0.0

   adj_par_lhs = sum(xa%u(its:ite,jts:jte,:)**2)/typical_u_rms**2
   adj_par_lhs = sum(xa%v(its:ite,jts:jte,:)**2)/typical_v_rms**2 + adj_par_lhs
   adj_par_lhs = sum(xa%t(its:ite,jts:jte,:)**2)/typical_t_rms**2 + adj_par_lhs
   adj_par_lhs = sum(xa%q(its:ite,jts:jte,:)**2)/typical_q_rms**2 + adj_par_lhs
   adj_par_lhs = sum(xa%psfc(its:ite,jts:jte)**2)/typical_p_rms**2 + adj_par_lhs

   adj_par_lhs = sum(xa%p(its:ite,jts:jte,:)**2)/typical_p_rms**2 + adj_par_lhs
   adj_par_lhs = sum(xa%rho(its:ite,jts:jte,:)**2)/typical_rho_rms**2 + &
      adj_par_lhs

   if (Use_RadarObs) then
      adj_par_lhs = adj_par_lhs &
                + sum(xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
                + sum(xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   end if

   if (cv_options_hum == 2) then
      adj_par_lhs = sum(xa % rh(its:ite,jts:jte,:)**2) / &
         typical_rh_rms**2 + adj_par_lhs
   end if

   if (cv_options_hum == 3) then
      adj_par_lhs = sum(xa % qcw(its:ite,jts:jte,:)**2) / &
         typical_qcw_rms**2 + adj_par_lhs
      adj_par_lhs = sum(xa % qrn(its:ite,jts:jte,:)**2) / &
         typical_qrn_rms**2 + adj_par_lhs
      adj_par_lhs = sum(xa % qt (its:ite,jts:jte,:)**2) / &
         typical_q_rms**2 + adj_par_lhs
   end if

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------
      
   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2
   xa % t(:,:,:) = xa % t(:,:,:) / typical_t_rms**2
   xa % q(:,:,:) = xa % q(:,:,:) / typical_q_rms**2
   xa%psfc(:,:) = xa%psfc(:,:) / typical_p_rms**2

   xa % p(:,:,:) = xa % p(:,:,:) / typical_p_rms**2
   xa % rho(:,:,:) = xa % rho(:,:,:) / typical_rho_rms**2

   if (Use_RadarObs) then
      xa %wh(:,:,:) = xa %wh(:,:,:) / typical_w_rms**2
      xa % w(:,:,:) = 0.0
   else
      xa %w (:,:,:) = xa %w (:,:,:) / typical_w_rms**2
   end if

   if (cv_options_hum == 2) then
      xa % rh(:,:,:) = xa % rh(:,:,:) / typical_rh_rms**2
   end if

   if (cv_options_hum == 3) then
      xa % qcw(:,:,:) = xa % qcw(:,:,:) / typical_qcw_rms**2
      xa % qrn(:,:,:) = xa % qrn(:,:,:) / typical_qrn_rms**2
      xa % qt (:,:,:) = xa % qt (:,:,:) / typical_q_rms**2
   end if
   
   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vp)

   call da_transform_vptox_adj(grid, xb, xa, vp, be, ep, xp)

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

   adj_par_rhs = sum(vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:))
   adj_par_rhs = sum(vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v3(its:ite,jts:jte,:) * vp2_v3(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v4(its:ite,jts:jte,:) * vp2_v4(its:ite,jts:jte,:)) + &
      adj_par_rhs
   adj_par_rhs = sum(vp % v5(its:ite,jts:jte,:) * vp2_v5(its:ite,jts:jte,:)) + &
      adj_par_rhs

   if (be % ne > 0) then
      adj_par_rhs = sum(vp % alpha(its:ite,jts:jte,:) * &
         vp2_alpha(its:ite,jts:jte,:)) + adj_par_rhs
   end if

   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_da_check_vptox_adjoint B:  adj_sum_lhs,adj_sum_rhs = ', &
      adj_sum_lhs,adj_sum_rhs

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >       = ', adj_sum_lhs, &
         'Whole  Domain: < vp_adj, vp > = ', adj_sum_rhs
   end if

   vp % v1(:,:,:) = vp2_v1(:,:,:)
   vp % v2(:,:,:) = vp2_v2(:,:,:)
   vp % v3(:,:,:) = vp2_v3(:,:,:)
   vp % v4(:,:,:) = vp2_v4(:,:,:)
   vp % v5(:,:,:) = vp2_v5(:,:,:)
   if (be % ne > 0) vp % alpha(:,:,:) = vp2_alpha(:,:,:)

   write(unit=stdout, fmt='(/a/)') &
        'End of da_check_vptox_adjoint.'

   if (trace_use) call da_trace_exit("da_check_vptox_adjoint")
      
end subroutine da_check_vptox_adjoint


