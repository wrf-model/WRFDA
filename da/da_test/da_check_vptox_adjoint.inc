SUBROUTINE DA_Check_VpToX_Adjoint( ne, xb, xbx, be, ep, xp, vp, xa, &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test Vp to X routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < x, x > = < v_adj, v >.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: ne   ! Ensemble size.
   type (xb_type), intent(in)       :: xb   ! first guess.
   TYPE (xbx_type),intent(in)       :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)       :: be   ! background errors.
   type (ep_type), intent(in)       :: ep   ! Ensemble perturbation type.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   type (vp_type),intent(inout)     :: vp   ! grdipt/level cv (local).
   type (x_type), intent(inout)     :: xa   ! grad_x(jo)
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

   REAL                             :: adj_par_lhs ! < x, x >
   REAL                             :: adj_par_rhs ! < v_adj, v >
   REAL                             :: adj_sum_lhs ! < x, x >
   REAL                             :: adj_sum_rhs ! < v_adj, v >
   real                             :: vp2_v1(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v2(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v3(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v4(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_v5(ims:ime,jms:jme,kms:kme)
   real                             :: vp2_alpha(ims:ime,jms:jme,1:ne)

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') 'da_check_vptox_adjoint: Test Results:'

   CALL DA_Zero_X( xa )

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)

   CALL DA_PsiChi_To_UV(vp % v1, vp % v2, xb % coefx, &
                        xb % coefy , xa % u, xa % v, &
                        ids,ide, jds,jde, kds,kde, &
                        ims,ime, jms,jme, kms,kme, &
                        its,ite, jts,jte, kts,kte )

   adj_par_lhs = SUM( xa % u(its:ite,jts:jte,:)**2 ) / typical_u_rms**2
   adj_par_lhs = SUM( xa % v(its:ite,jts:jte,:)**2 ) / typical_v_rms**2 + adj_par_lhs

   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2

   vp%v1(:,:,:)=0.
   vp%v2(:,:,:)=0.

   CALL DA_PsiChi_To_UV_Adj( xa % u, xa % v, xb % coefx,   &
                             xb % coefy, vp % v1, vp % v2, &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte )

   adj_par_rhs = SUM( vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:) )
   adj_par_rhs = SUM( vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:) ) + adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real( adj_par_lhs )
   adj_sum_rhs = wrf_dm_sum_real( adj_par_rhs )
PRINT *,'TEST_COVERAGE_DA_Check_VpToX_Adjoint A:  adj_sum_lhs,adj_sum_rhs = ', adj_sum_lhs,adj_sum_rhs

   IF ( wrf_dm_on_monitor() ) THEN
      write(UNIT=stdout, fmt='(/a/)') &
           ' DA_Check_DA_PsiChi_To_UV: Adjoint Test Results:'

      write(UNIT=stdout, fmt='(/)')
      write(UNIT=stdout, fmt='(a,1pe22.14)') &
           'Whole  Domain: < u_v,     u_v         > = ', adj_sum_lhs, &
           'Whole  Domain: < psi_chi, psi_chi_adj > = ', adj_sum_rhs
   ENDIF

   vp%v1(:,:,:) = vp2_v1(:,:,:)
   vp%v2(:,:,:) = vp2_v2(:,:,:)

!-------------------------------------------------------------------------

   CALL DA_Zero_X( xa )

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)
   vp2_v3(:,:,:) = vp % v3(:,:,:)
   vp2_v4(:,:,:) = vp % v4(:,:,:)
   vp2_v5(:,:,:) = vp % v5(:,:,:)
   if ( be % ne > 0 ) vp2_alpha(:,:,:) = vp % alpha(:,:,:)

!-------------------------------------------------------------------------
!  [2.0] Perform x = U vp transform:
!-------------------------------------------------------------------------

   call da_transform_vptox( xb, xbx, vp, xp, xa, be, ep, &
                            ids,ide, jds,jde, kds,kde,   &
                            ims,ime, jms,jme, kms,kme,   &
                            its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
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

   adj_par_lhs = SUM( xa % u(its:ite,jts:jte,:)**2 ) / typical_u_rms**2
   adj_par_lhs = SUM( xa % v(its:ite,jts:jte,:)**2 ) / typical_v_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % t(its:ite,jts:jte,:)**2 ) / typical_t_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % q(its:ite,jts:jte,:)**2 ) / typical_q_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % psfc(its:ite,jts:jte)**2 ) / typical_p_rms**2 + adj_par_lhs

   adj_par_lhs = SUM( xa % p(its:ite,jts:jte,:)**2 ) / typical_p_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % rho(its:ite,jts:jte,:)**2 ) / typical_rho_rms**2 + adj_par_lhs

   IF ( Use_RadarObs ) THEN
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   END IF

   IF ( cv_options_hum == 2 ) THEN
      adj_par_lhs = SUM( xa % rh(its:ite,jts:jte,:)**2 ) / typical_rh_rms**2 + adj_par_lhs
   END IF

   IF ( cv_options_hum == 3 ) THEN
   adj_par_lhs = SUM( xa % qcw(its:ite,jts:jte,:)**2 ) / typical_qcw_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % qrn(its:ite,jts:jte,:)**2 ) / typical_qrn_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % qt (its:ite,jts:jte,:)**2 ) / typical_q_rms**2 + adj_par_lhs
   END IF

!-------------------------------------------------------------------------
!  [4.0] Rescale input to adjoint routine:
!-------------------------------------------------------------------------
      
   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2
   xa % t(:,:,:) = xa % t(:,:,:) / typical_t_rms**2
   xa % q(:,:,:) = xa % q(:,:,:) / typical_q_rms**2
   xa%psfc(:,:) = xa%psfc(:,:) / typical_p_rms**2

   xa % p(:,:,:) = xa % p(:,:,:) / typical_p_rms**2
   xa % rho(:,:,:) = xa % rho(:,:,:) / typical_rho_rms**2

   IF ( Use_RadarObs ) THEN
      xa %wh(:,:,:) = xa %wh(:,:,:) / typical_w_rms**2
      xa % w(:,:,:) = 0.0
   else
      xa %w (:,:,:) = xa %w (:,:,:) / typical_w_rms**2
   END IF

   IF ( cv_options_hum == 2 ) THEN
      xa % rh(:,:,:) = xa % rh(:,:,:) / typical_rh_rms**2
   END IF

   IF ( cv_options_hum == 3 ) THEN
   xa % qcw(:,:,:) = xa % qcw(:,:,:) / typical_qcw_rms**2
   xa % qrn(:,:,:) = xa % qrn(:,:,:) / typical_qrn_rms**2
   xa % qt (:,:,:) = xa % qt (:,:,:) / typical_q_rms**2
   END IF
   
!-------------------------------------------------------------------------
!  [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------

   call DA_Zero_vp_type ( vp )

   call da_transform_vptox_adj( xb, xbx, xa, vp, be, ep, xp, &
                                ids,ide, jds,jde, kds,kde,   &
                                ims,ime, jms,jme, kms,kme,   &
                                its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [6.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_rhs = SUM( vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:) )
   adj_par_rhs = SUM( vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v3(its:ite,jts:jte,:) * vp2_v3(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v4(its:ite,jts:jte,:) * vp2_v4(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v5(its:ite,jts:jte,:) * vp2_v5(its:ite,jts:jte,:) ) + adj_par_rhs
   if ( be % ne > 0 ) then
      adj_par_rhs = SUM( vp % alpha(its:ite,jts:jte,:) * vp2_alpha(its:ite,jts:jte,:) ) + adj_par_rhs
   end if

!-------------------------------------------------------------------------
!  [7.0] Print output:
!-------------------------------------------------------------------------

   adj_sum_lhs = wrf_dm_sum_real( adj_par_lhs )
   adj_sum_rhs = wrf_dm_sum_real( adj_par_rhs )
PRINT *,'TEST_COVERAGE_DA_Check_VpToX_Adjoint B:  adj_sum_lhs,adj_sum_rhs = ', adj_sum_lhs,adj_sum_rhs

   IF ( wrf_dm_on_monitor() ) THEN
      write(UNIT=stdout, fmt='(/)')
      write(UNIT=stdout, fmt='(a,1pe22.14)') &
           'Whole  Domain: < x, x >       = ', adj_sum_lhs, &
           'Whole  Domain: < vp_adj, vp > = ', adj_sum_rhs
   ENDIF

   vp % v1(:,:,:) = vp2_v1(:,:,:)
   vp % v2(:,:,:) = vp2_v2(:,:,:)
   vp % v3(:,:,:) = vp2_v3(:,:,:)
   vp % v4(:,:,:) = vp2_v4(:,:,:)
   vp % v5(:,:,:) = vp2_v5(:,:,:)
   if ( be % ne > 0 ) vp % alpha(:,:,:) = vp2_alpha(:,:,:)

   write(UNIT=stdout, fmt='(/a/)') &
        'End of DA_Check_VpToX_Adjoint.'
      
END SUBROUTINE DA_Check_VpToX_Adjoint

