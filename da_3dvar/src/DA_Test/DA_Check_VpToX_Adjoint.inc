SUBROUTINE DA_Check_VpToX_Adjoint( xb, xbx, be, xa, xp, vp,  &
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

   type (xb_type), intent(in)       :: xb   ! first guess.
   TYPE (xbx_type),intent(in)       :: xbx  ! For header & non-grid arrays.
   type (be_type), intent(in)       :: be   ! background errors.
   type (x_type), intent(inout)     :: xa   ! grad_x(jo)
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers.
   type (vp_type),intent(inout)     :: vp   ! grdipt/level cv (local).
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims

!-------------------------------------------------------------------------
      
   INTEGER                        :: k           ! Loop counter.

   REAL                           :: adj_par_lhs ! < x, x >
   REAL                           :: adj_par_rhs ! < v_adj, v >

   REAL                           :: adj_sum_lhs ! < x, x >
   REAL                           :: adj_sum_rhs ! < v_adj, v >

!-------------------------------------------------------------------------

   real, dimension(ims:ime, jms:jme, kms:kme) :: vp2_v1, vp2_v2, vp2_v3, &
                                                 vp2_v4, vp2_v5

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        ' DA_Check_VpToX_Adjoint: Adjoint Test Results:'

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   CALL DA_Zero_X( xa )

!-------------------------------------------------------------------------
!  [2.0] Perform x = U vp transform:
!-------------------------------------------------------------------------

   call da_transform_vptox( xb, xbx, vp, xp, xa, be,    &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_lhs = SUM( xa % u(its:ite,jts:jte,:)**2 ) / typical_u_rms**2
   adj_par_lhs = SUM( xa % v(its:ite,jts:jte,:)**2 ) / typical_v_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % w(its:ite,jts:jte,:)**2 ) / typical_w_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % t(its:ite,jts:jte,:)**2 ) / typical_t_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % p(its:ite,jts:jte,:)**2 ) / typical_p_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % q(its:ite,jts:jte,:)**2 ) / typical_q_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % rho(its:ite,jts:jte,:)**2 ) / typical_rho_rms**2 + adj_par_lhs

   IF ( cv_options_hum == 2 ) THEN
      adj_par_lhs = SUM( xa % rh(its:ite,jts:jte,:)**2 ) / typical_rh_rms**2 + adj_par_lhs
   END IF

   IF ( cv_options_hum == 3 ) THEN
   adj_par_lhs = SUM( xa % qcw(its:ite,jts:jte,:)**2 ) / typical_qcw_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % qrn(its:ite,jts:jte,:)**2 ) / typical_qrn_rms**2 + adj_par_lhs
   adj_par_lhs = SUM( xa % qt (its:ite,jts:jte,:)**2 ) / typical_q_rms**2 + adj_par_lhs
   END IF

   adj_par_lhs = SUM( xa % psfc(its:ite,jts:jte)**2 ) / typical_p_rms**2 + adj_par_lhs

!-------------------------------------------------------------------------
!  [4.0] Rescale input to adjoint routine:
!-------------------------------------------------------------------------
      
   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2
   xa % t(:,:,:) = xa % t(:,:,:) / typical_t_rms**2
   xa % p(:,:,:) = xa % p(:,:,:) / typical_p_rms**2
   xa % q(:,:,:) = xa % q(:,:,:) / typical_q_rms**2
   xa % rho(:,:,:) = xa % rho(:,:,:) / typical_rho_rms**2
   xa % w(:,:,:) = xa % w(:,:,:) / typical_w_rms**2

   IF ( cv_options_hum == 2 ) THEN
      xa % rh(:,:,:) = xa % rh(:,:,:) / typical_rh_rms**2
   END IF

   IF ( cv_options_hum == 3 ) THEN
   xa % qcw(:,:,:) = xa % qcw(:,:,:) / typical_qcw_rms**2
   xa % qrn(:,:,:) = xa % qrn(:,:,:) / typical_qrn_rms**2
   xa % qt (:,:,:) = xa % qt (:,:,:) / typical_q_rms**2
   END IF

   xa%psfc(:,:) = xa%psfc(:,:) / typical_p_rms**2
   
!-------------------------------------------------------------------------
!  [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------

   vp2_v1(:,:,:) = vp % v1(:,:,:)
   vp2_v2(:,:,:) = vp % v2(:,:,:)
   vp2_v3(:,:,:) = vp % v3(:,:,:)
   vp2_v4(:,:,:) = vp % v4(:,:,:)
   vp2_v5(:,:,:) = vp % v5(:,:,:)

   call DA_Zero_vp_type ( vp )

   call da_transform_vptox_adj( xb, xbx, xa, vp, be, xp, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [6.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_rhs = SUM( vp % v1(its:ite,jts:jte,:) * vp2_v1(its:ite,jts:jte,:) )
   adj_par_rhs = SUM( vp % v2(its:ite,jts:jte,:) * vp2_v2(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v3(its:ite,jts:jte,:) * vp2_v3(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v4(its:ite,jts:jte,:) * vp2_v4(its:ite,jts:jte,:) ) + adj_par_rhs
   adj_par_rhs = SUM( vp % v5(its:ite,jts:jte,:) * vp2_v5(its:ite,jts:jte,:) ) + adj_par_rhs

!-------------------------------------------------------------------------
!  [7.0] Print output:
!-------------------------------------------------------------------------

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( adj_par_lhs, adj_sum_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   call MPI_ALLREDUCE( adj_par_rhs, adj_sum_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=*, fmt='(/)')
      write(unit=*, fmt='(a,1pe22.14)') &
           'Whole  Domain: < x, x >       = ', adj_sum_lhs, &
           'Whole  Domain: < vp_adj, vp > = ', adj_sum_rhs
   ENDIF
#else
   adj_sum_lhs = adj_par_lhs
   adj_sum_rhs = adj_par_rhs

   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a,1pe22.14)') &
        'Whole  Domain: < x, x >       = ', adj_sum_lhs, &
        'Whole  Domain: < vp_adj, vp > = ', adj_sum_rhs
#endif

   vp % v1(:,:,:) = vp2_v1(:,:,:)
   vp % v2(:,:,:) = vp2_v2(:,:,:)
   vp % v3(:,:,:) = vp2_v3(:,:,:)
   vp % v4(:,:,:) = vp2_v4(:,:,:)
   vp % v5(:,:,:) = vp2_v5(:,:,:)

   write(unit=*, fmt='(/a/)') &
        'End of DA_Check_VpToX_Adjoint.'
      
END SUBROUTINE DA_Check_VpToX_Adjoint

