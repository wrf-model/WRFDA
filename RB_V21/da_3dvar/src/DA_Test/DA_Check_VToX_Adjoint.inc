SUBROUTINE DA_Check_VToX_Adjoint( xb, xbx, be, cv1, vv, vp, xp, xa, &
                                 ids, ide, jds, jde, kds, kde,      &
                                 ims, ime, jms, jme, kms, kme,      &
                                 its, ite, jts, jte, kts, kte )
                                  

!------------------------------------------------------------------------------
!  PURPOSE: Test V to X routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < x, x > = < v_adj, v >.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(in)         :: xb     ! first guess (local).
   type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   real,dimension(cv_size),intent(in) :: cv1    ! control variable (local).
   type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
   type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
   type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
   type (x_type) , intent(inout)      :: xa     ! gridded analy. incs. (local)

      
   integer, intent(in)                :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)                :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)                :: its,ite, jts,jte, kts,kte ! tile   dims.

   real,dimension(cv_size) :: cv2 ! control variable (local).

!-------------------------------------------------------------------------

   INTEGER                        :: k           ! Loop counter.

   REAL                           :: adj_par_lhs ! < x, x >
   REAL                           :: adj_par_rhs ! < x, x >

   REAL                           :: adj_sum_lhs ! < x, x >
   REAL                           :: adj_sum_rhs ! < v_adj, v >

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        'DA_Check_VToX_Adjoint: Adjoint Test Results:'
      
!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   cv2(:) = 0.0
      
!-------------------------------------------------------------------------
!  [2.0] Perform x = U v transform:
!-------------------------------------------------------------------------

   call da_zero_x ( xa )
   call da_transform_vtox( xb, xbx, be, cv1, vv, vp, xp, xa, &
                          ids, ide, jds, jde, kds, kde,      &
                          ims, ime, jms, jme, kms, kme,      &
                          its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_lhs = SUM( xa % u(its:ite, jts:jte, kts:kte)**2 ) / typical_u_rms**2 &
               + SUM( xa % v(its:ite, jts:jte, kts:kte)**2 ) / typical_v_rms**2 &
               + SUM( xa % p(its:ite, jts:jte, kts:kte)**2 ) / typical_p_rms**2 &
               + SUM( xa % t(its:ite, jts:jte, kts:kte)**2 ) / typical_t_rms**2 &
               + SUM( xa % q(its:ite, jts:jte, kts:kte)**2 ) / typical_q_rms**2 &
               + SUM( xa % rho(its:ite, jts:jte, kts:kte)**2 ) / typical_rho_rms**2 &
               + SUM( xa % psfc(its:ite, jts:jte)**2 ) / typical_p_rms**2

   IF ( cv_options_hum == 2 ) THEN
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % rh(its:ite, jts:jte, kts:kte)**2 ) / typical_rh_rms**2
   END IF

   IF ( cv_options_hum == 3 ) THEN
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % qcw(its:ite, jts:jte, kts:kte)**2)/typical_qcw_rms**2 &
                  + SUM( xa % qrn(its:ite, jts:jte, kts:kte)**2)/typical_qrn_rms**2
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % qt (its:ite, jts:jte, kts:kte)**2)/typical_q_rms**2
   END IF

   IF ( Use_RadarObs ) THEN
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
                  + SUM( xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   END IF

!-------------------------------------------------------------------------
!  [4.0] Rescale input to adjoint routine:
!-------------------------------------------------------------------------

   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2
   xa % p(:,:,:) = xa % p(:,:,:) / typical_p_rms**2
   xa % t(:,:,:) = xa % t(:,:,:) / typical_t_rms**2
   xa % q(:,:,:) = xa % q(:,:,:) / typical_q_rms**2
   xa % rho(:,:,:) = xa % rho(:,:,:) / typical_rho_rms**2

   xa % psfc(:,:) = xa % psfc(:,:) / typical_p_rms**2

   IF ( cv_options_hum == 2 ) THEN
      xa % rh(:,:,:) = xa % rh(:,:,:) / typical_rh_rms**2
   END IF

   IF ( cv_options_hum == 3 ) THEN
      xa % qcw(:,:,:) = xa % qcw(:,:,:) / typical_qcw_rms**2
      xa % qrn(:,:,:) = xa % qrn(:,:,:) / typical_qrn_rms**2
      xa % qt (:,:,:) = xa % qt (:,:,:) / typical_q_rms**2
   END IF

   IF ( Use_RadarObs ) THEN
      xa %wh(:,:,:) = xa %wh(:,:,:) / typical_w_rms**2
      xa % w(:,:,:) = 0.0
   else
      xa %w (:,:,:) = xa %w (:,:,:) / typical_w_rms**2
   END IF
!-------------------------------------------------------------------------
!  [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------
   call da_transform_vtox_adj( xb, xbx, be, xa, xp, vp, vv, cv2, &
                              ids, ide, jds, jde, kds, kde,      &
                              ims, ime, jms, jme, kms, kme,      &
                              its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  [6.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_rhs = SUM( cv1(1:cv_size) * cv2(1:cv_size) )

!-------------------------------------------------------------------------
!  [7.0] Print output:
!-------------------------------------------------------------------------

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( adj_par_lhs, adj_sum_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   if( global ) then
   adj_sum_rhs = adj_par_rhs
   else
   call MPI_ALLREDUCE( adj_par_rhs, adj_sum_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   end if

   IF ( wrf_dm_on_monitor() ) THEN
   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a,1pe22.14)') &
        'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
        'Whole  Domain: < v_adj, v > = ', adj_sum_rhs
   ENDIF
#else
   adj_sum_lhs = adj_par_lhs
   adj_sum_rhs = adj_par_rhs

   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a,1pe22.14)') &
        'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
        'Whole  Domain: < v_adj, v > = ', adj_sum_rhs
#endif

   write(unit=*, fmt='(/a/)') &
        'DA_Check_VToX_Adjoint: Finished'

END SUBROUTINE DA_Check_VToX_Adjoint

