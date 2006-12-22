subroutine da_check_vtox_adjoint( cv_size, xb, xbx, be, ep, cv1, vv, vp, xp, &
                                  xa, &
                                  ids, ide, jds, jde, kds, kde,      &
                                  ims, ime, jms, jme, kms, kme,      &
                                  its, ite, jts, jte, kts, kte )

   !---------------------------------------------------------------------------
   ! PURPOSE: Test V to X routine and adjoint for compatibility.
   !
   ! METHOD:  Standard adjoint test: < x, x > = < v_adj, v >.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: cv_size
   type (xb_type), intent(in)      :: xb     ! first guess (local).
   type (xbx_type),intent(in)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)      :: be     ! background error structure.
   type (ep_type), intent(in)      :: ep     ! Ensemble perturbation structure.
   real, intent(in)                :: cv1(1:cv_size) ! control variable (local).
   type (vp_type), intent(inout)   :: vv     ! Grdipt/EOF CV.
   type (vp_type), intent(inout)   :: vp     ! Grdipt/level CV.
   type (xpose_type), intent(inout):: xp     ! Domain decomposition vars.
   type (x_type) , intent(inout)   :: xa     ! gridded analy. incs. (local)

      
   integer, intent(in)    :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)    :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)    :: its,ite, jts,jte, kts,kte ! tile   dims.

   real                   :: cv2(1:cv_size)    ! control variable (local).
   real                   :: adj_par_lhs ! < x, x >
   real                   :: adj_par_rhs ! < x, x >
   real                   :: adj_sum_lhs ! < x, x >
   real                   :: adj_sum_rhs ! < v_adj, v >

   if (trace_use) call da_trace_entry("da_check_vtox_adjoint")

   write(unit=stdout, fmt='(/a/)') &
      'da_check_vtox_adjoint: Adjoint Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   cv2(:) = 0.0
      
   !----------------------------------------------------------------------
   ! [2.0] Perform x = U v transform:
   !----------------------------------------------------------------------

   call da_zero_x ( xa )

   call da_transform_vtox( cv_size, xb, xbx, be, ep, cv1, vv, vp, xp, xa, &
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           its, ite, jts, jte, kts, kte )

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation: 
   !----------------------------------------------------------------------

   adj_par_lhs = sum(xa % u(its:ite, jts:jte, kts:kte)**2 ) / typical_u_rms**2 &
               + sum(xa % v(its:ite, jts:jte, kts:kte)**2 ) / typical_v_rms**2 &     
               + sum(xa % p(its:ite, jts:jte, kts:kte)**2 ) / typical_p_rms**2 &     
               + sum(xa % t(its:ite, jts:jte, kts:kte)**2 ) / typical_t_rms**2 &     
               + sum(xa % q(its:ite, jts:jte, kts:kte)**2 ) / typical_q_rms**2 &     
               + sum(xa % rho(its:ite,jts:jte,kts:kte)**2)/ typical_rho_rms**2 & 
               + sum(xa % psfc(its:ite, jts:jte)**2 ) / typical_p_rms**2             

   if ( cv_options_hum == 2 ) then
      adj_par_lhs = adj_par_lhs &
              + sum( xa % rh(its:ite, jts:jte, kts:kte)**2 ) / typical_rh_rms**2
   end if

   if ( cv_options_hum == 3 ) then
      adj_par_lhs = adj_par_lhs &
             + sum( xa % qcw(its:ite, jts:jte, kts:kte)**2)/typical_qcw_rms**2 &
             + sum( xa % qrn(its:ite, jts:jte, kts:kte)**2)/typical_qrn_rms**2
      adj_par_lhs = adj_par_lhs &
             + sum( xa % qt (its:ite, jts:jte, kts:kte)**2)/typical_q_rms**2
   end if

   if ( Use_RadarObs ) then
      adj_par_lhs = adj_par_lhs &
         + sum( xa % wh (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   else
      adj_par_lhs = adj_par_lhs &
         + sum( xa % w  (its:ite, jts:jte, kts:kte)**2)/typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------

   xa % u(:,:,:) = xa % u(:,:,:) / typical_u_rms**2
   xa % v(:,:,:) = xa % v(:,:,:) / typical_v_rms**2
   xa % p(:,:,:) = xa % p(:,:,:) / typical_p_rms**2
   xa % t(:,:,:) = xa % t(:,:,:) / typical_t_rms**2
   xa % q(:,:,:) = xa % q(:,:,:) / typical_q_rms**2
   xa % rho(:,:,:) = xa % rho(:,:,:) / typical_rho_rms**2

   xa % psfc(:,:) = xa % psfc(:,:) / typical_p_rms**2

   if ( cv_options_hum == 2 ) then
      xa % rh(:,:,:) = xa % rh(:,:,:) / typical_rh_rms**2
   end if

   if ( cv_options_hum == 3 ) then
      xa % qcw(:,:,:) = xa % qcw(:,:,:) / typical_qcw_rms**2
      xa % qrn(:,:,:) = xa % qrn(:,:,:) / typical_qrn_rms**2
      xa % qt (:,:,:) = xa % qt (:,:,:) / typical_q_rms**2
   end if

   if ( Use_RadarObs ) then
      xa %wh(:,:,:) = xa %wh(:,:,:) / typical_w_rms**2
      xa % w(:,:,:) = 0.0
   else
      xa %w (:,:,:) = xa %w (:,:,:) / typical_w_rms**2
   end if

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_transform_vtox_adj( cv_size, xb, xbx, be, ep, xa, xp, vp, vv, cv2, &
                               ids, ide, jds, jde, kds, kde,      &
                               ims, ime, jms, jme, kms, kme,      &
                               its, ite, jts, jte, kts, kte )

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

   adj_par_rhs = sum( cv1(1:cv_size) * cv2(1:cv_size) )

   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   adj_sum_lhs = wrf_dm_sum_real( adj_par_lhs )

   if( global ) then
      adj_sum_rhs = adj_par_rhs
   else
      adj_sum_rhs = wrf_dm_sum_real( adj_par_rhs )
   end if
   write (unit=stdout,fmt='(A,2F10.2)') &
      'TEST_COVERAGE_da_check_vtox_adjoint:  adj_sum_lhs,adj_sum_rhs = ', &
      adj_sum_lhs,adj_sum_rhs

   if ( rootproc ) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
         'Whole  Domain: < v_adj, v > = ', adj_sum_rhs
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vtox_adjoint: Finished'

   if (trace_use) call da_trace_exit("da_check_vtox_adjoint")

end subroutine da_check_vtox_adjoint


