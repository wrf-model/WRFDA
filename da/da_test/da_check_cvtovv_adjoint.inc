subroutine da_check_cvtovv_adjoint( cv_size, xb, xbx, xp, be, cv, vv, &
                                    ids,ide, jds,jde, kds,kde, &
                                    ims,ime, jms,jme, kms,kme, &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test vtovv routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < vv, vv > = < cv_adj, cv >.
!------------------------------------------------------------------------------

   IMPLICIT NONE

   integer, intent(in)               :: cv_size ! Size of cv array.      
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers.
   type (be_type), intent(in)        :: be    ! background error structure.
   real, intent(in)                  :: cv(1:cv_size) ! control variable.
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   REAL                              :: adj_par_lhs ! < Vv, Vv >
   REAL                              :: adj_par_rhs ! < cv_adj, cv >
   REAL                              :: adj_sum_lhs ! < Vv, Vv >
   REAL                              :: adj_sum_rhs ! < cv_adj, cv >
   real                              :: cv2(1:cv_size)! control variable.

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') 'DA_Check_CvToVv_Adjoint: Test Results:'
      
!-------------------------------------------------------------------------
!  [2.0] Perform Vp = U_v Vv transform:
!-------------------------------------------------------------------------

   if ( global ) then
      call da_transform_vtovv_global( cv_size, xb, xbx, be, cv, vv, &
                                      ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte )
   else
      call da_transform_vtovv( cv_size, xb, be, cv, vv, xp,         &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
   end if

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_lhs = SUM( vv % v1(its:ite,jts:jte,1:be%v1%mz)**2 ) &
               + SUM( vv % v2(its:ite,jts:jte,1:be%v2%mz)**2 ) &
               + SUM( vv % v3(its:ite,jts:jte,1:be%v3%mz)**2 ) &
               + SUM( vv % v4(its:ite,jts:jte,1:be%v4%mz)**2 ) &
               + SUM( vv % v5(its:ite,jts:jte,1:be%v5%mz)**2 )

   if ( be % ne > 0 ) then
      adj_par_lhs = adj_par_lhs + sum( vv % alpha(its:ite,jts:jte,1:be%ne)**2 )
   end if

!-------------------------------------------------------------------------
!  [4.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------

   if ( global ) then
       call da_transform_vtovv_global_adj( cv_size, xb, xbx, be, cv2, vv, &
                                           ids, ide, jds, jde, kds, kde, &
                                           ims, ime, jms, jme, kms, kme, &
                                           its, ite, jts, jte, kts, kte )
   else
      call da_transform_vtovv_adj( cv_size, xb, be, cv2, vv, xp, &
                                   ids,ide, jds,jde, kds,kde,  &
                                   ims,ime, jms,jme, kms,kme,  &
                                   its,ite, jts,jte, kts,kte )
   end if

   adj_par_rhs = SUM( cv(1:cv_size) * cv2(1:cv_size) )

!-------------------------------------------------------------------------
!  [5.0] Print output:
!-------------------------------------------------------------------------

   if( .not. global) then
   write(unit=stdout, fmt='(a,1pe22.14)') &
        'Single Domain: < Vv, Vv >     = ', adj_par_lhs, &
        'Single Domain: < cv_adj, cv > = ', adj_par_rhs
   endif

   adj_sum_lhs = wrf_dm_sum_real( adj_par_lhs )

   if( global) then
      adj_sum_rhs = adj_par_rhs
   else
      adj_sum_rhs = wrf_dm_sum_real( adj_par_rhs )
   endif  

   WRITE(UNIT=stdout,FMT='(A,2F12.2)') &
     'TEST_COVERAGE_DA_Check_CvToVv_Adjoint:  adj_sum_lhs,adj_sum_rhs = ', &
     adj_sum_lhs,adj_sum_rhs
   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
           'Whole  Domain: < Vv, Vv >     = ', adj_sum_lhs, &
           'Whole  Domain: < cv_adj, cv > = ', adj_sum_rhs
   ENDIF
      
   write(unit=stdout, fmt='(/a/)') &
        'DA_Check_CvToVv_Adjoint: Test Finished.'

end subroutine da_check_cvtovv_adjoint
