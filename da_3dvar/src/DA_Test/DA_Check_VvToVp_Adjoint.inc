SUBROUTINE DA_Check_VvToVp_Adjoint( ne, xb, xbx,  be, vv, vp, &
                                    ids,ide, jds,jde, kds,kde, &
                                    ims,ime, jms,jme, kms,kme, &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test Vv to Vp routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < Vp, Vp > = < Vv_adj, Vv >.
!------------------------------------------------------------------------------

   IMPLICIT NONE

   integer, intent(in)               :: ne    ! Ensemble size.
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).
   type (vp_type), intent(inout)     :: vp    ! CV(i,j,k)

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   REAL                              :: adj_par_lhs ! < x, x >
   REAL                              :: adj_par_rhs ! < v_adj, v >
   REAL                              :: adj_sum_lhs ! < x, x >
   REAL                              :: adj_sum_rhs ! < v_adj, v >

   real                              :: vv2_v1(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v2(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v3(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v4(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v5(ims:ime,jms:jme)
   real                              :: vv2_alpha(ims:ime,jms:jme,1:ne)

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') 'da_check_vvtovp_adjoint: Test Results:'
      
!-------------------------------------------------------------------------
!  [2.0] Perform Vp = U_v Vv transform:
!-------------------------------------------------------------------------

   call da_vertical_transform( 'u', be, &
#ifndef DEREF_KLUDGE
                               xb % vertical_inner_product, &
#else
                               xb % vertical_inner_product(ims,jms,kms), &
#endif
                               vv, vp,  &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_lhs = SUM( vp % v1(its:ite,jts:jte,kts:kte)**2 ) * inv_typ_vp1_sumsq &
               + SUM( vp % v2(its:ite,jts:jte,kts:kte)**2 ) * inv_typ_vp2_sumsq &
               + SUM( vp % v3(its:ite,jts:jte,kts:kte)**2 ) * inv_typ_vp3_sumsq &
               + SUM( vp % v4(its:ite,jts:jte,kts:kte)**2 ) * inv_typ_vp4_sumsq &
               + SUM( vp % v5(its:ite,jts:jte)**2 ) * inv_typ_vp5_sumsq

   if ( be % ne > 0 ) then
      adj_par_lhs = adj_par_lhs + &
                    sum( vp % alpha(its:ite,jts:jte,1:be%ne)**2 ) * inv_typ_vpalpha_sumsq
   end if

!-------------------------------------------------------------------------
!  [4.0] Rescale input to adjoint routine:
!-------------------------------------------------------------------------

   vp % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte) * inv_typ_vp1_sumsq
   vp % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte) * inv_typ_vp2_sumsq
   vp % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte) * inv_typ_vp3_sumsq
   vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) * inv_typ_vp4_sumsq
   vp % v5(its:ite,jts:jte) = vp % v5(its:ite,jts:jte) * inv_typ_vp5_sumsq

   if ( be % ne ) vp % alpha(its:ite,jts:jte,1:be%ne) = vp % alpha(its:ite,jts:jte,1:be%ne) * &
                                                        inv_typ_vpalpha_sumsq

!-------------------------------------------------------------------------
!  [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------

   vv2_v1(its:ite,jts:jte,1:be%v1%mz) = vv % v1(its:ite,jts:jte,1:be%v1%mz)
   vv2_v2(its:ite,jts:jte,1:be%v2%mz) = vv % v2(its:ite,jts:jte,1:be%v2%mz)
   vv2_v3(its:ite,jts:jte,1:be%v3%mz) = vv % v3(its:ite,jts:jte,1:be%v3%mz)
   vv2_v4(its:ite,jts:jte,1:be%v4%mz) = vv % v4(its:ite,jts:jte,1:be%v4%mz)
   vv2_v5(its:ite,jts:jte) = vv % v5(its:ite,jts:jte)

   if ( be % ne > 0 ) then
      vv2_alpha(its:ite,jts:jte,1:be%ne) = vv % alpha(its:ite,jts:jte,1:be%ne)
   end if      

   call da_vertical_transform( 'u_adj', be, &
#ifndef DEREF_KLUDGE
                               xb % vertical_inner_product, &
#else
                               xb % vertical_inner_product(ims,jms,kms), &
#endif
                               vv, vp,  &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )

!-------------------------------------------------------------------------
!     [6.0] Calculate RHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_rhs = 0.0
   IF ( be % v1 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v1(its:ite,jts:jte,1:be%v1%mz) * vv2_v1(its:ite,jts:jte,1:be%v1%mz) ) + adj_par_rhs
   IF ( be % v2 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v2(its:ite,jts:jte,1:be%v2%mz) * vv2_v2(its:ite,jts:jte,1:be%v2%mz) ) + adj_par_rhs
   IF ( be % v3 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v3(its:ite,jts:jte,1:be%v3%mz) * vv2_v3(its:ite,jts:jte,1:be%v3%mz) ) + adj_par_rhs
   IF ( be % v4 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v4(its:ite,jts:jte,1:be%v4%mz) * vv2_v4(its:ite,jts:jte,1:be%v4%mz) ) + adj_par_rhs
   IF ( be % v5 % mz == 1 ) &
      adj_par_rhs = SUM( vv % v5(its:ite,jts:jte) * vv2_v5(its:ite,jts:jte) ) + adj_par_rhs
   IF ( be % ne > 0 ) &
      adj_par_rhs = SUM( vv % alpha(its:ite,jts:jte,1:be%ne) * vv2_alpha(its:ite,jts:jte,1:be%ne)) + adj_par_rhs

!-------------------------------------------------------------------------
!  [7.0] Print output:
!-------------------------------------------------------------------------

   write(UNIT=stdout, fmt='(a,1pe22.14)') &
        'Single domain < vp,     vp > = ', adj_par_lhs, &
        'Single domain < Vv_adj, Vv > = ', adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real( adj_par_lhs )
   adj_sum_rhs = wrf_dm_sum_real( adj_par_rhs )
PRINT *,'TEST_COVERAGE_DA_Check_VvToVp_Adjoint:  adj_sum_lhs,adj_sum_rhs = ', adj_sum_lhs,adj_sum_rhs

   IF ( wrf_dm_on_monitor() ) THEN
      write(UNIT=stdout, fmt='(/)')
      write(UNIT=stdout, fmt='(a,1pe22.14)') &
           'Whole  Domain: < Vp, Vp >     = ', adj_sum_lhs, &
           'Whole  Domain: < Vv_adj, Vv > = ', adj_sum_rhs
   ENDIF
      
   vv % v1(its:ite,jts:jte,1:be%v1%mz) = vv2_v1(its:ite,jts:jte,1:be%v1%mz)
   vv % v2(its:ite,jts:jte,1:be%v2%mz) = vv2_v2(its:ite,jts:jte,1:be%v2%mz)
   vv % v3(its:ite,jts:jte,1:be%v3%mz) = vv2_v3(its:ite,jts:jte,1:be%v3%mz)
   vv % v4(its:ite,jts:jte,1:be%v4%mz) = vv2_v4(its:ite,jts:jte,1:be%v4%mz)
   vv % v5(its:ite,jts:jte) = vv2_v5(its:ite,jts:jte)

   if ( be % ne > 0 ) then
      vv % alpha(its:ite,jts:jte,1:be%ne) = vv2_alpha(its:ite,jts:jte,1:be%ne)
   end if

   write(UNIT=stdout, fmt='(/a/)') 'DA_Check_VvToVp_Adjoint: Test Finished.'

END SUBROUTINE DA_Check_VvToVp_Adjoint

