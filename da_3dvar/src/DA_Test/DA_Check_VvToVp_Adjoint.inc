SUBROUTINE DA_Check_VvToVp_Adjoint( xb, xbx, xp, be, vp, vv, &
                                    ids,ide, jds,jde, kds,kde, &
                                    ims,ime, jms,jme, kms,kme, &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test Vv to Vp routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < Vp, Vp > = < Vv_adj, Vv >.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!------------------------------------------------------------------------------

   IMPLICIT NONE
      
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   TYPE (vp_type), intent(inout)     :: vp          ! CV(i,j,k)

!-------------------------------------------------------------------------

   REAL                           :: adj_par_lhs ! < x, x >
   REAL                           :: adj_par_rhs ! < v_adj, v >

   REAL                           :: adj_sum_lhs ! < x, x >
   REAL                           :: adj_sum_rhs ! < v_adj, v >

!-------------------------------------------------------------------------

   real, dimension(ims:ime, jms:jme, kms:kme) :: vv2_v1, vv2_v2, vv2_v3, &
                                                 vv2_v4, vv2_v5

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        'DA_Check_VvToVp_Adjoint: Adjoint Test Results:'
      
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

   adj_par_lhs = SUM( vp % v1(its:ite,jts:jte,1:kz_vp(1))**2 ) * inv_typ_vp1_sumsq &
               + SUM( vp % v2(its:ite,jts:jte,1:kz_vp(2))**2 ) * inv_typ_vp2_sumsq &
               + SUM( vp % v3(its:ite,jts:jte,1:kz_vp(3))**2 ) * inv_typ_vp3_sumsq &
               + SUM( vp % v4(its:ite,jts:jte,1:kz_vp(4))**2 ) * inv_typ_vp4_sumsq &
               + SUM( vp % v5(its:ite,jts:jte,1:kz_vp(5))**2 ) * inv_typ_vp5_sumsq

!-------------------------------------------------------------------------
!  [4.0] Rescale input to adjoint routine:
!-------------------------------------------------------------------------
      
   vp % v1(its:ite,jts:jte,1:kz_vp(1)) = vp % v1(its:ite,jts:jte,1:kz_vp(1)) * inv_typ_vp1_sumsq
   vp % v2(its:ite,jts:jte,1:kz_vp(2)) = vp % v2(its:ite,jts:jte,1:kz_vp(2)) * inv_typ_vp2_sumsq
   vp % v3(its:ite,jts:jte,1:kz_vp(3)) = vp % v3(its:ite,jts:jte,1:kz_vp(3)) * inv_typ_vp3_sumsq
   vp % v4(its:ite,jts:jte,1:kz_vp(4)) = vp % v4(its:ite,jts:jte,1:kz_vp(4)) * inv_typ_vp4_sumsq
   vp % v5(its:ite,jts:jte,1:kz_vp(5)) = vp % v5(its:ite,jts:jte,1:kz_vp(5)) * inv_typ_vp5_sumsq

!-------------------------------------------------------------------------
!  [5.0] Perform adjoint operation:
!-------------------------------------------------------------------------
      
   vv2_v1(its:ite,jts:jte,1:kz_vv(1)) = vv % v1(its:ite,jts:jte,1:kz_vv(1))
   vv2_v2(its:ite,jts:jte,1:kz_vv(2)) = vv % v2(its:ite,jts:jte,1:kz_vv(2))
   vv2_v3(its:ite,jts:jte,1:kz_vv(3)) = vv % v3(its:ite,jts:jte,1:kz_vv(3))
   vv2_v4(its:ite,jts:jte,1:kz_vv(4)) = vv % v4(its:ite,jts:jte,1:kz_vv(4))
   vv2_v5(its:ite,jts:jte,1:kz_vv(5)) = vv % v5(its:ite,jts:jte,1:kz_vv(5))

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
      adj_par_rhs = SUM( vv % v1(its:ite,jts:jte,1:kz_vv(1)) * vv2_v1(its:ite,jts:jte,1:kz_vv(1)) ) + adj_par_rhs
   IF ( be % v2 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v2(its:ite,jts:jte,1:kz_vv(2)) * vv2_v2(its:ite,jts:jte,1:kz_vv(2)) ) + adj_par_rhs
   IF ( be % v3 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v3(its:ite,jts:jte,1:kz_vv(3)) * vv2_v3(its:ite,jts:jte,1:kz_vv(3)) ) + adj_par_rhs
   IF ( be % v4 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v4(its:ite,jts:jte,1:kz_vv(4)) * vv2_v4(its:ite,jts:jte,1:kz_vv(4)) ) + adj_par_rhs
   IF ( be % v5 % mz > 0 ) &
      adj_par_rhs = SUM( vv % v5(its:ite,jts:jte,1:kz_vv(5)) * vv2_v5(its:ite,jts:jte,1:kz_vv(5)) ) + adj_par_rhs
     
!-------------------------------------------------------------------------
!  [7.0] Print output:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(a,1pe22.14)') &
        'Single domain < vp,     vp > = ', adj_par_lhs, &
        'Single domain < Vv_adj, Vv > = ', adj_par_rhs

#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( adj_par_lhs, adj_sum_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   call MPI_ALLREDUCE( adj_par_rhs, adj_sum_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )

   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=*, fmt='(/)')
      write(unit=*, fmt='(a,1pe22.14)') &
           'Whole  Domain: < Vp, Vp >     = ', adj_sum_lhs, &
           'Whole  Domain: < Vv_adj, Vv > = ', adj_sum_rhs
   ENDIF
#else
   adj_sum_lhs = adj_par_lhs
   adj_sum_rhs = adj_par_rhs

   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a,1pe22.14)') &
        'Whole  Domain: < Vp, Vp >     = ', adj_sum_lhs, &
        'Whole  Domain: < Vv_adj, Vv > = ', adj_sum_rhs
#endif
      
   vv % v1(its:ite,jts:jte,1:kz_vv(1)) = vv2_v1(its:ite,jts:jte,1:kz_vv(1))
   vv % v2(its:ite,jts:jte,1:kz_vv(2)) = vv2_v2(its:ite,jts:jte,1:kz_vv(2))
   vv % v3(its:ite,jts:jte,1:kz_vv(3)) = vv2_v3(its:ite,jts:jte,1:kz_vv(3))
   vv % v4(its:ite,jts:jte,1:kz_vv(4)) = vv2_v4(its:ite,jts:jte,1:kz_vv(4))
   vv % v5(its:ite,jts:jte,1:kz_vv(5)) = vv2_v5(its:ite,jts:jte,1:kz_vv(5))

   write(unit=*, fmt='(/a/)') &
        'DA_Check_VvToVp_Adjoint: Test Finished.'

END SUBROUTINE DA_Check_VvToVp_Adjoint

