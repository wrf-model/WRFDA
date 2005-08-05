SUBROUTINE DA_Check_CvToVv_Adjoint( xb, xbx, xp, be, cv, vv, &
                                    ids,ide, jds,jde, kds,kde, &
                                    ims,ime, jms,jme, kms,kme, &
                                    its,ite, jts,jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Test Vv to Vp routine and adjoint for compatibility.
!
!  METHOD:  Standard adjoint test: < Vp, Vp > = < Vv_adj, Vv >.
!
!  HISTORY: 02/23/2000 - Creation of F90 version.           Dale Barker
!           02/04/2005 - Updated for cv_option =4           Syed RH Rizvi
!------------------------------------------------------------------------------

   IMPLICIT NONE
      
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers.
   type (be_type), intent(in)        :: be    ! background error structure.
   real,dimension(cv_size), &
                   intent(inout)     :: cv    ! control variable (local).
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).

   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.


!-------------------------------------------------------------------------

   REAL                           :: adj_par_lhs ! < Vv, Vv >
   REAL                           :: adj_par_rhs ! < cv_adj, cv >

   REAL                           :: adj_sum_lhs ! < Vv, Vv >
   REAL                           :: adj_sum_rhs ! < cv_adj, cv >

!-------------------------------------------------------------------------

   real, dimension(cv_size) :: cv_2

!-------------------------------------------------------------------------

   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------
!  [1.0] Initialise:
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/)') &
        'DA_Check_CvToVv_Adjoint: Test Results:'
      
!-------------------------------------------------------------------------
!  [2.0] Perform Vp = U_v Vv transform:
!-------------------------------------------------------------------------

   cv_2(1:cv_size) = cv(1:cv_size)
   if( cv_options == 2 .or. cv_options == 5 ) &
   call da_transform_vtovv( xb, be, cv, vv, xp,         &
                            ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte )
   if( cv_options == 4) &
       call DA_Transform_VToVv_Global(xb, xbx, be, cv,vv, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   adj_par_lhs = SUM( vv % v1(its:ite,jts:jte,1:kz_vv(1))**2 ) &
               + SUM( vv % v2(its:ite,jts:jte,1:kz_vv(2))**2 ) &
               + SUM( vv % v3(its:ite,jts:jte,1:kz_vv(3))**2 ) &
               + SUM( vv % v4(its:ite,jts:jte,1:kz_vv(4))**2 ) &
               + SUM( vv % v5(its:ite,jts:jte,1:kz_vv(5))**2 )

!-------------------------------------------------------------------------
!  [4.0] Calculate LHS of adjoint test equation:
!-------------------------------------------------------------------------

   if( cv_options == 2 .or. cv_options == 5 ) &
   call DA_Transform_VToVv_Adj( xb, be, vv, cv, xp, &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte )
   if( cv_options == 4) &
       call DA_Transform_VToVv_Global_Adj(xb, xbx, be, cv,vv, &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte )


   adj_par_rhs = SUM( cv(1:cv_size) * cv_2(1:cv_size) )

!-------------------------------------------------------------------------
!  [5.0] Print output:
!-------------------------------------------------------------------------

   if( .not. global) then
   write(unit=*, fmt='(a,1pe22.14)') &
        'Single Domain: < Vv, Vv >     = ', adj_par_lhs, &
        'Single Domain: < cv_adj, cv > = ', adj_par_rhs
   endif


#ifdef DM_PARALLEL
   call MPI_ALLREDUCE( adj_par_lhs, adj_sum_lhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   if( global) then
   adj_sum_rhs = adj_par_rhs
   else
   call MPI_ALLREDUCE( adj_par_rhs, adj_sum_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       MPI_COMM_WORLD, IERROR )
   endif  
   IF ( wrf_dm_on_monitor() ) THEN
      write(unit=*, fmt='(/)')
      write(unit=*, fmt='(a,1pe22.14)') &
           'Whole  Domain: < Vv, Vv >     = ', adj_sum_lhs, &
           'Whole  Domain: < cv_adj, cv > = ', adj_sum_rhs
   ENDIF
#else
   adj_sum_lhs = adj_par_lhs
   adj_sum_rhs = adj_par_rhs

   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a,1pe22.14)') &
        'Whole  Domain: < Vv, Vv >     = ', adj_sum_lhs, &
        'Whole  Domain: < cv_adj, cv > = ', adj_sum_rhs
#endif
      
   write(unit=*, fmt='(/a/)') &
        'DA_Check_CvToVv_Adjoint: Test Finished.'

END SUBROUTINE DA_Check_CvToVv_Adjoint
