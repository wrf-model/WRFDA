SUBROUTINE DA_Test_VToY_Transform(grid, config_flags, vp, vv, &
                                  xb, xbx, xp, be, xa, iv, y,&
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte  )

!------------------------------------------------------------------------------
!  PURPOSE: Perform V To Y Adjoint transform test                             
!
!  METHOD:  Perform adjoint test on complete transform: <y,y> = <v_adj,v>.
!
!  HISTORY: 07/21/2005            Syed  RH  Rizvi
!
!------------------------------------------------------------------------------
#ifdef DM_PARALLEL
   USE module_dm, ONLY : mpi_comm_local
#endif

   IMPLICIT NONE

   type(grid_config_rec_type), intent(inout) :: config_flags
   type(domain) ,   TARGET              :: grid
   type (vp_type),    intent(inout)     :: vv    ! Grdipt/EOF CV.
   type (vp_type),    intent(inout)     :: vp    ! Grdipt/level CV.
   type (xb_type),    intent(in)        :: xb    ! first guess (local).
   type (xbx_type),   intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)     :: xp    ! Dimensions and xpose buffers.
   type (be_type),    intent(in)        :: be    ! background error structure.
   type (x_type),     intent(out)       :: xa    ! analysis increments (local).
   type (ob_type),    intent(inout)     :: iv    ! ob. increment vector.
   TYPE (y_type),     intent(inout)     :: y     ! y = h (xa)


   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.
!-------------------------------------------------------------------------

   real, dimension(1:cv_size)        :: cv          ! Test control variable.
   real, dimension(1:cv_size)        :: cv_2

   real, dimension(ims:ime, jms:jme) :: field
!-------------------------------------------------------------------------
   integer                        :: ierror        ! MPI error code.

!-------------------------------------------------------------------------
   REAL                           :: adj_sum_lhs               ! < y, y >
   REAL                           :: adj_rhs,adj_sum_rhs       ! < v, v_adj >
   REAL                           :: partial_lhs   ! < y, y >
   REAL                           :: pertile_lhs   ! < y, y >
!-------------------------------------------------------------------------

   write(unit=*, fmt='(/a/a)') &
        '       DA_Test_VToY_Transform:',&
        '---------------------------------------'


   call random_number(cv(:))
   cv(:) = cv(:) - 0.5

   cv_2(1:cv_size) = cv(1:cv_size)

   call DA_zero_x(xa)
   call da_zero_vp_type(vp)
   call da_zero_vp_type(vv)

   call DA_Transform_VToY(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte,  &
                             grid, config_flags )

!-------------------------------------------------------------------------
!  [3.0] Calculate LHS of adjoint test equation and
!        Rescale input to adjoint routine :
!-------------------------------------------------------------------------

   call da_get_y_lhs_value( iv, y, partial_lhs, pertile_lhs, adj_sum_lhs )

!-------------------------------------------------------------------------

   cv = 0.

!   call da_zero_vp_type(vp)
!   call da_zero_vp_type(vv)
!   call da_zero_x(xa)      
!
   call DA_Transform_VToY_Adj(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 its, ite, jts, jte, kts, kte,     &
                                 grid, config_flags )

   adj_rhs = SUM( cv(1:cv_size) * cv_2(1:cv_size) )
!-------------------------------------------------------------------------
!      Print output:
!-------------------------------------------------------------------------
#ifdef DM_PARALLEL
  if( global ) then
   adj_sum_rhs = adj_rhs
  else
   call MPI_ALLREDUCE( adj_rhs, adj_sum_rhs, 1, TRUE_MPI_REAL, MPI_SUM, &
                       mpi_comm_local, IERROR )
  end if
#else
   adj_sum_rhs = adj_rhs
   adj_sum_lhs = partial_lhs
#endif

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
   write(unit=*, fmt='(A,1pe22.14)') 'Whole Domain  < y, y     > = ', adj_sum_lhs
   write(unit=*, fmt='(A,1pe22.14)') 'Whole Domain  < v, v_adj > = ', adj_sum_rhs
   END IF
#else
   write(unit=*, fmt='(A,1pe22.14)') 'Whole Domain  < y, y     > = ', adj_sum_lhs
   write(unit=*, fmt='(A,1pe22.14)') 'Whole Domain  < v, v_adj > = ', adj_sum_rhs
#endif


END SUBROUTINE DA_Test_VToY_Transform
