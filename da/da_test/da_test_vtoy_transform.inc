subroutine da_test_vtoy_transform(grid, config_flags, vp, vv, &
                                  xb, xbx, xp, be, xa, iv, y)

   !---------------------------------------------------------------------------
   !  Purpose: Perform V To Y Adjoint transform test                             
   !
   !  Method:  Perform adjoint test on complete transform: <y,y> = <v_adj,v>.
   !---------------------------------------------------------------------------

   implicit none

   type(grid_config_rec_type), intent(inout) :: config_flags
   type(domain) ,   target              :: grid
   type (vp_type),    intent(inout)     :: vv    ! Grdipt/EOF CV.
   type (vp_type),    intent(inout)     :: vp    ! Grdipt/level CV.
   type (xb_type),    intent(in)        :: xb    ! first guess (local).
   type (xbx_type),   intent(in)        :: xbx   ! Header & non-gridded vars.
   type (xpose_type), intent(inout)     :: xp    ! Dimensions and xpose buffers.
   type (be_type),    intent(in)        :: be    ! background error structure.
   type (x_type),     intent(out)       :: xa    ! analysis increments (local).
   type (ob_type),    intent(inout)     :: iv    ! ob. increment vector.
   type (y_type),     intent(inout)     :: y     ! y = h (xa)


   real, dimension(1:cv_size)        :: cv          ! Test control variable.
   real, dimension(1:cv_size)        :: cv_2

   real, dimension(ims:ime, jms:jme) :: field

   real                           :: adj_sum_lhs               ! < y, y >
   real                           :: adj_rhs,adj_sum_rhs       ! < v, v_adj >
   real                           :: partial_lhs   ! < y, y >
   real                           :: pertile_lhs   ! < y, y >

   write(unit=stdout, fmt='(/a/a)') &
        '       da_test_vtoy_transform:',&
        '---------------------------------------'


   call random_number(cv(:))
   cv(:) = cv(:) - 0.5

   cv_2(1:cv_size) = cv(1:cv_size)

   call da_zero_x(xa)
   call da_zero_vp_type(vp)
   call da_zero_vp_type(vv)

   call da_transform_vtoy(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                             grid, config_flags )

   !-------------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation and
   !       Rescale input to adjoint routine :
   !-------------------------------------------------------------------------

   call da_get_y_lhs_value( iv, y, partial_lhs, pertile_lhs, adj_sum_lhs )

   cv = 0.0

   ! call da_zero_vp_type(vp)
   ! call da_zero_vp_type(vv)
   ! call da_zero_x(xa)      

   call da_transform_vtoy_adj(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                                 grid, config_flags )

   adj_rhs = sum( cv(1:cv_size) * cv_2(1:cv_size) )
   !-------------------------------------------------------------------------
   ! Print output:
   !-------------------------------------------------------------------------
   ! FIX? with wrf_dm_sum_real

#ifdef DM_PARALLEL
  if( global ) then
     adj_sum_rhs = adj_rhs
  else
     call mpi_allreduce( adj_rhs, adj_sum_rhs, 1, true_mpi_real, mpi_sum, &
                       comm, ierr)
  end if
#else
   adj_sum_rhs = adj_rhs
   adj_sum_lhs = partial_lhs
#endif

#ifdef DM_PARALLEL
   if ( rootproc ) then
      write(unit=stdout, fmt='(A,1pe22.14)') &
         'Whole Domain  < y, y     > = ', adj_sum_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') &
         'Whole Domain  < v, v_adj > = ', adj_sum_rhs
   end if
#else
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < y, y     > = ', adj_sum_lhs
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < v, v_adj > = ', adj_sum_rhs
#endif


end subroutine da_test_vtoy_transform


