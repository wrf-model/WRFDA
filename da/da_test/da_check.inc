subroutine da_check(cv_size, xb, xbx, be, ep, iv, &
                     xa, vv, vp, xp, ob, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: cv_size ! Size of cv array.
   type (xb_type),    intent(in)    :: xb    ! first guess (local).
   type (xbx_type),   intent(in)    :: xbx   ! Header & non-gridded vars.
   type (be_type),    intent(in)    :: be    ! background error structure.
   type (ep_type),    intent(in)    :: ep    ! Ensemble perturbation structure.
   type (ob_type),    intent(in)    :: iv    ! ob. increment vector.
   type (x_type),     intent(inout) :: xa    ! analysis increments (local).
   type (vp_type),    intent(inout) :: vv    ! Grdipt/EOF CV.
   type (vp_type),    intent(inout) :: vp    ! Grdipt/level CV.
   type (xpose_type), intent(inout) :: xp    ! Dimensions and xpose buffers. 
   type (y_type),     intent(inout) :: ob            ! yo
   type (y_type),     intent(inout) :: y             ! y = h (xa)

   integer :: sizec
   real    :: cvtest(1:cv_size)    ! background error structure.
   real    :: field(its:ite,jts:jte) ! Field for spectral transform test.

   call da_trace_entry("da_check")

   !----------------------------------------------------------------------------
   ! [1] Set up test data:
   !----------------------------------------------------------------------------

   ! Initialize cv values with random data:
   call random_number(cvtest(:))
   cvtest(:) = cvtest(:) - 0.5

   ! vv arrays initialized already.
   ! vp arrays initialized already.

   !----------------------------------------------------------------------------
   ! [2] Perform vtox adjoint tests:
   !----------------------------------------------------------------------------

   call da_message((/"Performing vtox adjoint tests"/))

   ! v_to_vv adjoint test:

   call da_check_cvtovv_adjoint(cv_size, xb, xbx,xp, be, cvtest, vv)

   !-------------------------------------------------------------------------
   ! vv_to_vp adjoint test:
   !-------------------------------------------------------------------------

   call da_check_vvtovp_adjoint(be % ne, xb, be, vv, vp)

   !-------------------------------------------------------------------------
   ! vptox adjoint test:
   !-------------------------------------------------------------------------

   call da_check_vptox_adjoint(be % ne, xb, xbx, be, ep, xp, vp, xa)

   !-------------------------------------------------------------------------
   ! vtox adjoint test: <x,x> = <v_adj,v>
   !-------------------------------------------------------------------------

   call da_check_vtox_adjoint(cv_size, xb, xbx, be, ep, cvtest, vv, vp, xp, &
                               xa)

   !----------------------------------------------------------------------------
   ! [2] Perform xtoy adjoint tests:
   !----------------------------------------------------------------------------

   call da_message((/"Performing xtoy adjoint tests"/))

   call da_allocate_y(iv, y)
   call da_zero_x(xa)

#if 1
   call da_setup_testfield(xb, xa, xp)
#else

   ! Make cv_array random.

   call random_number(cvtest(1:cv_size))
   cvtest(1:cv_size) = cvtest(1:cv_size) - 0.5

   call da_transform_vtox(cv_size, xb, xbx, be, ep, cvtest, vv, vp, xp, xa)
#endif

   call da_check_xtoy_adjoint(xb, xa, iv, xp, y)

   call da_deallocate_y(y)

   !----------------------------------------------------------------------------
   ! [4] Perform spectral test:
   !----------------------------------------------------------------------------

   if (global) then

      call da_message((/"Performing spectral tests"/))

      call random_number(field(:,:))
      field(:,:) = field(:,:) - 0.5

      sizec = (be % max_wave+1) * (be % max_wave+2)/2
      call da_test_spectral(be % max_wave, sizec, xbx, field)

   end if

   !call wrf_shutdown
   !call da_error(__FILE__,__LINE__,(/"wrf_shutdown failed"/))

   call da_trace_exit("da_check")

end subroutine da_check


