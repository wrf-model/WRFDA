subroutine da_calculate_jo_and_grady_synop( iv, re, jo, jo_grad_y)

   implicit none

   type (ob_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type),  intent(in)   :: re          ! Residual vector.
   type (y_type),  intent(inout):: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n
   ! the following "global" objects are used only when testing
   type (ob_type) :: iv_glob         ! Global Innovation vector (O-B).
   type (y_type)  :: re_glob         ! Global Residual vector (O-A).
   type (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

   jo % synop_u = 0.0
   jo % synop_v = 0.0
   jo % synop_t = 0.0
   jo % synop_p = 0.0
   jo % synop_q = 0.0

   if (testing_dm_exact) then
      if (iv%num_synop_glo == 0) return
   else
      if (iv%num_synop < 1) return
   end if

   do n=1, iv%num_synop

         jo_grad_y%synop(n)%u = -re%synop(n)%u / &
                               ( iv%synop(n)%u%error * iv%synop(n)%u%error)
         jo_grad_y%synop(n)%v = -re%synop(n)%v / &
                               ( iv%synop(n)%v%error * iv%synop(n)%v%error)
         jo_grad_y%synop(n)%t = -re%synop(n)%t / &
                               ( iv%synop(n)%t%error * iv%synop(n)%t%error)
         jo_grad_y%synop(n)%p = -re%synop(n)%p / &
                               ( iv%synop(n)%p%error * iv%synop(n)%p%error)
         jo_grad_y%synop(n)%q = -re%synop(n)%q / &
                               ( iv%synop(n)%q%error * iv%synop(n)%q%error)
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .false. for a serial run.
   if (testing_dm_exact) then
      ! collect all obs in serial order and allocate global objects
      call da_to_global_synop( iv,      re,      jo_grad_y, &
                           iv_glob, re_glob, jo_grad_y_glob)
      ! perform remaining computations
      call da_compute_jo_synop_uvtq( iv_glob, re_glob, jo_grad_y_glob, jo)
      ! free global objects
      call da_free_global_synop( iv_glob, re_glob, jo_grad_y_glob)
   else
      ! perform remaining computations
      call da_compute_jo_synop_uvtq( iv, re, jo_grad_y, jo)
   end if

   jo % synop_u = 0.5 * jo % synop_u
   jo % synop_v = 0.5 * jo % synop_v
   jo % synop_t = 0.5 * jo % synop_t
   jo % synop_p = 0.5 * jo % synop_p
   jo % synop_q = 0.5 * jo % synop_q

end subroutine da_calculate_jo_and_grady_synop


