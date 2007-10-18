subroutine da_set_boundary_2d(var)

   !------------------------------------------------------------------------
   ! Purpose: Merge East-West boundary values for input 3d-array (var)   
   !------------------------------------------------------------------------

   implicit none

   real :: var(ims:ime, jms:jme)

   integer :: n, j

   if ((its /= ids) .or. (ite /= ide)) return

   if (trace_use) call da_trace_entry("da_set_boundary_2d")

   do j=jts, jte
      do n=1,bdyzone
         var(ids-n,j) = var(ide+1-n,j)
         var(ide+n,j) = var(ids-1+n,j)
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_boundary_2d")

end subroutine da_set_boundary_2d


djtest_lhs = adjtest_lhs + (y%pseudo(n)%u/typical_u_rms)**2 + &
            (y%pseudo(n)%v/typical_v_rms)**2 + &
            (y%pseudo(n)%v/typical_t_rms)**2 + &
            (y%pseudo(n)%v/typical_p_rms)**2 + &
            (y%pseudo(n)%v/typical_q_rms)**2
      end if

      pertile_lhs = pertile_lhs + (y%pseudo(n)%u/typical_u_rms)**2 + &
         (y%pseudo(n)%v/typical_v_rms)**2 + &
         (y%pseudo(n)%v/typical_t_rms)**2 + &
         (y%pseudo(n)%v/typical_p_rms)**2 + &
         (y%pseudo(n)%v/typical_q_rms)**2

      y%pseudo(n)%u = y%pseudo(n)%u/typical_u_rms ** 2
      y%pseudo(n)%v = y%pseudo(n)%v/typical_v_rms ** 2
      y%pseudo(n)%t = y%pseudo(n)%p/typical_t_rms ** 2
      y%pseudo(n)%p = y%pseudo(n)%v/typical_p_rms ** 2
      y%pseudo(n)%q = y%pseudo(n)%p/typical_q_rms ** 2
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_pseudo")

end subroutine da_check_xtoy_adjoint_pseudo


