subroutine da_check_rh_simple (grid)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout)  :: grid

   integer                        :: i, j, k ! Loop counters.
   real                           :: q_new   ! Modified rh.
   real                           :: es      ! Dummy output.
   real                           :: t_new, p_new
   real                           :: ramax,ramin,dqmax,dqmin            

   ramax=maximum_rh/100.0
   ramin=minimum_rh/100.0

   do k = kts, kte
      do j = jts, jte
         do i = its, ite
            p_new  = grid%xb % p(i,j,k) + grid%xa %  p(i,j,k)
            t_new  = grid%xb % t(i,j,k) + grid%xa %  t(i,j,k)

            call da_tp_to_qs(t_new, p_new, es, q_new)
            dqmax=q_new*ramax - grid%xb % q(i,j,k)        
            dqmin=q_new*ramin - grid%xb % q(i,j,k)        
            grid%xa % q(i,j,k)=min(max(grid%xa % q(i,j,k),dqmin),dqmax)
         end do
      end do
   end do

end subroutine da_check_rh_simple


