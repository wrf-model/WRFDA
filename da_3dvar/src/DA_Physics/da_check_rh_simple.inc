subroutine da_check_rh_simple( xb, xa, xp )

   implicit none

   TYPE (x_type),  INTENT(INOUT)  :: xa      ! Analysis increment
   TYPE (xb_type), INTENT(INOUT)  :: xb      ! First guess state
   type (xpose_type), intent(in)  :: xp      ! Domain decomposition vars.

   integer                        :: i, j, k ! Loop counters.
   real                           :: rh_new  ! Modified rh.
   real                           :: q_new   ! Modified rh.
   real                           :: es      ! Dummy output.
   real                           :: t_new, p_new
   real                           :: ramax,ramin,dqmax,dqmin            

    ramax=maximum_rh/100.
    ramin=minimum_rh/100.
   do k = xp%kts, xp%kte
   do j = xp%jts, xp%jte
   do i = xp%its, xp%ite
      p_new  = xb % p(i,j,k) + xa %  p(i,j,k)
      t_new  = xb % t(i,j,k) + xa %  t(i,j,k)

      call da_tp_to_qs( t_new, p_new, es, q_new )
      dqmax=q_new*ramax - xb % q(i,j,k)        
      dqmin=q_new*ramin - xb % q(i,j,k)        
      xa % q(i,j,k)=min(max(xa % q(i,j,k),dqmin),dqmax)
   end do
   end do
   end do

end subroutine da_check_rh_simple

