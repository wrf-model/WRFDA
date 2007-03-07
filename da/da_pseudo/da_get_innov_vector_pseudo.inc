subroutine da_get_innov_vector_pseudo( xb, xp, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   type(xb_type), intent(in)   :: xb          ! Background structure 
   type(xpose_type), intent(in):: xp          ! Domain decomposition vars.
   type( y_type), intent(inout):: ob          ! Observation structure.
   type(ob_type), intent(inout):: iv          ! O-B structure.

   integer                      :: n        ! Loop counter.
   integer                      :: i, j     ! Index dimension.
   real                         :: dx, dxm  ! Interpolation weights.
   real                         :: dy, dym  ! Interpolation weights.
   real                         :: model_u  ! Model value u at oblocation.
   real                         :: model_v  ! Model value v at oblocation.
   real                         :: model_t  ! Model value t at oblocation.
   real                         :: model_p  ! Model value p at oblocation.
   real                         :: model_q  ! Model value q at oblocation.

   if (iv % num_pseudo < 1) return
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_pseudo")

   do n=1, iv % num_pseudo
      ! [1.1] Get horizontal interpolation weights:

      i = iv%pseudo(n)%loc%i
      dy = iv%pseudo(n)%loc%dy
      dym = iv%pseudo(n)%loc%dym
      j = iv%pseudo(n)%loc%j
      dx = iv%pseudo(n)%loc%dx
      dxm = iv%pseudo(n)%loc%dxm

      write(unit=stdout, fmt='(/a,2i5/a,4f15.5/)') &
           'pseudo: i,j=', i,j, &
           'pseudo: dy, dym, dx, dxm=', dy, dym, dx, dxm

      ! [1.2] Interpolate horizontally:
      call da_interp_obs_lin_2d( xb % u, xp, i, j, dx, dy, dxm, dym, &
                              model_u, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d( xb % v, xp, i, j, dx, dy, dxm, dym, &
                              model_v, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d( xb % t, xp, i, j, dx, dy, dxm, dym, &
                              model_t, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d( xb % p, xp, i, j, dx, dy, dxm, dym, &
                              model_p, iv%pseudo(n)%zk)
      call da_interp_obs_lin_2d( xb % q, xp, i, j, dx, dy, dxm, dym, &
                              model_q, iv%pseudo(n)%zk)

      !---------------------------------------------------------------
      ! [3.0] Calculate observation O = B +(O-B):
      !---------------------------------------------------------------

      if (pseudo_var(1:1) == 'u' .or.  pseudo_var(1:1) == 'U') then
         ob % pseudo(n) % u = model_u + iv % pseudo(n) % u % inv
      else if (pseudo_var(1:1) == 'v' .or.  pseudo_var(1:1) == 'V') then
         ob % pseudo(n) % v = model_v + iv % pseudo(n) % v % inv
      else if (pseudo_var(1:1) == 't' .or.  pseudo_var(1:1) == 'T') then
         ob % pseudo(n) % t = model_t + iv % pseudo(n) % t % inv
      else if (pseudo_var(1:1) == 'p' .or.  pseudo_var(1:1) == 'P') then
         ob % pseudo(n) % p = model_p + iv % pseudo(n) % p % inv
      else if (pseudo_var(1:1) == 'q' .or.  pseudo_var(1:1) == 'Q') then
         ob % pseudo(n) % q = model_q + iv % pseudo(n) % q % inv
      end if 
   end do
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_pseudo")

end subroutine da_get_innov_vector_pseudo


