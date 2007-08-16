subroutine da_transform_xtoy_bogus (grid, iv, y)

   !---------------------------------------------------------------------
   ! Purpose: the linearized bogus observation operators.
   !---------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (ob_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm 
   real    :: dy, dym
   real    :: model_t(kts:kte)
   real    :: model_t9(kts:kte)
   real    :: model_q(kts:kte)
   real    :: model_q9(kts:kte)
   real    :: model_p_c(kts:kte)
   real    :: model_p_c9(kts:kte)
   real    :: psfc_loc,terr_loc,psfc_loc9

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_bogus")      

   do n = iv%plocal(iv%time-1,bogus)+1, iv%plocal(iv%time,bogus)
      num_levs = iv % bogus(n) % info % levels

      if (num_levs < 1) cycle

      ! [1.1] Get cross pt. horizontal interpolation weights:

      i   = iv%bogus(n)%loc%i
      dy  = iv%bogus(n)%loc%dy
      dym = iv%bogus(n)%loc%dym
      j   = iv%bogus(n)%loc%j
      dx  = iv%bogus(n)%loc%dx
      dxm = iv%bogus(n)%loc%dxm

      ! [1.2] Interpolate horizontally from cross points:

      do k = kts, kte
         model_t9(k) = dym*(dxm*grid%xa%t(i,j,k) + dx*grid%xa%t(i+1,j,k)) &
            + dy *(dxm*grid%xa%t(i,j+1,k) + dx*grid%xa%t(i+1,j+1,k))
         model_t(k) = dym*(dxm*grid%xb%t(i,j,k) + dx*grid%xb%t(i+1,j,k)) &
            + dy *(dxm*grid%xb%t(i,j+1,k) + dx*grid%xb%t(i+1,j+1,k))
         model_q9(k) = dym*(dxm*grid%xa%q(i,j,k) + dx*grid%xa%q(i+1,j,k)) &
            + dy *(dxm*grid%xa%q(i,j+1,k) + dx*grid%xa%q(i+1,j+1,k))
         model_q(k) = dym*(dxm*grid%xb%q(i,j,k) + dx*grid%xb%q(i+1,j,k)) &
            + dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
         model_p_c9(k) = dym*(dxm*grid%xa%p(i,j,k) + dx*grid%xa%p(i+1,j,k)) &
            + dy *(dxm*grid%xa%p(i,j+1,k) + dx*grid%xa%p(i+1,j+1,k))
         model_p_c(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) &
            + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      terr_loc = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) &
         + dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
      psfc_loc = dym*(dxm*grid%xb%psfc(i,j)   + dx*grid%xb%psfc(i+1,j)) &
         + dy *(dxm*grid%xb%psfc(i,j+1) + dx*grid%xb%psfc(i+1,j+1))
      psfc_loc9 = dym*(dxm*grid%xa%psfc(i,j)   + dx*grid%xa%psfc(i+1,j)) &
         + dy *(dxm*grid%xa%psfc(i,j+1) + dx*grid%xa%psfc(i+1,j+1))

      call da_tpq_to_slp_lin (model_t, model_q, model_p_c, terr_loc, psfc_loc,   &
         model_t9, model_q9, model_p_c9, psfc_loc9, y%bogus(n)%slp) 

      ! [1.4] Interpolate horizontally from dot points:
      call da_interp_lin_3d(grid%xa % u, iv%bogus(n)%loc, &
         y%bogus(n)%u, num_levs, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa % v, iv%bogus(n)%loc, &
         y%bogus(n)%v, num_levs, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa % t, iv%bogus(n)%loc, &
         y%bogus(n)%t, num_levs, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d(grid%xa % q, iv%bogus(n)%loc, &
         y%bogus(n)%q, num_levs, iv%bogus(n)%zk, num_levs) 
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_bogus") 

end subroutine da_transform_xtoy_bogus


