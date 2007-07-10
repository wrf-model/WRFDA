subroutine da_transform_xtoy_bogus_adj(grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------
   ! Purpose: the adjoint of bogus observation operators.
   !---------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)     :: grid       ! first guess state.
   type (ob_type),    intent(in)     :: iv          ! obs. inc vector (o-b).
   type (y_type) ,    intent(inout)  :: jo_grad_y   ! grad_y(jo)
   type (x_type) ,    intent(inout)  :: jo_grad_x   ! grad_x(jo)

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs
   real                           :: dx, dxm  ! 
   real                           :: dy, dym  !

   real, dimension(kms:kme) :: model_t,tt
   real, dimension(kms:kme) :: model_q,qq
   real, dimension(kms:kme) :: model_p_c,pp
   real                           :: psfc_loc,terr_loc,ppsfc                     

   if (iv%num_bogus > 0) then
      do n= iv%ob_numb(iv%current_ob_time-1)%bogus+1, iv%ob_numb(iv%current_ob_time)%bogus

         num_levs = iv % bogus(n) % info % levels

         if (num_levs < 1) cycle

         ! [1.1] Get cross pt. horizontal interpolation weights:

         i = iv%bogus(n)%loc%i
         dy = iv%bogus(n)%loc%dy
         dym = iv%bogus(n)%loc%dym
         j = iv%bogus(n)%loc%j
         dx = iv%bogus(n)%loc%dx
         dxm = iv%bogus(n)%loc%dxm

         ! [1.1] Adjoint feedback from Y to X for u and v:

         call da_interp_lin_3d_adj(jo_grad_x % u, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%bogus(n)%u, num_levs,            &
                               iv%bogus(n)%zk, num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % v, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%bogus(n)%v, num_levs,            &
                               iv%bogus(n)%zk, num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % t, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%bogus(n)%t, num_levs,            &
                               iv%bogus(n)%zk, num_levs)
         call da_interp_lin_3d_adj(jo_grad_x % q, i, j, dx, dy, dxm, dym, &
                               jo_grad_y%bogus(n)%q, num_levs,            &
                               iv%bogus(n)%zk, num_levs)

         ! [1.2] Compute the feedback from SLP to t and q:

         ! 1.2.1 Background at the observation location:

         do k = kts, kte
            model_t(k) = dym*(dxm*grid%xb%t(i,j,k) + dx*grid%xb%t(i+1,j,k)) &
                         + dy *(dxm*grid%xb%t(i,j+1,k) + dx*grid%xb%t(i+1,j+1,k))
            model_q(k) = dym*(dxm*grid%xb%q(i,j,k) + dx*grid%xb%q(i+1,j,k)) &
                         + dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
            model_p_c(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) &
                         + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
         end do

         terr_loc = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) &
                   + dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
         psfc_loc = dym*(dxm*grid%xb%psfc(i,j)   + dx*grid%xb%psfc(i+1,j)) &
                   + dy *(dxm*grid%xb%psfc(i,j+1) + dx*grid%xb%psfc(i+1,j+1))

         ! 1.2.2 Compute the feedback from SLP to p, t, q, and psfc 
         !       at the observed location:

         call da_tpq_to_slp_adj(model_t, model_q ,model_p_c,    &
                               terr_loc,  psfc_loc,   &
                               tt, qq, pp, ppsfc,     &
                               jo_grad_y%bogus(n)%slp, grid%xp)  

         ! 1.2.3 feedback from the observed location to grid space:

         ! 1.2.3.1 for Psfc

         jo_grad_x % psfc(i,j) = jo_grad_x % psfc(i,j) + dym*dxm*ppsfc
         jo_grad_x % psfc(i+1,j) = jo_grad_x % psfc(i+1,j)+ dym*dx*ppsfc
         jo_grad_x % psfc(i,j+1) = jo_grad_x % psfc(i,j+1)+ dy*dxm*ppsfc
         jo_grad_x % psfc(i+1,j+1)=jo_grad_x % psfc(i+1,j+1)+dy*dx*ppsfc

         ! 1.2.3.2 for t, q, p

         do k = kts, kte
            jo_grad_x % t(i,j,k) = jo_grad_x % t(i,j,k)+dym*dxm*tt(k)
            jo_grad_x % t(i+1,j,k) = jo_grad_x % t(i+1,j,k)+ dym*dx*tt(k)
            jo_grad_x % t(i,j+1,k) = jo_grad_x % t(i,j+1,k)+ dy*dxm*tt(k)
            jo_grad_x % t(i+1,j+1,k)=jo_grad_x % t(i+1,j+1,k)+dy*dx*tt(k)
            jo_grad_x % q(i,j,k) = jo_grad_x % q(i,j,k)+dym*dxm*qq(k)
            jo_grad_x % q(i+1,j,k) = jo_grad_x % q(i+1,j,k)+ dym*dx*qq(k)
            jo_grad_x % q(i,j+1,k) = jo_grad_x % q(i,j+1,k)+ dy*dxm*qq(k)
            jo_grad_x % q(i+1,j+1,k)=jo_grad_x % q(i+1,j+1,k)+dy*dx*qq(k)
            jo_grad_x % p(i,j,k) = jo_grad_x % p(i,j,k)+dym*dxm*pp(k)
            jo_grad_x % p(i+1,j,k) = jo_grad_x % p(i+1,j,k)+ dym*dx*pp(k)
            jo_grad_x % p(i,j+1,k) = jo_grad_x % p(i,j+1,k)+ dy*dxm*pp(k)
            jo_grad_x % p(i+1,j+1,k)=jo_grad_x % p(i+1,j+1,k)+dy*dx*pp(k)
         end do
      end do
   end if

end subroutine da_transform_xtoy_bogus_adj


