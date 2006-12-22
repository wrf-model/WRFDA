subroutine da_transform_xtoy_sonde_sfc_adj( xb, iv, xp, jo_grad_y, jo_grad_x )

   implicit none

   type (xb_type), intent(in)    :: xb          ! first guess state.
   type (ob_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n        ! Loop counter.
   integer                       :: i, j     ! Index dimension.
   real                          :: dx, dxm  ! 
   real                          :: dy, dym  !

   if ( iv%num_sound > 0 ) then
      if (sfc_assi_options == 1) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
            iv%ob_numb(iv%current_ob_time)%sound
            ! [1.1] Get horizontal interpolation weights:

            i = iv%sonde_sfc(n)%loc%i
            j = iv%sonde_sfc(n)%loc%j
            dx = iv%sonde_sfc(n)%loc%dx
            dy = iv%sonde_sfc(n)%loc%dy
            dxm = iv%sonde_sfc(n)%loc%dxm
            dym = iv%sonde_sfc(n)%loc%dym

            ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
            call da_interp_obs_lin_2d_adj(jo_grad_x % u, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % v, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % t, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj(jo_grad_x % q, xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc, xp%ims, xp%ime, &
               xp%jms, xp%jme, i, j, dx, dy, dxm, dym, &
               jo_grad_y%sonde_sfc(n)%p)
#else
            call da_interp_obs_lin_2d_adj( &
               jo_grad_x % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%u, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj( &
               jo_grad_x % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%v, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj( &
               jo_grad_x % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%t, iv%sonde_sfc(n)%zk)
            call da_interp_obs_lin_2d_adj( &
               jo_grad_x % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, &
               dxm, dym, jo_grad_y%sonde_sfc(n)%q, iv%sonde_sfc(n)%zk)

            call da_interp_lin_2d_adj(jo_grad_x % psfc(xp%ims,xp%jms), &
               xp%ims, xp%ime, xp%jms, xp%jme, &
               i, j, dx, dy, dxm, dym, jo_grad_y%sonde_sfc(n)%p)
#endif
         end do
      else if (sfc_assi_options == 2) then
         do n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
            iv%ob_numb(iv%current_ob_time)%sound
            call da_transform_xtopsfc_adj(xb,xp,iv%sonde_sfc(n), &
                                          jo_grad_y%sonde_sfc(n),jo_grad_x)
         end do
      end if
   end if

end subroutine da_transform_xtoy_sonde_sfc_adj


