subroutine da_get_innov_vector_pilot ( it, xb, xp, ob, iv )

   implicit none

   integer, intent(in)            :: it       ! External iteration.
   type (xb_type), intent(in)     :: xb       ! first guess state.
   type (xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type (y_type),  intent(inout)  :: ob       ! Observation structure.
   type (ob_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs ! Number of obs levels.

   real                           :: dx, dxm  ! Interpolation weights.
   real                           :: dy, dym  ! Interpolation weights.

   real, dimension(1:max_ob_levels) :: model_u  ! Model value u at ob location.
   real, dimension(1:max_ob_levels) :: model_v  ! Model value v at ob location.

   real, dimension(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.

   integer           :: itu,ituf,itvv,itvvf

   if ( iv % num_pilot > 0 ) then

      itu   = 0
      itvv  = 0
      ituf  = 0
      itvvf = 0

      do n=iv%ob_numb(iv%current_ob_time-1)%pilot + 1, iv%ob_numb(iv%current_ob_time)%pilot

         num_levs = iv % pilot(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0

         ! [1.3] Get horizontal interpolation weights:

         i = iv%pilot(n)%loc%i
         j = iv%pilot(n)%loc%j
         dx = iv%pilot(n)%loc%dx
         dy = iv%pilot(n)%loc%dy
         dxm = iv%pilot(n)%loc%dxm
         dym = iv%pilot(n)%loc%dym

         do k=xp%kts,xp%kte
           v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                  + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         end do

         do k=1, iv % pilot(n) % info % levels
           iv%pilot(n)%zk(k)=missing_r

           if ( iv % pilot(n) % p(k) > 1.0 ) then
             call da_to_zk(iv % pilot(n) % p(k), v_p, xp, v_interp_p, iv%pilot(n)%zk(k))
           end if

           if (iv%pilot(n)%zk(k) < 0.0) then
             iv % pilot(n) % u(k) % qc = missing
             iv % pilot(n) % v(k) % qc = missing
           end if
         end do

         ! [1.4] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d( xb % u, xp, i, j, dx, dy, dxm, dym, &
                            model_u, max_ob_levels, iv%pilot(n)%zk, num_levs )
         call da_interp_lin_3d( xb % v, xp, i, j, dx, dy, dxm, dym, &
                            model_v, max_ob_levels, iv%pilot(n)%zk, num_levs )
#else
         call da_interp_lin_3d( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            model_u(1), max_ob_levels, iv%pilot(n)%zk(1), num_levs )
         call da_interp_lin_3d( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            model_v(1), max_ob_levels, iv%pilot(n)%zk(1), num_levs )
#endif
         !------------------------------------------------------------------------
         ! [2.0] Initialise components of innovation vector:
         !------------------------------------------------------------------------

         do k = 1, iv % pilot(n) % info % levels
           iv % pilot(n) % u(k) % inv = 0.0
           iv % pilot(n) % v(k) % inv = 0.0

           !------------------------------------------------------------------------
           ! [4.0] Fast interpolation:
           !------------------------------------------------------------------------

           if ( ob % pilot(n) % u(k) > missing_r .AND. &
                iv % pilot(n) % u(k) % qc >= obs_qc_pointer ) then
             iv % pilot(n) % u(k) % inv = ob % pilot(n) % u(k) - model_u(k)
           end if

           if ( ob % pilot(n) % v(k) > missing_r .AND. &
                iv % pilot(n) % v(k) % qc >= obs_qc_pointer ) then
             iv % pilot(n) % v(k) % inv = ob % pilot(n) % v(k) - model_v(k)
           end if
         end do

         !------------------------------------------------------------------------
         ! [5.0] Perform optional maximum error check:
         !------------------------------------------------------------------------

         if ( check_max_iv ) then
           call da_check_max_iv_pilot(it, iv % pilot(n), itu,ituf,itvv,itvvf)
         end if
      end do

      if (check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
            ', Total Rejections for Pilot follows:'
         write (unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
            'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
            'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
            'Finally Total Pilot rejections ',ituf+itvvf,' on ',itu+itvv
      end if
   end if

end subroutine da_get_innov_vector_pilot

