subroutine da_get_innov_vector_airep( it, xb, xp, ob, iv)

   implicit none

   integer, intent(in)            :: it       ! External iteration.
   type(xb_type), intent(in)     :: xb       ! first guess state.
   type(xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type(y_type),  intent(inout)  :: ob       ! Observation structure.
   type(ob_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs ! Number of obs levels.
   real                           :: dx, dxm  ! Interpolation weights.
   real                           :: dy, dym  ! Interpolation weights.
   real, dimension(1:max_ob_levels) :: model_u  ! Model value u at ob location.
   real, dimension(1:max_ob_levels) :: model_v  ! Model value v at ob location.
   real, dimension(1:max_ob_levels) :: model_t  ! Model value t at ob location.


   real, dimension(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.

   integer           :: itu,ituf,itvv,itvvf,itt,ittf

   if (iv % num_airep > 0) then

      itu   = 0; itvv    = 0; itt  = 0;
      ituf  = 0; itvvf   = 0; ittf = 0;

      do n=iv%ob_numb(iv%current_ob_time-1)%airep + 1, iv%ob_numb(iv%current_ob_time)%airep
         num_levs = iv % airep(n) % info % levels
         if (num_levs < 1) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0
         model_t(:) = 0.0

         ! [1.1] Get horizontal interpolation weights:

         i = iv%airep(n)%loc%i
         j = iv%airep(n)%loc%j
         dx = iv%airep(n)%loc%dx
         dy = iv%airep(n)%loc%dy
         dxm = iv%airep(n)%loc%dxm
         dym = iv%airep(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                    + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                    + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         end do

         num_levs=0
         do k=1, iv % airep(n) % info % levels
            iv%airep(n)%zk(k)=missing_r
            if (iv % airep(n) % p(k) > 1.0) then
               call da_to_zk(iv % airep(n) % p(k), v_p, xp, v_interp_p, &
                  iv%airep(n)%zk(k))
            else if (iv % airep(n) % h(k) > 0.0) then
               call da_to_zk(iv % airep(n) % h(k), v_h, xp, v_interp_h, &
                  iv%airep(n)%zk(k))
            end if

            if (iv%airep(n)%zk(k) < 0.0) then
               iv % airep(n) % u(k) % qc = missing
               iv % airep(n) % v(k) % qc = missing
               iv % airep(n) % t(k) % qc = missing
            end if
         end do

         num_levs = iv % airep(n) % info % levels

         ! [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d( xb % u, xp, i, j, dx, dy, dxm, dym, &
            model_u, max_ob_levels, iv%airep(n)%zk, num_levs)

         call da_interp_lin_3d( xb % v, xp, i, j, dx, dy, dxm, dym, &
            model_v, max_ob_levels, iv%airep(n)%zk, num_levs)

         call da_interp_lin_3d( xb % t, xp, i, j, dx, dy, dxm, dym, &
             model_t, max_ob_levels, iv%airep(n)%zk, num_levs)
#else
         call da_interp_lin_3d( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, &
            dx, dy, dxm, dym, &
            model_u(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)

         call da_interp_lin_3d( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, &
            dx, dy, dxm, dym, &
            model_v(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)

         call da_interp_lin_3d( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, &
            dx, dy, dxm, dym, &
            model_t(1), max_ob_levels, iv%airep(n)%zk(1), num_levs)
#endif

         !-------------------------------------------------------------------
         ! [2.0] Initialise components of innovation vector:
         !-------------------------------------------------------------------

         do k = 1, iv % airep(n) % info % levels
            iv % airep(n) % u(k) % inv = 0.0
            iv % airep(n) % v(k) % inv = 0.0
            iv % airep(n) % t(k) % inv = 0.0

            !----------------------------------------------------------------
            ! [3.0] Fast interpolation:
            !----------------------------------------------------------------

            if (ob % airep(n) % u(k) > missing_r .AND. &
                 iv % airep(n) % u(k) % qc >= obs_qc_pointer) then
               iv % airep(n) % u(k) % inv = ob % airep(n) % u(k) - model_u(k)
            end if

            if (ob % airep(n) % v(k) > missing_r .AND. &
                 iv % airep(n) % v(k) % qc >= obs_qc_pointer) then
               iv % airep(n) % v(k) % inv = ob % airep(n) % v(k) - model_v(k)
            end if

            if (ob % airep(n) % t(k) > missing_r .AND. &
                 iv % airep(n) % t(k) % qc >= obs_qc_pointer) then
               iv % airep(n) % t(k) % inv = ob % airep(n) % t(k) - model_t(k)
            end if
         end do

         !-------------------------------------------------------------------
         ! [5.0] Perform optional maximum error check:
         !-------------------------------------------------------------------

         if (check_max_iv)then
            call da_check_max_iv_airep(it, iv % airep(n),itu,ituf,itvv, &
               itvvf,itt,ittf)
         end if
      end do

      if (check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)') &
            'For outer iteration ',it, &
            ', Total Rejections for Airep follows:'
         write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
            'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
            'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
            'Number of failed temperature observations:',ittf, ' on ',itt,   &
            'Finally Total Airep rejections ',ituf+itvvf+ittf,' on ', &
            itu +itvv +itt
      end if
   end if

end subroutine da_get_innov_vector_airep


