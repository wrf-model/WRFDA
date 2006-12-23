subroutine da_get_innov_vector_ssmt1( it, xb, xp, ob, iv)

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
   real, dimension(1:max_ob_levels) :: model_t  ! Model value t at ob location.

   real                            :: zk       ! Interpolation vertical coordinator.

   real, dimension(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.

   integer                         :: itt, ittf
   if (iv % num_ssmt1 > 0) then
      itt = 0 ; ittf = 0
      do n=1, iv % num_ssmt1
      
         num_levs = iv % ssmt1(n) % info % levels

         if (num_levs < 1) cycle

         model_t(:) = 0.0

         ! [1.1] Get horizontal interpolation weights:

         i = iv%ssmt1(n)%loc%i
         j = iv%ssmt1(n)%loc%j
         dx = iv%ssmt1(n)%loc%dx
         dy = iv%ssmt1(n)%loc%dy
         dxm = iv%ssmt1(n)%loc%dxm
         dym = iv%ssmt1(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         end do

         num_levs=0
         do k=1, iv % ssmt1(n) % info % levels

            zk=missing_r

            if (iv % ssmt1(n) % h(k) > 0.0) then
               call da_to_zk(iv % ssmt1(n) % h(k), v_h, xp, v_interp_h, zk)
            else if (iv % ssmt1(n) % p(k) > 1.0) then
               call da_to_zk(iv % ssmt1(n) % p(k), v_p, xp, v_interp_p, zk)
            end if

            if (zk > 0.0) then
               num_levs=num_levs+1
               iv%ssmt1(n)%zk(num_levs)=zk

               ob % ssmt1(n) % t(num_levs) = ob % ssmt1(n) % t(k)

               iv % ssmt1(n) % t(num_levs) % qc = iv % ssmt1(n) % t(k) % qc

               iv % ssmt1(n) % t(num_levs) % error = iv % ssmt1(n) % t(k) % error
            end if
         end do

         iv % ssmt1(n) % info % levels = num_levs

!        [1.2] Interpolate horizontally to ob:

#ifndef DEREF_KLUDGE
         call da_interp_lin_3d( xb % t, xp, i, j, dx, dy, dxm, dym, &
                             model_t, max_ob_levels, iv%ssmt1(n)%zk, num_levs)
#else
         call da_interp_lin_3d( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_t, max_ob_levels, iv%ssmt1(n)%zk, num_levs)
#endif

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv % ssmt1(n) % info % levels

            iv % ssmt1(n) % t(k) % inv = 0.0

!------------------------------------------------------------------------
!           [3.0] Interpolation:
!------------------------------------------------------------------------

            if (ob % ssmt1(n) % t(k) > missing_r .AND. &
               iv % ssmt1(n) % t(k) % qc >= obs_qc_pointer) then

               iv % ssmt1(n) % t(k) % inv = ob % ssmt1(n) % t(k) - &
                                            model_t(k)
            end if
         end do

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

         if (check_max_iv) call da_check_max_iv_ssmt1(it,iv%ssmt1(n),itt,ittf)
      end do

      if (check_max_iv_print) then
         write(unit = check_max_iv_unit, fmt ='(A,i5,A)')    &
            'For outer iteration ',it,' Total Rejections for SSMI(T1) follows:'
         write(unit = check_max_iv_unit, fmt = '(/,2(2(A,I6),/))') &
            'Number of failed RH(T1) observations:    ',ittf, ' on ',itt
      end if
   end if

end subroutine da_get_innov_vector_ssmt1


