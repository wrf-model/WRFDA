subroutine da_get_innov_vector_ssmt2( it, xb, xp, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

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
   real, dimension(1:max_ob_levels):: model_rh ! Model value rh at ob location.

   real                            :: zk       ! Interpolation vertical coordinator.

   real, dimension(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.

   integer                         :: irh, irhf
   
   if (iv % num_ssmt2 < 1) return
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_ssmt2")
   
   irh = 0 ; irhf = 0
   do n=1, iv % num_ssmt2
      num_levs = iv % ssmt2(n) % info % levels

      if (num_levs < 1) cycle

      model_rh(:) = 0.0

      ! [1.1] Get horizontal interpolation weights:

      i = iv%ssmt2(n)%loc%i
      j = iv%ssmt2(n)%loc%j
      dx = iv%ssmt2(n)%loc%dx
      dy = iv%ssmt2(n)%loc%dy
      dxm = iv%ssmt2(n)%loc%dxm
      dym = iv%ssmt2(n)%loc%dym

      do k=xp%kts,xp%kte
         v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
      end do

      num_levs=0
      do k=1, iv % ssmt2(n) % info % levels

         zk=missing_r

         if (iv % ssmt2(n) % h(k) > 0.0) then
            call da_to_zk(iv % ssmt2(n) % h(k), v_h, xp, v_interp_h, zk)
         else if (iv % ssmt2(n) % p(k) > 1.0) then
            call da_to_zk(iv % ssmt2(n) % p(k), v_p, xp, v_interp_p, zk)
         end if

         if (zk > 0.0) then
            num_levs=num_levs+1
            iv%ssmt2(n)%zk(num_levs)=zk

            ob % ssmt2(n) % rh(num_levs) = ob % ssmt2(n) % rh(k)

            iv % ssmt2(n) % rh(num_levs) % qc = iv % ssmt2(n) % rh(k) % qc

            iv % ssmt2(n) % rh(num_levs) % error = iv % ssmt2(n) % rh(k) % error
         end if
      end do

      iv % ssmt2(n) % info % levels = num_levs

      ! [1.2] Interpolate horizontally to ob:

      call da_interp_lin_3d( xb % rh, xp, i, j, dx, dy, dxm, dym, &
                          model_rh, max_ob_levels, iv%ssmt2(n)%zk, num_levs)

      !--------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !--------------------------------------------------------------------

      do k = 1, iv % ssmt2(n) % info % levels

         iv % ssmt2(n) % rh(k) % inv = 0.0

         !-----------------------------------------------------------------
         ! [3.0] Interpolation:
         !-----------------------------------------------------------------

         if (ob % ssmt2(n) % rh(k) > missing_r .AND. &
            iv % ssmt2(n) % rh(k) % qc >= obs_qc_pointer) then

            iv % ssmt2(n) % rh(k) % inv = ob % ssmt2(n) % rh(k) - &
                                         model_rh(k)
         end if

         ! write(122,'(2i4,i8,5f15.5)')n, k, iv%ssmt2(n)%height_qc(k), &
         ! iv%ssmt2(n)%info%lat, iv%ssmt2(n)%info%lon, &
         ! iv%ssmt2(n)%h(k), &
         ! ob % ssmt2(n) % rh(k), model_rh(k)

      end do

      !------------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !------------------------------------------------------------------------

      if (check_max_iv) call da_check_max_iv_ssmt2(it, iv % ssmt2(n), irh,irhf)
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')    &
         'For outer iteration ',it,' Total Rejections for SSMI(T2) follows:'
      write(unit = check_max_iv_unit, fmt = '(/,2(2(A,I6),/))') &
         'Number of failed RH(T2) observations:    ',irhf, ' on ',irh
   end if
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_ssmt2")

end subroutine da_get_innov_vector_ssmt2


