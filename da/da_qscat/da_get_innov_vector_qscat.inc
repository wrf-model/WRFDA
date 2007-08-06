subroutine da_get_innov_vector_qscat (it, xb, xp, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(xb_type),    intent(in)    :: xb      ! first guess state.
   type(xpose_type), intent(in)    :: xp      ! Domain decomposition vars.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(ob_type),    intent(inout) :: iv      ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_u  ! Model value u at oblocation.
   real    :: model_v  ! Model value v at oblocation.

   real    :: zk       ! Interpolation vertical coordinator.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   integer  :: itu, itvv, ituf, itvvf

   if (iv % num_qscat < 1) return 
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_qscat")

   do n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat

      itu = 0 ; itvv = 0 ;  ituf = 0 ; itvvf = 0

      ! [1.1] Get horizontal interpolation weights:

      i = iv%qscat(n)%loc%i
      j = iv%qscat(n)%loc%j
      dx = iv%qscat(n)%loc%dx
      dy = iv%qscat(n)%loc%dy
      dxm = iv%qscat(n)%loc%dxm
      dym = iv%qscat(n)%loc%dym

      do k=kts,kte
         v_h(k) = dym*(dxm*xb%h(i,j,k)+dx*xb%h(i+1,j,k)) + dy*(dxm*xb%h(i,j+1,k)+dx*xb%h(i+1,j+1,k))
      end do

      zk=missing_r

      if (iv % qscat(n) % h > missing_r) then
         call da_to_zk(iv % qscat(n) % h, v_h, xp, v_interp_h, zk)
         if (zk < 1.0) then
            zk = 1.0
         end if
      end if

      iv%qscat(n)%zk=zk

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      iv % qscat(n) % u % inv = 0.0
      iv % qscat(n) % v % inv = 0.0

      if (zk < 0.0 .and.  .not.anal_type_verify) then
         iv % qscat(n) % u % qc = missing
         iv % qscat(n) % v % qc = missing
      else

         ! [1.2] Interpolate horizontally:
         call da_interp_obs_lin_2d( xb % u, i, j, dx, dy, dxm, dym, &
            model_u, iv%qscat(n)%zk)
         call da_interp_obs_lin_2d( xb % v, i, j, dx, dy, dxm, dym, &
            model_v, iv%qscat(n)%zk)

         !------------------------------------------------------------------------
         ! [2.0] Initialise components of innovation vector:
         !------------------------------------------------------------------------

         !------------------------------------------------------------------------
         ! [3.0] Fast interpolation:
         !------------------------------------------------------------------------

         if (ob % qscat(n) % u > missing_r .AND. &
             iv % qscat(n) % u % qc >= obs_qc_pointer) then
            iv % qscat(n) % u % inv = ob % qscat(n) % u - model_u
         end if

         if (ob % qscat(n) % v > missing_r .AND. &
             iv % qscat(n) % v % qc >= obs_qc_pointer) then
            iv % qscat(n) % v % inv = ob % qscat(n) % v - model_v
         end if

         !------------------------------------------------------------------------
         ! [5.0] Perform optional maximum error check:
         !------------------------------------------------------------------------

         if (check_max_iv) & 
            call da_check_max_iv_qscat(it, iv % qscat(n), itu, itvv, ituf,itvvf)
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it,&
         ', Total Rejections for qscat follows:'
      write(unit = check_max_iv_unit, fmt = '(/,4(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv
   end if 
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_qscat")

end subroutine da_get_innov_vector_qscat


