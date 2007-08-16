subroutine da_get_innov_vector_polaramv( it, xb, xp, ob, iv)

   !----------------------------------------------------------------------
   ! Purpose: Calculates innovation vector, does QC for polaramv
   !----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(xb_type),    intent(in)    :: xb      ! first guess state.
   type(xpose_type), intent(in)    :: xp      ! Domain decomposition vars.
   type(y_type),     intent(in)    :: ob      ! Observation structure.
   type(ob_type),    intent(inout) :: iv      ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.

   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_u(1:max_ob_levels)  ! Model value u at ob location.
   real    :: model_v(1:max_ob_levels)  ! Model value v at ob location.

   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   integer :: itu,ituf,itvv,itvvf
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_polaramv")

   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

   do n=iv%plocal(iv%time-1,polaramv) + 1, iv%plocal(iv%time,polaramv)
      ! [1.3] Get horizontal interpolation weights:

      num_levs = iv % polaramv(n) % info % levels
      if (num_levs < 1) cycle

      model_u(:) = 0.0
      model_v(:) = 0.0

      i = iv%polaramv(n)%loc%i
      j = iv%polaramv(n)%loc%j
      dx = iv%polaramv(n)%loc%dx
      dy = iv%polaramv(n)%loc%dy
      dxm = iv%polaramv(n)%loc%dxm
      dym = iv%polaramv(n)%loc%dym

      do k=kts,kte
         v_p(k) = dym*(dxm*xb%p(i,j,k) + dx*xb%p(i+1,j,k)) + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
      end do

      do k=1, iv % polaramv(n) % info % levels
         iv%polaramv(n)%zk(k)=missing_r

         if (iv % polaramv(n) % p(k) > 1.0) then
            call da_to_zk(iv % polaramv(n) % p(k), v_p, xp, v_interp_p, iv%polaramv(n)%zk(k))
         end if

         if (iv%polaramv(n)%zk(k) < 0.0 .and.  .not.anal_type_verify) then
            iv % polaramv(n) % u(k) % qc = missing
            iv % polaramv(n) % v(k) % qc = missing
         end if
      end do

      call da_interp_lin_3d(xb%u, iv%polaramv(n)%loc, model_u, max_ob_levels, iv%polaramv(n)%zk, num_levs)
      call da_interp_lin_3d(xb%v, iv%polaramv(n)%loc, model_v, max_ob_levels, iv%polaramv(n)%zk, num_levs)

      do k = 1, iv % polaramv(n) % info % levels
         iv % polaramv(n) % u(k) % inv = 0.0
         iv % polaramv(n) % v(k) % inv = 0.0


         if (ob % polaramv(n) % u(k) > missing_r .AND. &
             iv % polaramv(n) % u(k) % qc >= obs_qc_pointer) then
            iv % polaramv(n) % u(k) % inv = ob % polaramv(n) % u(k) - model_u(k)
         end if

         if (ob % polaramv(n) % v(k) > missing_r .AND. &
             iv % polaramv(n) % v(k) % qc >= obs_qc_pointer) then
            iv % polaramv(n) % v(k) % inv = ob % polaramv(n) % v(k) - model_v(k)
         end if
      end do

      !------------------------------------------------------------------------
      ! Perform optional maximum error check:
      !------------------------------------------------------------------------

      if (check_max_iv) call da_check_max_iv_polaramv(it, iv % polaramv(n), itu,ituf,itvv,itvvf)
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Polar AMVs follows:'
      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
         'Finally Total Polar AMVs rejections ',ituf+itvvf,' on ',itu+itvv
   end if
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_polaramv")

end subroutine da_get_innov_vector_polaramv


