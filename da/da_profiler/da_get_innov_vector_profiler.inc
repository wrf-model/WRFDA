subroutine da_get_innov_vector_profiler( it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.

   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real    :: model_u(1:max_ob_levels)  ! Model value u at ob location.
   real    :: model_v(1:max_ob_levels)  ! Model value v at ob location.

   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.
   integer :: itu,ituf,itvv,itvvf
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_profiler")
   
   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

   do n=iv%plocal(iv%time-1,profiler) + 1, iv%plocal(iv%time,profiler)
      num_levs = iv % profiler(n) % info % levels

      if (num_levs < 1) cycle

      model_u(:) = 0.0
      model_v(:) = 0.0

      ! [1.3] Get horizontal interpolation weights:

      i = iv%profiler(n)%loc%i
      j = iv%profiler(n)%loc%j
      dx = iv%profiler(n)%loc%dx
      dy = iv%profiler(n)%loc%dy
      dxm = iv%profiler(n)%loc%dxm
      dym = iv%profiler(n)%loc%dym

      do k=kts,kte
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, iv % profiler(n) % info % levels
         iv%profiler(n)%zk(k)=missing_r

         if (iv % profiler(n) % p(k) > 1.0) then
            call da_to_zk(iv % profiler(n) % p(k), v_p, v_interp_p, iv%profiler(n)%zk(k))
         end if

         if (iv%profiler(n)%zk(k) < 0.0 .and.  .not.anal_type_verify) then
            iv % profiler(n) % u(k) % qc = missing
            iv % profiler(n) % v(k) % qc = missing
         end if
      end do

      ! [1.4] Interpolate horizontally:
      call da_interp_lin_3d( grid%xb%u, iv%profiler(n)%loc, model_u, max_ob_levels, iv%profiler(n)%zk, num_levs)
      call da_interp_lin_3d( grid%xb%v, iv%profiler(n)%loc, model_v, max_ob_levels, iv%profiler(n)%zk, num_levs)

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      do k = 1, iv % profiler(n) % info % levels
         iv % profiler(n) % u(k) % inv = 0.0
         iv % profiler(n) % v(k) % inv = 0.0

         !----------------------------------------------------------------
         ! [4.0] Fast interpolation:
         !----------------------------------------------------------------

         if (ob % profiler(n) % u(k) > missing_r .AND. &
             iv % profiler(n) % u(k) % qc >= obs_qc_pointer) then
            iv % profiler(n) % u(k) % inv = ob % profiler(n) % u(k) - model_u(k)
         end if

         if (ob % profiler(n) % v(k) > missing_r .AND. &
             iv % profiler(n) % v(k) % qc >= obs_qc_pointer) then
            iv % profiler(n) % v(k) % inv = ob % profiler(n) % v(k) - model_v(k)
         end if
      end do

      !------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !------------------------------------------------------------------

      if (check_max_iv) then  
         call da_check_max_iv_profiler(it, iv % profiler(n), itu,ituf,itvv,itvvf)
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')&
         'For outer iteration ',it, ', Total Rejections for Profiler follows:'
      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv,  &
         'Finally Total Profiler rejections ',ituf+itvvf,' on ',itu +itvv
   end if
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_profiler")

end subroutine da_get_innov_vector_profiler


