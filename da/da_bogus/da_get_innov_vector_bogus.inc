subroutine da_get_innov_vector_bogus( it, grid, ob, iv)

   !------------------------------------------------------------------------------
   ! Purpose: calculate the innovations for the bogus data.
   !------------------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(in)    :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.

   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   real    :: model_u(1:max_ob_levels)   ! Model value u at ob location.
   real    :: model_v(1:max_ob_levels)   ! Model value v at ob location.
   real    :: model_t(1:max_ob_levels)   ! Model value t at ob location.
   real    :: model_q(1:max_ob_levels)   ! Model value q at ob location.
   real    :: model_slp                  ! Model value slp at ob location.

   integer :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf,itslp,itslpf
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_bogus")

   itu   = 0; itvv    = 0; itt  = 0; itqv  = 0; itslp  = 0;
   ituf  = 0; itvvf   = 0; ittf = 0; itqvf = 0; itslpf = 0;

   do n=iv%plocal(iv%time-1,bogus)+1,iv%plocal(iv%time,bogus)
      num_levs = iv % bogus(n) % info % levels

      if (num_levs < 1) cycle

      model_u(:) = 0.0
      model_v(:) = 0.0
      model_t(:) = 0.0
      model_q(:) = 0.0
      model_slp  = 0.0

      i   = iv%bogus(n)%loc%i
      j   = iv%bogus(n)%loc%j
      dx  = iv%bogus(n)%loc%dx
      dy  = iv%bogus(n)%loc%dy
      dxm = iv%bogus(n)%loc%dxm
      dym = iv%bogus(n)%loc%dym

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k) + dx*grid%xb%h(i+1,j,k)) + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, iv % bogus(n) % info % levels
         iv%bogus(n)%zk(k)=missing_r
         if (iv % bogus(n) % p(k) > 1.0) then
            call da_to_zk(iv % bogus(n) % p(k), v_p, v_interp_p, iv%bogus(n)%zk(k))
         else if (iv % bogus(n) % h(k) > 0.0) then
            call da_to_zk(iv % bogus(n) % h(k), v_h, v_interp_h, iv%bogus(n)%zk(k))
         end if

         if (iv%bogus(n)%zk(k) < 0.0 .and.  .not.anal_type_verify) then
            iv % bogus(n) % u(k) % qc = missing
            iv % bogus(n) % v(k) % qc = missing
            iv % bogus(n) % t(k) % qc = missing
            iv % bogus(n) % q(k) % qc = missing
         end if
      end do

      ! [1.4] Interpolate horizontally:
      call da_interp_lin_3d( grid%xb % u, iv%bogus(n)%loc, model_u, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( grid%xb % v, iv%bogus(n)%loc, model_v, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( grid%xb % t, iv%bogus(n)%loc, model_t, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( grid%xb % q, iv%bogus(n)%loc, model_q, max_ob_levels, iv%bogus(n)%zk, num_levs)

      model_slp = dym*(dxm*grid%xb%slp(i,j)   + dx*grid%xb%slp(i+1,j)) &
         + dy *(dxm*grid%xb%slp(i,j+1) + dx*grid%xb%slp(i+1,j+1))

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      iv % bogus(n) % slp % inv = 0.0

      if (ABS(ob % bogus(n) % slp - missing_r) > 1.0 .AND. &
           iv % bogus(n) % slp % qc >= obs_qc_pointer) then
        iv % bogus(n) % slp % inv = ob % bogus(n) % slp - model_slp
      end if

      do k = 1, iv % bogus(n) % info % levels
         iv % bogus(n) % u(k) % inv = 0.0
         iv % bogus(n) % v(k) % inv = 0.0
         iv % bogus(n) % t(k) % inv = 0.0
         iv % bogus(n) % q(k) % inv = 0.0

         !------------------------------------------------------------------------
         ! [4.0] Fast interpolation:
         !------------------------------------------------------------------------

         if (ob % bogus(n) % u(k) > missing_r .AND. iv % bogus(n) % u(k) % qc >= obs_qc_pointer) then
           iv % bogus(n) % u(k) % inv = ob % bogus(n) % u(k) - model_u(k)
         end if

         if (ob % bogus(n) % v(k) > missing_r .AND. iv % bogus(n) % v(k) % qc >= obs_qc_pointer) then
           iv % bogus(n) % v(k) % inv = ob % bogus(n) % v(k) - model_v(k)
         end if

         if (ob % bogus(n) % t(k) > missing_r .AND. iv % bogus(n) % t(k) % qc >= obs_qc_pointer) then
            ! only for global Bogus(YRG 07/15/2005):
            if (iv % bogus(n) % info % platform(8:12) /= 'TCBOG') then
               iv % bogus(n) % t(k) % inv = ob % bogus(n) % t(k) - model_t(k)
            else
               iv % bogus(n) % t(k) % inv = missing_r 
               iv % bogus(n) % t(k) % qc  = missing_data
            end if
         end if

         if (ob % bogus(n) % q(k) > missing_r .AND. iv % bogus(n) % q(k) % qc >= obs_qc_pointer) then
            ! only for global Bogus(YRG 07/15/2005):
            if (iv % bogus(n) % info % platform(8:12) /= 'TCBOG') then
               iv % bogus(n) % q(k) % inv = ob % bogus(n) % q(k) - model_q(k)
            else
              iv % bogus(n) % q(k) % inv = missing_r 
              iv % bogus(n) % q(k) % qc  = missing_data
            end if
         end if
      end do

      !------------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !------------------------------------------------------------------------

      if (check_max_iv) then
         call da_check_max_iv_bogus(it, iv % bogus(n), &
            itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf,itslp,itslpf)
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Bogus follows:'

      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
        'Number of failed u-wind       observations:     ',ituf,  ' on ',itu,   &
        'Number of failed v-wind       observations:     ',itvvf, ' on ',itvv,   &
        'Number of failed temperature  observations:     ',ittf,  ' on ',itt,   &
        'Number of failed mixing ratio observations:     ',itqvf, ' on ',itqv,   &
        'Number of failed slp          observations:     ',itslpf,' on ',itslp,   &
        'Finally Total Bogus rejections ',ituf+itvvf+ittf+itqvf+itslpf,' on ', &
                                         itu +itvv +itt +itqv +itslp
   end if
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_bogus")

end subroutine da_get_innov_vector_bogus


