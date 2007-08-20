subroutine da_get_innov_vector_sound (it, xb, xp, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD  
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(xb_type),    intent(in)    :: xb       ! first guess state.
   type(xpose_type), intent(in)    :: xp       ! Domain decomposition vars.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real    :: model_u(1:max_ob_levels)  ! Model value u at ob location.
   real    :: model_v(1:max_ob_levels)  ! Model value v at ob location.
   real    :: model_t(1:max_ob_levels)  ! Model value t at ob location.
   real    :: model_q(1:max_ob_levels)  ! Model value q at ob location.
   ! real    :: model_h(1:max_ob_levels)  ! Model value h at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   integer :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_sound")

   itu   = 0; itvv    = 0;  itt  = 0; itqv  = 0;
   ituf  = 0; itvvf   = 0;  ittf = 0; itqvf = 0;

   do n=iv%plocal(iv%time-1,sound) + 1, iv%plocal(iv%time,sound)
      num_levs = iv%sound(n)%info%levels

      if (num_levs < 1) cycle

      model_u(:) = 0.0
      model_v(:) = 0.0
      model_t(:) = 0.0
      model_q(:) = 0.0
      ! model_h(:) = 0.0

      ! [1.1] Get horizontal interpolation weights:

      i   = iv%sound(n)%loc%i
      j   = iv%sound(n)%loc%j
      dx  = iv%sound(n)%loc%dx
      dy  = iv%sound(n)%loc%dy
      dxm = iv%sound(n)%loc%dxm
      dym = iv%sound(n)%loc%dym

      do k=kts,kte
         v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
      end do

      do k=1, num_levs
         iv%sound(n)%zk(k)=missing_r

         if (iv%sound(n)%p(k) > 1.0) then
            call da_to_zk(iv%sound(n)%p(k), v_p, xp, v_interp_p, iv%sound(n)%zk(k))
         else if (iv%sound(n)%h(k) > 0.0) then
            call da_to_zk(iv%sound(n)%h(k), v_h, xp, v_interp_h, iv%sound(n)%zk(k))
         end if

         if (iv%sound(n)%zk(k) < 0.0 .and. .not. anal_type_verify) then
            iv%sound(n)%u(k)%qc = missing
            iv%sound(n)%v(k)%qc = missing
            iv%sound(n)%t(k)%qc = missing
            iv%sound(n)%q(k)%qc = missing
         end if
      end do

      ! [1.2] Interpolate horizontally to ob:

      call da_interp_lin_3d( xb%u, iv%sound(n)%loc, model_u, max_ob_levels, iv%sound(n)%zk, num_levs)
      call da_interp_lin_3d( xb%v, iv%sound(n)%loc, model_v, max_ob_levels, iv%sound(n)%zk, num_levs)
      call da_interp_lin_3d( xb%t, iv%sound(n)%loc, model_t, max_ob_levels, iv%sound(n)%zk, num_levs)
      call da_interp_lin_3d( xb%q, iv%sound(n)%loc, model_q, max_ob_levels, iv%sound(n)%zk, num_levs)

      ! call da_interp_lin_3d( xb%h, i, j, dx, dy, dxm, dym, &
      !    model_h, max_ob_levels, iv%sound(n)%zk, num_levs)

      !----------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !----------------------------------------------------------------------

      do k = 1, iv%sound(n)%info%levels
         iv%sound(n)%u(k)%inv = 0.0
         iv%sound(n)%v(k)%inv = 0.0
         iv%sound(n)%t(k)%inv = 0.0
         iv%sound(n)%q(k)%inv = 0.0

         !-------------------------------------------------------------------
         ! [3.0] Interpolation:
         !-------------------------------------------------------------------

         if (ob%sound(n)%u(k) > missing_r .AND. iv%sound(n)%u(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%u(k)%inv = ob%sound(n)%u(k) - model_u(k)
         end if

         if (ob%sound(n)%v(k) > missing_r .AND. iv%sound(n)%v(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%v(k)%inv = ob%sound(n)%v(k) - model_v(k)
         end if


         if (ob%sound(n)%t(k) > missing_r .AND. iv%sound(n)%t(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%t(k)%inv = ob%sound(n)%t(k) - model_t(k)
         end if

         if (ob%sound(n)%q(k) > missing_r .AND. iv%sound(n)%q(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%q(k)%inv = ob%sound(n)%q(k) - model_q(k)
         end if
      end do

      !----------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !----------------------------------------------------------------------

      if (check_max_iv) then
         call da_check_max_iv_sound(it, iv % sound(n), &
           itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf)
      end if
   end do

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ',it, &
         ', Total Rejections for Sound follows:'

      write(unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') &
         'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
         'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
         'Number of failed temperature observations:',ittf, ' on ',itt,   &
         'Number of failed mixing ratio observations:',itqvf,' on ',itqv, &
         'Finally Total Sound rejections: ',ituf+itvvf+ittf+itqvf,' on ',&
                                            itu +itvv+itt +itqv
   end if
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_sound")

end subroutine da_get_innov_vector_sound


