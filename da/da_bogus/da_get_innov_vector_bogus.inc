subroutine da_get_innov_vector_bogus( it, xb, xp, ob, iv)

   !------------------------------------------------------------------------------
   ! Purpose: calculate the innovations for the bogus data.
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: it       ! External iteration.
   type(xb_type), intent(in)     :: xb       ! first guess state.
   type(xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type(y_type),  intent(in)     :: ob       ! Observation structure.
   type(ob_type), intent(inout)  :: iv       ! O-B structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, j, k  ! Index dimension.
   integer                        :: num_levs ! Number of obs levels.

   real                           :: dx, dxm  ! Interpolation weights.
   real                           :: dy, dym  ! Interpolation weights.
   real, dimension(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.
   real, dimension(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.

   real, dimension(1:max_ob_levels) :: model_u   ! Model value u at ob location.
   real, dimension(1:max_ob_levels) :: model_v   ! Model value v at ob location.
   real, dimension(1:max_ob_levels) :: model_t   ! Model value t at ob location.
   real, dimension(1:max_ob_levels) :: model_q   ! Model value q at ob location.
   real                             :: model_slp ! Model value slp at ob location.

   integer           :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf,itslp,itslpf

   if (iv % num_bogus < 1) return

   itu   = 0; itvv    = 0; itt  = 0; itqv  = 0; itslp  = 0;
   ituf  = 0; itvvf   = 0; ittf = 0; itqvf = 0; itslpf = 0;

   do n=iv%ob_numb(iv%current_ob_time-1)%bogus+1,iv%ob_numb(iv%current_ob_time)%bogus
      num_levs = iv % bogus(n) % info % levels

      if (num_levs < 1) cycle

      model_u(:) = 0.0
      model_v(:) = 0.0
      model_t(:) = 0.0
      model_q(:) = 0.0
      model_slp  = 0.0

      i = iv%bogus(n)%loc%i
      j = iv%bogus(n)%loc%j
      dx = iv%bogus(n)%loc%dx
      dy = iv%bogus(n)%loc%dy
      dxm = iv%bogus(n)%loc%dxm
      dym = iv%bogus(n)%loc%dym

      do k=xp%kts,xp%kte
         v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
      end do

      do k=1, iv % bogus(n) % info % levels
        iv%bogus(n)%zk(k)=missing_r
        if (iv % bogus(n) % p(k) > 1.0) then
          call da_to_zk(iv % bogus(n) % p(k), v_p, xp, v_interp_p, iv%bogus(n)%zk(k))
        else if (iv % bogus(n) % h(k) > 0.0) then
          call da_to_zk(iv % bogus(n) % h(k), v_h, xp, v_interp_h, iv%bogus(n)%zk(k))
        end if

        if (iv%bogus(n)%zk(k) < 0.0 .and.  .not.anal_type_verify) then
          iv % bogus(n) % u(k) % qc = missing
          iv % bogus(n) % v(k) % qc = missing
          iv % bogus(n) % t(k) % qc = missing
          iv % bogus(n) % q(k) % qc = missing
        end if
      end do

      ! [1.4] Interpolate horizontally:
      call da_interp_lin_3d( xb % u, xp, i, j, dx, dy, dxm, dym, &
                         model_u, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( xb % v, xp, i, j, dx, dy, dxm, dym, &
                         model_v, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( xb % t, xp, i, j, dx, dy, dxm, dym, &
                         model_t, max_ob_levels, iv%bogus(n)%zk, num_levs)
      call da_interp_lin_3d( xb % q, xp, i, j, dx, dy, dxm, dym, &
                         model_q, max_ob_levels, iv%bogus(n)%zk, num_levs)

      model_slp = dym*(dxm*xb%slp(i,j)   + dx*xb%slp(i+1,j)) &
                    + dy *(dxm*xb%slp(i,j+1) + dx*xb%slp(i+1,j+1))

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      iv % bogus(n) % slp % inv = 0.0

      if (ABS(ob % bogus(n) % slp - missing_r) > 1. .AND. &
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

        if (ob % bogus(n) % u(k) > missing_r .AND. &
             iv % bogus(n) % u(k) % qc >= obs_qc_pointer) then
          iv % bogus(n) % u(k) % inv = ob % bogus(n) % u(k) - model_u(k)
        end if

        if (ob % bogus(n) % v(k) > missing_r .AND. &
             iv % bogus(n) % v(k) % qc >= obs_qc_pointer) then
          iv % bogus(n) % v(k) % inv = ob % bogus(n) % v(k) - model_v(k)
        end if

        if (ob % bogus(n) % t(k) > missing_r .AND. &
             iv % bogus(n) % t(k) % qc >= obs_qc_pointer) then

          ! only for global Bogus(YRG 07/15/2005):
          if (iv % bogus(n) % info % platform(8:12) /= 'TCBOG') then
            iv % bogus(n) % t(k) % inv = ob % bogus(n) % t(k) - model_t(k)
          else
            iv % bogus(n) % t(k) % inv = missing_r 
            iv % bogus(n) % t(k) % qc  = missing_data
          end if
        end if

        if (ob % bogus(n) % q(k) > missing_r .AND. &
             iv % bogus(n) % q(k) % qc >= obs_qc_pointer) then

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

end subroutine da_get_innov_vector_bogus


