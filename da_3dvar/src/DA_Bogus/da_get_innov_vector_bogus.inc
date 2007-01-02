SUBROUTINE da_get_innov_vector_bogus ( it, xb, xp, ob, iv )

!------------------------------------------------------------------------------
!  This subroutine is to calculate the innovations for the bogus data.
!  History: Initial setup for MM5 3DVAR, on September 2003, by Ying Zhang.
!  History: Port to WRF frame,           on November  2004, by Qingnong Xiao
!------------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(IN)     :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.

   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.
   REAL, DIMENSION(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme) :: v_p      ! Model value p at ob hor. location.

   REAL, DIMENSION(1:max_ob_levels) :: model_u   ! Model value u at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_v   ! Model value v at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_t   ! Model value t at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_q   ! Model value q at ob location.
   REAL                             :: model_slp ! Model value slp at ob location.

   INTEGER           :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf,itslp,itslpf

   IF ( iv % num_bogus < 1 ) return

   itu   = 0; itvv    = 0; itt  = 0; itqv  = 0; itslp  = 0;
   ituf  = 0; itvvf   = 0; ittf = 0; itqvf = 0; itslpf = 0;

   DO n=iv%ob_numb(iv%current_ob_time-1)%bogus+1,iv%ob_numb(iv%current_ob_time)%bogus

     if (iv%bogus(n)%loc%proc_domain_with_halo) then

      num_levs = iv % bogus(n) % info % levels

      if ( num_levs < 1 ) cycle

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
      enddo

      do k=1, iv % bogus(n) % info % levels

         iv%bogus(n)%zk(k)=missing_r

         IF ( iv % bogus(n) % p(k) > 1.0 ) THEN
            call to_zk(iv % bogus(n) % p(k), v_p, xp, v_interp_p, iv%bogus(n)%zk(k))
         ELSE IF ( iv % bogus(n) % h(k) > 0.0 ) THEN
            call to_zk(iv % bogus(n) % h(k), v_h, xp, v_interp_h, iv%bogus(n)%zk(k))
         ENDIF

         if(iv%bogus(n)%zk(k) < 0.0) then
            iv % bogus(n) % u(k) % qc = missing
            iv % bogus(n) % v(k) % qc = missing
            iv % bogus(n) % t(k) % qc = missing
            iv % bogus(n) % q(k) % qc = missing
         endif
      enddo

!     [1.4] Interpolate horizontally:
#ifndef DEREF_KLUDGE
      call Interp_lin_3D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                          model_u, max_ob_levels, iv%bogus(n)%zk, num_levs )
      call Interp_lin_3D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                          model_v, max_ob_levels, iv%bogus(n)%zk, num_levs )
      call Interp_lin_3D( xb % t, xp, i, j, dx, dy, dxm, dym, &
                          model_t, max_ob_levels, iv%bogus(n)%zk, num_levs )
      call Interp_lin_3D( xb % q, xp, i, j, dx, dy, dxm, dym, &
                          model_q, max_ob_levels, iv%bogus(n)%zk, num_levs )
#else
      call Interp_lin_3D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                          model_u(1), max_ob_levels, iv%bogus(n)%zk(1), num_levs )
      call Interp_lin_3D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                          model_v(1), max_ob_levels, iv%bogus(n)%zk(1), num_levs )
      call Interp_lin_3D( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                          model_t(1), max_ob_levels, iv%bogus(n)%zk(1), num_levs )
      call Interp_lin_3D( xb % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                          model_q(1), max_ob_levels, iv%bogus(n)%zk(1), num_levs )
#endif

      model_slp = dym*(dxm*xb%slp(i,j)   + dx*xb%slp(i+1,j)) &
                     + dy *(dxm*xb%slp(i,j+1) + dx*xb%slp(i+1,j+1))

!------------------------------------------------------------------------
!     [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

      iv % bogus(n) % slp % inv = 0.0

      IF ( ABS(ob % bogus(n) % slp - missing_r) > 1. .AND. &
           iv % bogus(n) % slp % qc >= obs_qc_pointer ) THEN
         iv % bogus(n) % slp % inv = ob % bogus(n) % slp - &
                                                 model_slp
      ENDIF

      do k = 1, iv % bogus(n) % info % levels

         iv % bogus(n) % u(k) % inv = 0.0
         iv % bogus(n) % v(k) % inv = 0.0
         iv % bogus(n) % t(k) % inv = 0.0
         iv % bogus(n) % q(k) % inv = 0.0

!------------------------------------------------------------------------
!     [4.0] Fast interpolation:
!------------------------------------------------------------------------

         IF ( ob % bogus(n) % u(k) > missing_r .AND. &
              iv % bogus(n) % u(k) % qc >= obs_qc_pointer ) THEN

              iv % bogus(n) % u(k) % inv = ob % bogus(n) % u(k) - &
                                           model_u(k)
         ENDIF

         IF ( ob % bogus(n) % v(k) > missing_r .AND. &
              iv % bogus(n) % v(k) % qc >= obs_qc_pointer ) THEN

              iv % bogus(n) % v(k) % inv = ob % bogus(n) % v(k) - &
                                           model_v(k)
         ENDIF

         IF ( ob % bogus(n) % t(k) > missing_r .AND. &
              iv % bogus(n) % t(k) % qc >= obs_qc_pointer ) THEN

! Only for global Bogus (YRG 07/15/2005):
              if ( iv % bogus(n) % info % platform(8:12) /= 'TCBOG') then
                iv % bogus(n) % t(k) % inv = ob % bogus(n) % t(k) - &
                                           model_t(k)
              else
                iv % bogus(n) % t(k) % inv = missing_r 
                iv % bogus(n) % t(k) % qc  = missing_data
              endif
         ENDIF

         IF ( ob % bogus(n) % q(k) > missing_r .AND. &
              iv % bogus(n) % q(k) % qc >= obs_qc_pointer ) THEN

! Only for global Bogus (YRG 07/15/2005):
              if ( iv % bogus(n) % info % platform(8:12) /= 'TCBOG') then
                iv % bogus(n) % q(k) % inv = ob % bogus(n) % q(k) - &
                                           model_q(k)
              else
                iv % bogus(n) % q(k) % inv = missing_r 
                iv % bogus(n) % q(k) % qc  = missing_data
              endif
         ENDIF

      enddo

!------------------------------------------------------------------------
!     [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

      IF ( check_max_iv ) CALL da_check_max_iv_bogus(it, iv % bogus(n), &
                   itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf,itslp,itslpf)

     endif

   END DO

   write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Bogus follows:'

   WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
    'Number of failed u-wind       observations:     ',ituf,  ' on ',itu,   &
    'Number of failed v-wind       observations:     ',itvvf, ' on ',itvv,   &
    'Number of failed temperature  observations:     ',ittf,  ' on ',itt,   &
    'Number of failed mixing ratio observations:     ',itqvf, ' on ',itqv,   &
    'Number of failed slp          observations:     ',itslpf,' on ',itslp,   &
    'Finally Total Bogus rejections ',ituf+itvvf+ittf+itqvf+itslpf,' on ', &
                                      itu +itvv +itt +itqv +itslp

END SUBROUTINE da_get_innov_vector_bogus

