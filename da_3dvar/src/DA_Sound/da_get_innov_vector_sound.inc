SUBROUTINE da_get_innov_vector_sound ( it, xb, xp, ob, iv )
!-------------------------------------------------------------------------
!
!  History:
!
!
!     Additions:        03/19/2003                    S. R. H. Rizvi
!
!               1. DA_Check_Max_IV is called for each outer iteration
!
!               2. Extra diagnostics added for generating detailed information
!                  about rejected obs in various outer iterations
!
!     Modification:     02/18/2004              Wei Huang
!               Make obs. operator independent of cv-options.
!
!-------------------------------------------------------------------------
!   USE DA_Grid_Definitions

   IMPLICIT NONE

   INTEGER, INTENT(IN)            :: it       ! External iteration.
   TYPE (xb_type), INTENT(IN)     :: xb       ! first guess state.
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)  :: ob       ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)  :: iv       ! O-B structure.

   INTEGER                        :: n        ! Loop counter.
   INTEGER                        :: i, j, k  ! Index dimension.
   INTEGER                        :: num_levs ! Number of obs levels.
   REAL                           :: dx, dxm  ! Interpolation weights.
   REAL                           :: dy, dym  ! Interpolation weights.

   REAL, DIMENSION(1:max_ob_levels) :: model_u  ! Model value u at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_v  ! Model value v at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.
   REAL, DIMENSION(1:max_ob_levels) :: model_q  ! Model value q at ob location.
!  REAL, DIMENSION(1:max_ob_levels) :: model_h  ! Model value h at ob location.

   REAL, DIMENSION(xp%kms:xp%kme)   :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme)   :: v_p      ! Model value p at ob hor. location.

   REAL                         :: model_p  ! Model value p at oblocation.
   REAL                         :: model_h  ! Model value h at oblocation.
!
   INTEGER           :: itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf
!


   IF ( iv%num_sound < 1 ) return

   itu   = 0; itvv    = 0;  itt  = 0; itqv  = 0;
   ituf  = 0; itvvf   = 0;  ittf = 0; itqvf = 0;

   DO n=iv%ob_numb(iv%current_ob_time-1)%sound + 1, iv%ob_numb(iv%current_ob_time)%sound

      if(iv%sound(n)%loc%proc_domain_with_halo) then

         num_levs = iv%sound(n)%info%levels

         if ( num_levs < 1 ) cycle

         model_u(:) = 0.0
         model_v(:) = 0.0
         model_t(:) = 0.0
         model_q(:) = 0.0
!        model_h(:) = 0.0

!        [1.1] Get horizontal interpolation weights:

         i = iv%sound(n)%loc%i
         j = iv%sound(n)%loc%j
         dx = iv%sound(n)%loc%dx
         dy = iv%sound(n)%loc%dy
         dxm = iv%sound(n)%loc%dxm
         dym = iv%sound(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
            v_p(k) = dym*(dxm*xb%p(i,j  ,k) + dx*xb%p(i+1,j  ,k)) &
                   + dy *(dxm*xb%p(i,j+1,k) + dx*xb%p(i+1,j+1,k))
         enddo

         do k=1, num_levs
            iv%sound(n)%zk(k)=missing_r

            IF ( iv%sound(n)%p(k) > 1.0 ) THEN
               call to_zk(iv%sound(n)%p(k), v_p, xp, v_interp_p, iv%sound(n)%zk(k))
            ELSE IF ( iv%sound(n)%h(k) > 0.0 ) THEN
               call to_zk(iv%sound(n)%h(k), v_h, xp, v_interp_h, iv%sound(n)%zk(k))
            ENDIF

            if(iv%sound(n)%zk(k) < 0.0 .and. &
               (ANALYSIS_TYPE /= 'VERIFY' .and. ANALYSIS_TYPE /= 'verify') ) then
               iv%sound(n)%u(k)%qc = missing
               iv%sound(n)%v(k)%qc = missing
               iv%sound(n)%t(k)%qc = missing
               iv%sound(n)%q(k)%qc = missing
            endif
         enddo

!        [1.2] Interpolate horizontally to ob:

#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb%u, xp, i, j, dx, dy, dxm, dym, &
                              model_u, max_ob_levels, iv%sound(n)%zk, num_levs )
         call Interp_lin_3D( xb%v, xp, i, j, dx, dy, dxm, dym, &
                              model_v, max_ob_levels, iv%sound(n)%zk, num_levs )
         call Interp_lin_3D( xb%t, xp, i, j, dx, dy, dxm, dym, &
                              model_t, max_ob_levels, iv%sound(n)%zk, num_levs )
         call Interp_lin_3D( xb%q, xp, i, j, dx, dy, dxm, dym, &
                              model_q, max_ob_levels, iv%sound(n)%zk, num_levs )

!        call Interp_lin_3D( xb%h, xp, i, j, dx, dy, dxm, dym, &
!                             model_h, max_ob_levels, iv%sound(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb%u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                              model_u(1), max_ob_levels, iv%sound(n)%zk(1), num_levs )
         call Interp_lin_3D( xb%v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                              model_v(1), max_ob_levels, iv%sound(n)%zk(1), num_levs )
         call Interp_lin_3D( xb%t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                              model_t(1), max_ob_levels, iv%sound(n)%zk(1), num_levs )
         call Interp_lin_3D( xb%q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                              model_q(1), max_ob_levels, iv%sound(n)%zk(1), num_levs )

!        call Interp_lin_3D( xb%h(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
!                             model_h(1), max_ob_levels, iv%sound(n)%zk(1), num_levs )
#endif
!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv%sound(n)%info%levels

            iv%sound(n)%u(k)%inv = 0.0
            iv%sound(n)%v(k)%inv = 0.0
            iv%sound(n)%t(k)%inv = 0.0
            iv%sound(n)%q(k)%inv = 0.0

!------------------------------------------------------------------------
!           [3.0] Interpolation:
!------------------------------------------------------------------------

            IF ( ob%sound(n)%u(k) > missing_r .AND. &
                 iv%sound(n)%u(k)%qc >= obs_qc_pointer ) THEN
                 iv%sound(n)%u(k)%inv = ob%sound(n)%u(k) - model_u(k)
            ENDIF

            IF ( ob%sound(n)%v(k) > missing_r .AND. &
                 iv%sound(n)%v(k)%qc >= obs_qc_pointer ) THEN
                 iv%sound(n)%v(k)%inv = ob%sound(n)%v(k) - model_v(k)
            ENDIF


            IF ( ob%sound(n)%t(k) > missing_r .AND. &
                 iv%sound(n)%t(k)%qc >= obs_qc_pointer ) THEN
                 iv%sound(n)%t(k)%inv = ob%sound(n)%t(k) - model_t(k)
            ENDIF

            IF ( ob%sound(n)%q(k) > missing_r .AND. &
                 iv%sound(n)%q(k)%qc >= obs_qc_pointer ) THEN
                 iv%sound(n)%q(k)%inv = ob%sound(n)%q(k) - model_q(k)
            ENDIF

         END DO
               
!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------
         IF ( check_max_iv ) CALL da_check_max_iv_sound(it, iv % sound(n), &
                        itu,ituf,itvv,itvvf,itt,ittf,itqv,itqvf)

       endif

   END DO

   write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it, &
                   ', Total Rejections for Sound follows:'

   WRITE (UNIT = check_max_iv_unit, FMT = '(/,10(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv,   &
      'Number of failed temperature observations:',ittf, ' on ',itt,   &
      'Number of failed mixing ratio observations:',itqvf,' on ',itqv, &
      'Finally Total Sound rejections: ',ituf+itvvf+ittf+itqvf,' on ',&
                                         itu +itvv+itt +itqv

END SUBROUTINE da_get_innov_vector_sound

