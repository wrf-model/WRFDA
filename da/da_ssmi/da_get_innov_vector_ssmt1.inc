SUBROUTINE da_get_innov_vector_ssmt1( it, xb, xp, ob, iv )

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
   REAL, DIMENSION(1:max_ob_levels) :: model_t  ! Model value t at ob location.

   REAL                            :: zk       ! Interpolation vertical coordinator.

   REAL, DIMENSION(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.

   integer                         :: itt, ittf
   IF ( iv % num_ssmt1 > 0 ) THEN
      itt = 0 ; ittf = 0
      DO n=1, iv % num_ssmt1
      
       if(iv%ssmt1(n)%loc%proc_domain_with_halo) then

         num_levs = iv % ssmt1(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_t(:) = 0.0

!        [1.1] Get horizontal interpolation weights:

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
         enddo

         num_levs=0
         do k=1, iv % ssmt1(n) % info % levels

            zk=missing_r

            IF ( iv % ssmt1(n) % h(k) > 0.0 ) THEN
               call to_zk(iv % ssmt1(n) % h(k), v_h, xp, v_interp_h, zk)
            ELSE IF ( iv % ssmt1(n) % p(k) > 1.0 ) THEN
               call to_zk(iv % ssmt1(n) % p(k), v_p, xp, v_interp_p, zk)
            ENDIF

            if(zk > 0.0) then
               num_levs=num_levs+1
               iv%ssmt1(n)%zk(num_levs)=zk

               ob % ssmt1(n) % t(num_levs) = ob % ssmt1(n) % t(k)

               iv % ssmt1(n) % t(num_levs) % qc = iv % ssmt1(n) % t(k) % qc

               iv % ssmt1(n) % t(num_levs) % error = iv % ssmt1(n) % t(k) % error
            endif
         enddo

         iv % ssmt1(n) % info % levels = num_levs

!        [1.2] Interpolate horizontally to ob:

#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb % t, xp, i, j, dx, dy, dxm, dym, &
                             model_t, max_ob_levels, iv%ssmt1(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_t, max_ob_levels, iv%ssmt1(n)%zk, num_levs )
#endif

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv % ssmt1(n) % info % levels

            iv % ssmt1(n) % t(k) % inv = 0.0

!------------------------------------------------------------------------
!           [3.0] Interpolation:
!------------------------------------------------------------------------

            IF ( ob % ssmt1(n) % t(k) > missing_r .AND. &
               iv % ssmt1(n) % t(k) % qc >= obs_qc_pointer ) THEN

               iv % ssmt1(n) % t(k) % inv = ob % ssmt1(n) % t(k) - &
                                            model_t(k)
            ENDIF
         END DO

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

      IF(check_max_iv) CALL da_check_max_iv_ssmt1(it,iv%ssmt1(n),itt,ittf)
            
       endif

      END DO
      write(unit = check_max_iv_unit, FMT ='(A,i5,A)')    &
      'For outer iteration ',it,' Total Rejections for SSMI (T1) follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,2(2(A,I6),/))') &
      'Number of failed RH (T1) observations:    ',ittf, ' on ',itt

   ENDIF

END SUBROUTINE da_get_innov_vector_ssmt1

