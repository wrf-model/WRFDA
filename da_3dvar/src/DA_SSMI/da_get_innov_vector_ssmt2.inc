SUBROUTINE da_get_innov_vector_ssmt2( it, xb, xp, ob, iv )

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
   REAL, DIMENSION(1:max_ob_levels):: model_rh ! Model value rh at ob location.

   REAL                            :: zk       ! Interpolation vertical coordinator.

   REAL, DIMENSION(xp%kms:xp%kme)  :: v_h      ! Model value h at ob hor. location.
   REAL, DIMENSION(xp%kms:xp%kme)  :: v_p      ! Model value p at ob hor. location.
!
   INTEGER                         :: irh, irhf
!
   IF ( iv % num_ssmt2 > 0 ) THEN
       irh = 0 ; irhf = 0
      DO n=1, iv % num_ssmt2

       if(iv%ssmt2(n)%loc%proc_domain_with_halo) then

         num_levs = iv % ssmt2(n) % info % levels

         if ( num_levs < 1 ) cycle

         model_rh(:) = 0.0

!        [1.1] Get horizontal interpolation weights:

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
         enddo

         num_levs=0
         do k=1, iv % ssmt2(n) % info % levels

            zk=missing_r

            IF ( iv % ssmt2(n) % h(k) > 0.0 ) THEN
               call to_zk(iv % ssmt2(n) % h(k), v_h, xp, v_interp_h, zk)
            ELSE IF ( iv % ssmt2(n) % p(k) > 1.0 ) THEN
               call to_zk(iv % ssmt2(n) % p(k), v_p, xp, v_interp_p, zk)
            ENDIF

            if(zk > 0.0) then
               num_levs=num_levs+1
               iv%ssmt2(n)%zk(num_levs)=zk

               ob % ssmt2(n) % rh(num_levs) = ob % ssmt2(n) % rh(k)

               iv % ssmt2(n) % rh(num_levs) % qc = iv % ssmt2(n) % rh(k) % qc

               iv % ssmt2(n) % rh(num_levs) % error = iv % ssmt2(n) % rh(k) % error
            endif
         enddo

         iv % ssmt2(n) % info % levels = num_levs

!        [1.2] Interpolate horizontally to ob:

#ifndef DEREF_KLUDGE
         call Interp_lin_3D( xb % rh, xp, i, j, dx, dy, dxm, dym, &
                             model_rh, max_ob_levels, iv%ssmt2(n)%zk, num_levs )
#else
         call Interp_lin_3D( xb % rh(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                             model_rh, max_ob_levels, iv%ssmt2(n)%zk, num_levs )
#endif

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         do k = 1, iv % ssmt2(n) % info % levels

            iv % ssmt2(n) % rh(k) % inv = 0.0

!------------------------------------------------------------------------
!           [3.0] Interpolation:
!------------------------------------------------------------------------

            IF ( ob % ssmt2(n) % rh(k) > missing_r .AND. &
               iv % ssmt2(n) % rh(k) % qc >= obs_qc_pointer ) THEN

               iv % ssmt2(n) % rh(k) % inv = ob % ssmt2(n) % rh(k) - &
                                            model_rh(k)
            ENDIF
            
!           write(122,'(2i4,i8,5f15.5)')n, k, iv%ssmt2(n)%height_qc(k), &
!           iv%ssmt2(n)%info%lat, iv%ssmt2(n)%info%lon, &
!           iv%ssmt2(n)%h(k), &
!           ob % ssmt2(n) % rh(k), model_rh(k)

         END DO

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

   IF ( check_max_iv ) CALL da_check_max_iv_ssmt2(it, iv % ssmt2(n), irh,irhf )
            
       endif

      END DO

       write(unit = check_max_iv_unit, FMT ='(A,i5,A)')    &
      'For outer iteration ',it,' Total Rejections for SSMI (T2) follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,2(2(A,I6),/))') &
      'Number of failed RH (T2) observations:    ',irhf, ' on ',irh

   ENDIF

END SUBROUTINE da_get_innov_vector_ssmt2

