subroutine da_get_innov_vector_qscat ( it, xb, xp, ob, iv )

   IMPLICIT NONE

   INTEGER, INTENT(IN)           :: it      ! External iteration.
   TYPE (xb_type), INTENT(IN)    :: xb      ! first guess state.
   TYPE (xpose_type), INTENT(IN) :: xp      ! Domain decomposition vars.
   TYPE (y_type),  INTENT(INOUT)    :: ob      ! Observation structure.
   TYPE (ob_type), INTENT(INOUT) :: iv      ! O-B structure.

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j, k  ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: model_u  ! Model value u at oblocation.
   REAL                         :: model_v  ! Model value v at oblocation.

   REAL                         :: zk       ! Interpolation vertical coordinator.

   REAL, DIMENSION(xp%kms:xp%kme) :: v_h      ! Model value h at ob hor. location.

   INTEGER                      :: itu, itvv, ituf, itvvf
   IF ( iv % num_qscat > 0 ) THEN

      DO n=iv%ob_numb(iv%current_ob_time-1)%qscat + 1, iv%ob_numb(iv%current_ob_time)%qscat

        itu = 0 ; itvv = 0 ;  ituf = 0 ; itvvf = 0

       if(iv%qscat(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%qscat(n)%loc%i
         j = iv%qscat(n)%loc%j
         dx = iv%qscat(n)%loc%dx
         dy = iv%qscat(n)%loc%dy
         dxm = iv%qscat(n)%loc%dxm
         dym = iv%qscat(n)%loc%dym

         do k=xp%kts,xp%kte
            v_h(k) = dym*(dxm*xb%h(i,j  ,k) + dx*xb%h(i+1,j  ,k)) &
                   + dy *(dxm*xb%h(i,j+1,k) + dx*xb%h(i+1,j+1,k))
         enddo

         zk=missing_r

         IF ( iv % qscat(n) % h > missing_r) THEN
            call to_zk(iv % qscat(n) % h, v_h, xp, v_interp_h, zk)
            if(zk < 1.0) then
               zk = 1.0
            endif
         ENDIF

         iv%qscat(n)%zk=zk

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

         iv % qscat(n) % u % inv = 0.0
         iv % qscat(n) % v % inv = 0.0

         if(zk < 0.0) then
            iv % qscat(n) % u % qc = missing
            iv % qscat(n) % v % qc = missing
         else

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                 model_u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                 model_v, iv%qscat(n)%zk)
#else
         call Interp_Obs_lin_2D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                        model_u, iv%qscat(n)%zk)
         call Interp_Obs_lin_2D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                        model_v, iv%qscat(n)%zk)
#endif

!------------------------------------------------------------------------
!        [2.0] Initialise components of innovation vector:
!------------------------------------------------------------------------

!------------------------------------------------------------------------
!           [3.0] Fast interpolation:
!------------------------------------------------------------------------

            IF ( ob % qscat(n) % u > missing_r .AND. &
                 iv % qscat(n) % u % qc >= obs_qc_pointer ) THEN

                 iv % qscat(n) % u % inv = ob % qscat(n) % u - model_u
            ENDIF

            IF ( ob % qscat(n) % v > missing_r .AND. &
                 iv % qscat(n) % v % qc >= obs_qc_pointer ) THEN

                 iv % qscat(n) % v % inv = ob % qscat(n) % v - model_v
            ENDIF

!------------------------------------------------------------------------
!        [5.0] Perform optional maximum error check:
!------------------------------------------------------------------------

       IF( check_max_iv )& 
       CALL da_check_max_iv_qscat(it, iv % qscat(n), itu, itvv, ituf,itvvf)

       endif
       endif
            
      END DO

     write(unit = check_max_iv_unit, FMT ='(A,i5,A)')'For outer iteration ',it,&
                   ', Total Rejections for qscat follows:'
       WRITE (UNIT = check_max_iv_unit, FMT = '(/,4(2(A,I6),/))') &
      'Number of failed u-wind observations:     ',ituf, ' on ',itu,   &
      'Number of failed v-wind observations:     ',itvvf,' on ',itvv

   ENDIF

END subroutine da_get_innov_vector_qscat

