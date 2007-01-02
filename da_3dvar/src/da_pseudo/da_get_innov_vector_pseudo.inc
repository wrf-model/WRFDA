SUBROUTINE da_get_innov_vector_pseudo( xb, xp, ob, iv )

   IMPLICIT NONE
   
   TYPE (xb_type), INTENT(IN)   :: xb          ! Background structure 
   TYPE (xpose_type), INTENT(IN):: xp          ! Domain decomposition vars.
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(INOUT):: iv          ! O-B structure.

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! Interpolation weights.
   REAL                         :: dy, dym  ! Interpolation weights.
   REAL                         :: model_u  ! Model value u at oblocation.
   REAL                         :: model_v  ! Model value v at oblocation.
   REAL                         :: model_t  ! Model value t at oblocation.
   REAL                         :: model_p  ! Model value p at oblocation.
   REAL                         :: model_q  ! Model value q at oblocation.

   IF ( iv % num_pseudo > 0 ) THEN

      DO n=1, iv % num_pseudo

       if(iv%pseudo(n)%loc%proc_domain_with_halo) then

!        [1.1] Get horizontal interpolation weights:

         i = iv%pseudo(n)%loc%i
         dy = iv%pseudo(n)%loc%dy
         dym = iv%pseudo(n)%loc%dym
         j = iv%pseudo(n)%loc%j
         dx = iv%pseudo(n)%loc%dx
         dxm = iv%pseudo(n)%loc%dxm

         write(unit=*, fmt='(/a,2i5/a,4f15.5/)') &
              'pseudo: i,j=', i,j, &
              'pseudo: dy, dym, dx, dxm=', dy, dym, dx, dxm

!        [1.2] Interpolate horizontally:
#ifndef DEREF_KLUDGE
         call Interp_Obs_lin_2D( xb % u, xp, i, j, dx, dy, dxm, dym, &
                                 model_u, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % v, xp, i, j, dx, dy, dxm, dym, &
                                 model_v, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % t, xp, i, j, dx, dy, dxm, dym, &
                                 model_t, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % p, xp, i, j, dx, dy, dxm, dym, &
                                 model_p, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % q, xp, i, j, dx, dy, dxm, dym, &
                                 model_q, iv%pseudo(n)%zk )
#else
         call Interp_Obs_lin_2D( xb % u(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_u, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % v(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_v, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % t(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_t, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % p(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_p, iv%pseudo(n)%zk )
         call Interp_Obs_lin_2D( xb % q(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                                 model_q, iv%pseudo(n)%zk )
#endif
!------------------------------------------------------------------------
!        [3.0] Calculate observation O = B + (O-B):
!------------------------------------------------------------------------

         if ( pseudo_var(1:1) == 'u' ) then
            ob % pseudo(n) % u = model_u + iv % pseudo(n) % u % inv
         else if ( pseudo_var(1:1) == 'v' ) then
            ob % pseudo(n) % v = model_v + iv % pseudo(n) % v % inv
         else if ( pseudo_var(1:1) == 't' ) then
            ob % pseudo(n) % t = model_t + iv % pseudo(n) % t % inv
         else if ( pseudo_var(1:1) == 'p' ) then
            ob % pseudo(n) % p = model_p + iv % pseudo(n) % p % inv
         else if ( pseudo_var(1:1) == 'q' ) then
            ob % pseudo(n) % q = model_q + iv % pseudo(n) % q % inv
         end if 
         
       endif

      END DO
   ENDIF

END SUBROUTINE da_get_innov_vector_pseudo

