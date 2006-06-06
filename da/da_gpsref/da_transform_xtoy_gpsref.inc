SUBROUTINE da_transform_xtoy_gpsref ( xa, xb, iv, xp, y )

!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (x_type),  INTENT(IN)   :: xa       ! gridded analysis increment.
   TYPE (xb_type),  INTENT(IN)  :: xb       ! first guess state.
   TYPE (ob_type), INTENT(IN)   :: iv       ! Innovation vector (O-B).
   TYPE (xpose_type), INTENT(IN):: xp       ! Domain decomposition vars.
   TYPE (y_type), INTENT(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  !
   REAL                         :: dy, dym  !
   INTEGER                      :: num_levs ! obs vertical levels

   IF ( iv%num_gpsref < 1 ) return

   do n=iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, iv%ob_numb(iv%current_ob_time)%gpsref

      if(iv%gpsref(n)%loc%proc_domain_with_halo) then

!xyh         y%gpsref(n)%ref(:) = 0.0
         num_levs = iv % gpsref(n) % info % levels

         i = iv%gpsref(n)%loc%i
         j = iv%gpsref(n)%loc%j
         dx = iv%gpsref(n)%loc%dx
         dy = iv%gpsref(n)%loc%dy
         dxm = iv%gpsref(n)%loc%dxm
         dym = iv%gpsref(n)%loc%dym
      
#ifndef DEREF_KLUDGE
         call da_interp_lin_3d(xa%ref, xp, i, j, dx, dy, dxm, dym, &
                            y%gpsref(n)%ref, num_levs, iv%gpsref(n)%zk, num_levs)
#else
         call da_interp_lin_3d(xa%ref(xp%ims,xp%jms,xp%kms), xp, i, j, dx, dy, dxm, dym, &
                            y%gpsref(n)%ref(1), num_levs, iv%gpsref(n)%zk(1), num_levs)
#endif

      endif

   ENDDO

END SUBROUTINE da_transform_xtoy_gpsref

